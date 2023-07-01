library(terra)
library(sf)
library(tidyverse)
library(geodata)


terraOptions(datatype = "INT4U")

select_tiles <- function(country, year) {
  countries <- world(path = getwd())
  glad_tiles <- st_make_grid(what = "polygons") %>% st_as_sf()
  glad_tiles$bbox <- lapply(glad_tiles$x, "st_bbox")
  glad_tiles <- vect(glad_tiles)
  glad_tiles$EWmin <- ifelse(str_detect(glad_tiles$bbox, "xmin = -"), 'W', "E")
  glad_tiles$NSmin <- ifelse(str_detect(glad_tiles$bbox, "ymin = -"), 'S', "N")
  glad_tiles$EWmax <- ifelse(str_detect(glad_tiles$bbox, "xmax = -"), 'W', "E")
  glad_tiles$NSmax <- ifelse(str_detect(glad_tiles$bbox, "ymax = -"), 'S', "N")
  glad_tiles$xmax <- str_pad(str_extract(str_extract(glad_tiles$bbox, 'xmax = -*\\d+'), '\\d+'), 3, "left", "0")
  glad_tiles$xmin <- str_pad(str_extract(str_extract(glad_tiles$bbox, 'xmin = -*\\d+'), '\\d+'), 3, "left", "0")
  glad_tiles$ymax <- str_pad(str_extract(str_extract(glad_tiles$bbox, 'ymax = -*\\d+'), '\\d+'), 2, "left", "0")
  glad_tiles$ymin <- str_pad(str_extract(str_extract(glad_tiles$bbox, 'ymin = -*\\d+'), '\\d+'), 2, "left", "0")
  glad_tiles$id <- paste0(
    glad_tiles$xmin, glad_tiles$EWmin, '_',
    glad_tiles$ymin, glad_tiles$NSmin, '_',
    glad_tiles$xmax, glad_tiles$EWmax, '_',
    glad_tiles$ymax, glad_tiles$NSmax
  )
  countries <- countries[unique(relate(countries, glad_tiles, "intersects", pairs = TRUE)[, 1])]
  if (country %in% countries$NAME_0 & year > 2020) {
    aoi <- countries[countries$NAME_0 == country, ]
  } else if (country %in% countries$GID_0 & year > 2020) {
    aoi <- countries[countries$GID_0 == country, ]
  } else {
    return(print("No data for selected country or year"))
  }
  tiles_country <- glad_tiles$id[relate(glad_tiles, aoi, "intersects", pairs = TRUE)[, 1]]
  return(list(tiles_country, aoi))
}

load_n_mask <- function(selected_tile, aoi, year, dsn = NA) {
  file_path_date <- paste0(
    ifelse(is.na(dsn), 
           "https://storage.googleapis.com/earthenginepartners-hansen/GLADalert/C2/current/alertDate", paste0(dsn,"/alertDate")),
    substr(year, 3, 4), "_", selected_tile, ".tif"
  )
  file_path_alert <- paste0(
    ifelse(is.na(dsn), "https://storage.googleapis.com/earthenginepartners-hansen/GLADalert/C2/current/alert", paste0(dsn,"/alert")),
    substr(year, 3, 4), "_", selected_tile, ".tif"
  )
  print(paste0(" - ", ifelse(is.na(dsn), "Downloading", "Reading from disk"), " and masking tile at ", format(Sys.time(), "%b %d %X")))
  raster_glad <- mask(rast(file_path_date), aoi) %>% mask(., rast(file_path_alert), maskvalues = 2)
  return(raster_glad)
}

## Steps 5:6 - Clump and Sieve {#sec-ClumpAndSieve}------------

clump_n_sieve <- function(masked_glad, pixels = 16) {
  if (is.na(minmax(masked_glad)[2])) {
    return(print("No clumps for current tile"))
  } else {
    clumped <- patches(masked_glad, directions = 8, zeroAsNA = TRUE)
    f_clumped <- freq(clumped)
  }
  if (any(f_clumped$count >= pixels)) {
    excludeID <- f_clumped$value[which(f_clumped$count < pixels)]
    clumped[clumped %in% excludeID] <- NA
    f_clumped <- filter(f_clumped, count >= pixels)
    return(list(clumped, f_clumped))
  } else {
    return(print("No clumps for pixel's threshold"))
  }
}

## Step 7 - Get start and last dates {#sec-GetStartAndLastDates}---------

get_first_last_date <- function(glad_raster, deforestation, clumped, year) {
  first_last_dates <- data.frame(
    first = zonal(glad_raster, deforestation, "min", na.rm = TRUE),
    last = zonal(glad_raster, deforestation, "max", na.rm = TRUE),
    tile = NA,
    clump_id = NA
  )
  first_last_dates$tile <- glad_raster@ptr[["names"]]
  names(first_last_dates) <- c("first", "last", "tile", "clump_id")
  first_last_dates$clump_id <- clumped[[2]]$value
  r_cells <- terra::extract(glad_raster, deforestation, cells = TRUE, na.rm = TRUE, ID = FALSE, exact = TRUE) %>% filter(fraction > 0.5)
  c_cells <- terra::extract(clumped[[1]], deforestation, cells = TRUE, na.rm = TRUE, ID = FALSE, exact = TRUE) %>% filter(fraction > 0.5)
  cr_cells <- full_join(r_cells, c_cells, by = "cell")
  names(cr_cells) <- c("date", "cell", "drop1", "patches", "drop2")
  first_last_dates <- left_join(first_last_dates, cr_cells, join_by(clump_id == patches)) %>%
    group_by(clump_id) %>%
    summarise(first = first(first), last = first(last), tile = first(tile), id = first(cell)) %>%
    ungroup()
  first_last_dates <- mutate(first_last_dates,
    first = as.character(as.Date(first, origin = paste0(year, "-01-01"))),
    last = as.character(as.Date(last, origin = paste0(year, "-01-01"))),
    id = as.character(id)
  )
  return(first_last_dates[, c(5, 2:4)])
}

## Step 8 - Create Gald GRID ----------------
create_grid <- function() {
  xvertline <- rep(seq(-110, 170, 10), 1, each = 2)
  yvertline <- rep(c(40, -50), 29)
  object <- rep(1:29, each = 2)
  vertmatrix <- cbind(object, xvertline, yvertline)
  verts <- vect(vertmatrix, "lines", crs = "epsg:4326")
  xhoriline <- rep(c(-130, 180), 6)
  yhoriline <- rep(seq(-30, 20, 10), 1, each = 2)
  object <- rep(1:6, each = 2)
  horimatrix <- cbind(object, xhoriline, yhoriline)
  horis <- vect(horimatrix, "lines", crs = "epsg:4326")
  gladlinegrid <- rbind(horis, verts)
  gladlinegrid$id <- NA # Create id for each line grid
  gladlinegrid$max <- NA
  for (i in 1:length(gladlinegrid)) {
    if (ext(gladlinegrid[i])$xmax == ext(gladlinegrid[i])$xmin) {
      if (sign(ext(gladlinegrid[i])$xmax) == -1) {
        gladlinegrid[i]$id <- str_pad(paste0(abs(ext(gladlinegrid[i])$xmax), "W"), 4, "left", "0")
        gladlinegrid[i]$max <- ext(gladlinegrid[i])$xmax
      } else {
        gladlinegrid[i]$id <- str_pad(paste0(abs(ext(gladlinegrid[i])$xmax), "E"), 4, "left", "0")
        gladlinegrid[i]$max <- ext(gladlinegrid[i])$xmax
      }
    } else {
      if (sign(ext(gladlinegrid[i])$ymax) == -1) {
        gladlinegrid[i]$id <- str_pad(paste0(abs(ext(gladlinegrid[i])$ymax), "S"), 3, "left", "0")
        gladlinegrid[i]$max <- ext(gladlinegrid[i])$ymax
      } else {
        gladlinegrid[i]$id <- str_pad(paste0(abs(ext(gladlinegrid[i])$ymax), "N"), 3, "left", "0")
        gladlinegrid[i]$max <- ext(gladlinegrid[i])$ymax
      }
    }
  }
  return(gladlinegrid)
}


## Step 9 - treat_tile_boundary -----------------------

recursive_dissolve <- function(bound_pol) {
  relations <- which(relate(bound_pol, relation = "touches"), arr.ind = TRUE)
  if (length(relations) == 0) {
    return(bound_pol)
  } else {
    j <- relations[which(relations[1, 1] == relations[, 1]), 2]
    agregado <- aggregate(bound_pol[c(relations[1, 1], j)])
    bound_pol <- erase(bound_pol, bound_pol[c(relations[1, 1], j)]) %>%
      rbind(agregado)
    recursive_dissolve(bound_pol)
  }
}



## Step 10 - Main call and attribute creation -------------

isdo <- function(country, year, pixels = 16, dsn = NA) {
  start_processing <- Sys.time()
  print(paste0("Selecting tiles at ", format(Sys.time(), "%b %d %X")))
  tiles <- select_tiles(country, year)
  if (is.character(tiles)) {
    return(tiles)
  }
  length_tiles <- length(tiles[[1]])
  dates <- as.list(NA)
  deforestation <- as.list(NA)
  clumped_sieved <- as.list(NA)
  for (i in 1:length_tiles) {
    print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Load and mask tile at ", format(Sys.time(), "%b %d %X")))
    masked_glad <- load_n_mask(tiles[[1]][i], tiles[[2]], year, dsn)
    print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Clump and sieve tile at ", format(Sys.time(), "%b %d %X")))
    clumped_sieved <- clump_n_sieve(masked_glad, pixels)
    if (is.character(clumped_sieved)) {
      next
    } else {
      print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Create deforestation geometry at ", format(Sys.time(), "%b %d %X")))
      deforestation[[i]] <- as.polygons(clumped_sieved[[1]])
      print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Colect dates at ", format(Sys.time(), "%b %d %X")))
      dates[[i]] <- get_first_last_date(masked_glad, deforestation[[i]], clumped_sieved, year)
    }
  }
  print(paste0("Gather results at ", format(Sys.time(), "%b %d %X")))
  dates <- dates[!is.na(dates)]
  deforestation <- deforestation[!is.na(deforestation)]
  dates <- dates[!sapply(dates, is.null)]
  deforestation <- deforestation[!sapply(deforestation, is.null)]
  dates <- do.call("rbind", dates)
  deforestation <- do.call("rbind", deforestation)
  values(deforestation) <- dates
  print(paste0("PROCESS deforestation on tile boundry at ", format(Sys.time(), "%b %d %X")))
  gladlinegrid <- create_grid()
  treat_at_boundries <- deforestation[relate(deforestation, gladlinegrid, "touches", pairs = TRUE)[, 1]]
  if (length(treat_at_boundries) > 0) {
    treated_at_boundries <- recursive_dissolve(treat_at_boundries)
    treated_at_boundries <- st_join(st_as_sf(treated_at_boundries), st_as_sf(treat_at_boundries)) %>%
      reframe(
        .by = geometry,
        id = list(id.y),
        tile = list(tile.y),
        first = min(first.y),
        last = max(last.y),
      )
    deforestation <- erase(deforestation, treat_at_boundries)
    treated <- vect(treated_at_boundries$geometry)
    values(treated) <- treated_at_boundries[2:5]
    deforestation <- rbind(deforestation, treated)
  }
  deforestation$persist_days <- as.numeric(trunc(difftime(deforestation$last, deforestation$first, units = "day")) + 1)
  deforestation$age_weeks <- as.numeric(round(difftime(Sys.Date(), deforestation$last, units = "week")))
  deforestation$current_area <- expanse(deforestation, unit = "ha")
  deforestation <- fillHoles(deforestation)
  deforestation$intended_area <- expanse(deforestation, unit = "ha")
  deforestation$completeness_index <- round(as.numeric(deforestation$current_area) / deforestation$intended_area, 1)
  deforestation$isdo <- round(as.numeric(deforestation$current_area) / as.numeric(deforestation$persist_days), 2)
  if (country == "BRA" | country == "Brazil") { # Only for BRAZIL to get statistics by-------
    download.file("https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2020/Brasil/BR/BR_UF_2020.zip", "uf.zip")
    unzip("uf.zip")
    uf <- vect("BR_UF_2020.shp")
    attrib <- terra::extract(uf, deforestation)
    attrib <- cbind(values(deforestation), attrib$SIGLA_UF)
    values(deforestation) <- attrib
  }
  print(paste("Processing finished successfully after ", format(round(difftime(Sys.time(), start_processing)), 1)))
  return(deforestation)
}
