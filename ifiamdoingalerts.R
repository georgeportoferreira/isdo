if (!"terra" %in% installed.packages()) {
  install.packages("terra", repos = "https://cloud.r-project.org/")
}
if (!"sf" %in% installed.packages()) {
  install.packages("sf", repos = "https://cloud.r-project.org/")
}
if (!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org/")
}
if (!"geodata" %in% installed.packages()) {
  install.packages("geodata", repos = "https://cloud.r-project.org/")
}

library(terra)
library(sf)
library(tidyverse)
library(geodata)

terraOptions(datatype = "INT4U")

## Steps 1:2 - Select tiles and country AOI------------
select_tiles <- function(country) {
  countries <- world(path = tempdir())
  glad_tiles <- vect("https://opendata.arcgis.com/api/v3/datasets/bac1e89b8d1c4d57b0cf15764b07c26f_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
  countries <- countries[glad_tiles]
  if (country %in% countries$NAME_0) {
    aoi <- countries[countries$NAME_0 == country, ]
  } else if (country %in% countries$GID_0) {
    aoi <- countries[countries$GID_0 == country, ]
  } else {
    return(print("No data for selected country or year"))
  }
  tiles_country <- values(glad_tiles[relate(glad_tiles, aoi, "intersects", pairs = TRUE)[, 1]])[, 1:2]
  return(list(tiles_country, aoi))
}


## Steps 3:4 - Load and mask GLAD raster------------
load_n_mask <- function(selected_tile, aoi, dsn = NA) {
  file_path_date <- ifelse(is.na(dsn),
    selected_tile$download,
    paste0(dsn, "/", selected_tile$tile_id, ".tif")
  )
  print(paste0(" - ", ifelse(is.na(dsn), "Downloading", "Reading from disk"), " and masking tile at ", format(Sys.time(), "%b %d %X")))
  raster_glad <- rast(file_path_date)
  ifelse(is.na(minmax(raster_glad)[2]),
    return(print("Void Raster")),
    return(crop(raster_glad, aoi, mask = TRUE))
  )
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
    if(!is_empty(excludeID)){ #check if there is no small clumps to avoid error
      clumped[clumped %in% excludeID] <- NA
    }
    f_clumped <- filter(f_clumped, count >= pixels)
    return(list(clumped, f_clumped))
  } else {
    return(print("No clumps for pixel's threshold"))
  }
}

## Step 7 - Get start and last dates {#sec-GetStartAndLastDates}---------

get_first_last_date <- function(glad_raster, deforestation, clumped) {
  confidence_level <- glad_raster / 10000
  dates <- glad_raster - confidence_level * 10000
  first_last_dates <- data.frame(
    first = zonal(dates, deforestation, "min", na.rm = TRUE),
    last = zonal(dates, deforestation, "max", na.rm = TRUE),
    confidence = zonal(confidence_level, deforestation, "mean", na.rm = TRUE),
    tile = NA,
    clump_id = NA
  )
  first_last_dates$tile <- substr(glad_raster@ptr[["names"]], 16, 23)
  names(first_last_dates) <- c("first", "last", "confidence", "tile", "clump_id")
  first_last_dates$clump_id <- clumped[[2]]$value
  r_cells <- terra::extract(glad_raster, deforestation, cells = TRUE, na.rm = TRUE, ID = FALSE, exact = TRUE) %>% filter(fraction > 0.5)
  c_cells <- terra::extract(clumped[[1]], deforestation, cells = TRUE, na.rm = TRUE, ID = FALSE, exact = TRUE) %>% filter(fraction > 0.5)
  cr_cells <- full_join(r_cells, c_cells, by = "cell")
  names(cr_cells) <- c("date", "cell", "drop1", "patches", "drop2")
  first_last_dates <- left_join(first_last_dates, cr_cells, join_by(clump_id == patches)) %>%
    group_by(clump_id) %>%
    summarise(
      first = first(first), last = first(last),
      confidence = first(confidence), tile = first(tile), id = first(cell)
    ) %>%
    ungroup()
  first_last_dates <- mutate(first_last_dates,
    first = as.character(as.Date(first, origin = "2014-12-31")),
    last = as.character(as.Date(last, origin = "2014-12-31")),
    id = as.character(id)
  )
  return(first_last_dates[, c(6, 2:5)])
}

## Step 8 - Create Gald line GRID ----------------
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

isdo <- function(country, pixels = 16, dsn = NA) {
  start_processing <- Sys.time()
  print(paste0("Selecting tiles at ", format(Sys.time(), "%b %d %X")))
  tiles <- select_tiles(country)
  if (is.character(tiles)) {
    return(tiles)
  }
  length_tiles <- length(tiles[[1]]$tile_id)
  dates <- as.list(NA)
  deforestation <- as.list(NA)
  clumped_sieved <- as.list(NA)
  for (i in 1:length_tiles) {
    print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Load and mask tile at ", format(Sys.time(), "%b %d %X")))
    masked_glad <- load_n_mask(tiles[[1]][i, ], tiles[[2]], dsn)
    if (is.character(masked_glad)) {
      next
    } else {
      print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Clump and sieve tile at ", format(Sys.time(), "%b %d %X")))
      clumped_sieved <- clump_n_sieve(masked_glad, pixels)
      if (is.character(clumped_sieved)) {
        next
      } else {
        print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Create deforestation geometry at ", format(Sys.time(), "%b %d %X")))
        deforestation[[i]] <- as.polygons(clumped_sieved[[1]])
        print(paste0("PROCESS Tile ", i, "/", length_tiles, " - Colect dates at ", format(Sys.time(), "%b %d %X")))
        dates[[i]] <- get_first_last_date(masked_glad, deforestation[[i]], clumped_sieved)
      }
    }
  }
  print(paste0("Gather results at ", format(Sys.time(), "%b %d %X")))
  dates <- dates[!is.na(dates)]
  deforestation <- deforestation[!is.na(deforestation)]
  dates <- dates[!sapply(dates, is.null)]
  deforestation <- deforestation[!sapply(deforestation, is.null)]
  dates <- do.call("rbind", dates)
  deforestation <- do.call("rbind", deforestation)
  if (is.null(dates)) {
    return(print("No deforestation"))
  }
  values(deforestation) <- dates
  print(paste0("PROCESS deforestation on tile boundry at ", format(Sys.time(), "%b %d %X")))
  gladlinegrid <- create_grid()
  treat_at_boundries <- deforestation[relate(deforestation, gladlinegrid, "touches", pairs = TRUE)[, 1]]
  if (length(treat_at_boundries) > 1) {
    treated_at_boundries <- recursive_dissolve(treat_at_boundries)
    treated_at_boundries <- st_join(st_as_sf(treated_at_boundries), st_as_sf(treat_at_boundries))
    if (length(unique(treated_at_boundries$geometry)) == 1) {
      treated_at_boundries <- reframe(treated_at_boundries,
        .by = geometry,
        id = list(id),
        tile = list(tile),
        first = min(first),
        last = max(last),
        confidence = first(confidence)
      )
    } else {
      treated_at_boundries <- reframe(treated_at_boundries,
        .by = geometry,
        id = list(id.y),
        tile = list(tile.y),
        first = min(first.y),
        last = max(last.y),
        confidence = first(confidence.y)
      )
    }
    deforestation <- erase(deforestation, treat_at_boundries)
    treated <- vect(treated_at_boundries$geometry)
    values(treated) <- treated_at_boundries[2:6]
    deforestation <- rbind(deforestation, treated)
  }
  deforestation$persist_days <- as.numeric(trunc(difftime(deforestation$last, deforestation$first, units = "day")) + 1)
  deforestation$age_weeks <- as.numeric(round(difftime(Sys.Date(), deforestation$last, units = "week")))
  deforestation$current_area <- expanse(deforestation, unit = "ha")
  deforestation <- fillHoles(deforestation)
  deforestation$intended_area <- expanse(deforestation, unit = "ha")
  deforestation$completeness <- round(as.numeric(deforestation$current_area) / deforestation$intended_area, 1)
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

# Process all countries -------------------------

countries <- world(path = tempdir())
glad_tiles <- st_make_grid(what = "polygons") %>% st_as_sf()
glad_tiles$bbox <- lapply(glad_tiles$x, "st_bbox")
glad_tiles <- vect(glad_tiles)
glad_tiles$EWmin <- ifelse(str_detect(glad_tiles$bbox, "xmin = -"), "W", "E")
glad_tiles$NSmin <- ifelse(str_detect(glad_tiles$bbox, "ymin = -"), "S", "N")
glad_tiles$EWmax <- ifelse(str_detect(glad_tiles$bbox, "xmax = -"), "W", "E")
glad_tiles$NSmax <- ifelse(str_detect(glad_tiles$bbox, "ymax = -"), "S", "N")
glad_tiles$xmax <- str_pad(str_extract(str_extract(glad_tiles$bbox, "xmax = -*\\d+"), "\\d+"), 3, "left", "0")
glad_tiles$xmin <- str_pad(str_extract(str_extract(glad_tiles$bbox, "xmin = -*\\d+"), "\\d+"), 3, "left", "0")
glad_tiles$ymax <- str_pad(str_extract(str_extract(glad_tiles$bbox, "ymax = -*\\d+"), "\\d+"), 2, "left", "0")
glad_tiles$ymin <- str_pad(str_extract(str_extract(glad_tiles$bbox, "ymin = -*\\d+"), "\\d+"), 2, "left", "0")
glad_tiles$id <- paste0(
  glad_tiles$xmin, glad_tiles$EWmin, "_",
  glad_tiles$ymin, glad_tiles$NSmin, "_",
  glad_tiles$xmax, glad_tiles$EWmax, "_",
  glad_tiles$ymax, glad_tiles$NSmax
)
glad_tiles <- subset(glad_tiles, glad_tiles$id %in% c("000E_00N_010E_10N", "000E_10N_010E_20N", "000E_10S_010E_00N", "000E_20N_010E_30N", "010E_00N_020E_10N", "010E_10N_020E_20N", "010E_10S_020E_00N", "010E_20N_020E_30N", "010E_20S_020E_10S", "010E_30S_020E_20S", "010W_00N_000E_10N", "010W_10N_000E_20N", "010W_20N_000E_30N", "020E_00N_030E_10N", "020E_10N_030E_20N", "020E_10S_030E_00N", "020E_20N_030E_30N", "020E_20S_030E_10S", "020E_30S_030E_20S", "020W_00N_010W_10N", "020W_10N_010W_20N", "020W_20N_010W_30N", "030E_00N_040E_10N", "030E_10N_040E_20N", "030E_10S_040E_00N", "030E_20N_040E_30N", "030E_20S_040E_10S", "030E_30S_040E_20S", "030W_10N_020W_20N", "040E_00N_050E_10N", "040E_10N_050E_20N", "040E_10S_050E_00N", "040E_20N_050E_30N", "040E_20S_050E_10S", "040E_30S_050E_20S", "040W_10S_030W_00N", "040W_20S_030W_10S", "050E_00N_060E_10N", "050E_10N_060E_20N", "050E_10S_060E_00N", "050E_20N_060E_30N", "050E_20S_060E_10S", "050E_30S_060E_20S", "050W_00N_040W_10N", "050W_10S_040W_00N", "050W_20S_040W_10S", "050W_30S_040W_20S", "060E_20N_070E_30N", "060W_00N_050W_10N", "060W_10N_050W_20N", "060W_10S_050W_00N", "060W_20S_050W_10S", "060W_30S_050W_20S", "060W_40S_050W_30S", "070E_00N_080E_10N", "070E_10N_080E_20N", "070E_20N_080E_30N", "070W_00N_060W_10N", "070W_10N_060W_20N", "070W_10S_060W_00N", "070W_20S_060W_10S", "070W_30S_060W_20S", "080E_00N_090E_10N", "080E_10N_090E_20N", "080E_20N_090E_30N", "080W_00N_070W_10N", "080W_10N_070W_20N", "080W_10S_070W_00N", "080W_20N_070W_30N", "080W_20S_070W_10S", "080W_30S_070W_20S", "090E_00N_100E_10N", "090E_10N_100E_20N", "090E_10S_100E_00N", "090E_20N_100E_30N", "090W_00N_080W_10N", "090W_10N_080W_20N", "090W_10S_080W_00N", "090W_20N_080W_30N", "100E_00N_110E_10N", "100E_10N_110E_20N", "100E_10S_110E_00N", "100E_20N_110E_30N", "100W_10N_090W_20N", "100W_20N_090W_30N", "110E_00N_120E_10N", "110E_10N_120E_20N", "110E_10S_120E_00N", "110E_20N_120E_30N", "110E_20S_120E_10S", "110E_30S_120E_20S", "110W_10N_100W_20N", "110W_20N_100W_30N", "120E_00N_130E_10N", "120E_10N_130E_20N", "120E_10S_130E_00N", "120E_20N_130E_30N", "120E_20S_130E_10S", "120E_30S_130E_20S", "120W_20N_110W_30N", "130E_00N_140E_10N", "130E_10S_140E_00N", "130E_20N_140E_30N", "130E_20S_140E_10S", "130E_30S_140E_20S", "140E_10S_150E_00N", "140E_20S_150E_10S", "140E_30S_150E_20S", "150E_10S_160E_00N", "150E_20S_160E_10S", "150E_30S_160E_20S", "160E_10S_170E_00N", "160E_20S_170E_10S", "160E_30S_170E_20S", "170E_20S_180E_10S"))
countries <- countries[unique(relate(countries, glad_tiles, "intersects", pairs = TRUE)[, 1])]
countries <- countries$NAME_0

# Cedar
# for (country in countries) {
#   print(country)
#   desmatamento <- isdo(country, pixel = 16)
#   if (is.character(desmatamento)) {
#     next
#   } else {
#     writeVector(desmatamento, paste0("/home/gorg/GLAD/", country, ".geojson"), filetype = "GeoJSON")
#   }
# }


# Lenovo
for (country in countries) {
  print(country)
  desmatamento <- isdo(country, pixel = 16)
  if (is.character(desmatamento)) {
    next
  } else {
    writeVector(desmatamento, paste0("C:/Users/georg/Downloads/countries", country, ".geojson"), filetype = "GeoJSON")
  }
}
