library(raster)
library(sf)
library(dplyr)

# countries <- st_read("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson")
# glad_tiles <- st_read("https://raw.githubusercontent.com/georgeportoferreira/ubcdown/main/GLAD_GRID.geojson")
# nm_countries <- c("Afghanistan", "Algeria", "Angola", "Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "Ashmore and Cartier Islands", "Australia", "Bahrain", "Bajo Nuevo Bank (Petrel Is.)", "Bangladesh", "Barbados", "Belize", "Benin", "Bhutan", "Bolivia", "Botswana", "Brazil", "British Virgin Islands", "Brunei", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Clipperton Island", "Colombia", "Comoros", "Coral Sea Islands", "Costa Rica", "Cuba", "Curaçao", "Democratic Republic of the Congo", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Ethiopia", "Federated States of Micronesia", "Fiji", "France", "French Southern and Antarctic Lands", "Gabon", "Gambia", "Ghana", "Grenada", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong S.A.R.", "India", "Indonesia", "Iran", "Iraq", "Israel", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kenya", "Kiribati", "Kuwait", "Laos", "Lesotho", "Liberia", "Libya", "Macao S.A.R", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Mauritania", "Mauritius", "Mexico", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "Nicaragua", "Niger", "Nigeria", "Norfolk Island", "North Korea", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Puerto Rico", "Qatar", "Republic of Congo", "Russia", "Rwanda", "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", "Sao Tome and Principe", "Saudi Arabia", "Scarborough Reef", "Senegal", "Serranilla Bank", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Solomon Islands", "Somalia", "Somaliland", "South Africa", "South Sudan", "Spain", "Spratly Islands", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Taiwan", "Thailand", "The Bahamas", "Togo", "Trinidad and Tobago", "Turks and Caicos Islands", "Uganda", "United Arab Emirates", "United Republic of Tanzania", "United States Minor Outlying Islands", "United States of America", "United States Virgin Islands", "Uruguay", "US Naval Base Guantanamo Bay", "Vanuatu", "Venezuela", "Vietnam", "Western Sahara", "Yemen", "Zambia", "Zimbabwe")
# a3_countries <- c("ABW", "AFG", "AGO", "AIA", "ARE", "ARG", "ATF", "ATG", "AUS", "BDI", "BEN", "BFA", "BGD", "BHR", "BHS", "BLM", "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", "CAF", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV", "CRI", "CUB", "CUW", "CYM", "DJI", "DMA", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", "ETH", "FJI", "FRA", "FSM", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRD", "GTM", "GUY", "HKG", "HND", "HTI", "IDN", "IND", "IRN", "IRQ", "ISR", "JAM", "JOR", "JPN", "KEN", "KHM", "KIR", "KNA", "KWT", "LAO", "LBR", "LBY", "LCA", "LKA", "LSO", "MAC", "MAF", "MAR", "MDG", "MDV", "MEX", "MLI", "MMR", "MOZ", "MRT", "MSR", "MUS", "MWI", "MYS", "NAM", "NCL", "NER", "NFK", "NGA", "NIC", "NLD", "NPL", "NRU", "OMN", "PAK", "PAN", "PER", "PHL", "PLW", "PNG", "PRI", "PRK", "PRY", "QAT", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SLB", "SLE", "SLV", "SOM", "SSD", "STP", "SUR", "SWZ", "SXM", "SYC", "TCA", "TCD", "TGO", "THA", "TLS", "TTO", "TWN", "TZA", "UGA", "UMI", "URY", "USA", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", "YEM", "ZAF", "ZMB", "ZWE")


# Select Tiles -------------------
select_tiles <- function(country) {
  countries <- st_read("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson")|>
    dplyr::select(ADMIN, ISO_A3, geometry)
  glad_tiles <- st_read("https://raw.githubusercontent.com/georgeportoferreira/ubcdown/main/GLAD_GRID.geojson")
    if (country %in% countries$ADMIN) {
    tiles_country <- st_intersects(glad_tiles, countries[countries$ADMIN == country, ]$geometry, sparse = FALSE)
  } else if (country %in% countries$ISO_A3) {
    tiles_country <- st_intersects(glad_tiles, countries[countries$ISO_A3 == country, ]$geometry, sparse = FALSE)
  } else {
    return(print("No data for selected country"))
  }
  return(glad_tiles[tiles_country, ]$id)
}


# Download Tiles ---------------------
download_tiles <- function(selected_tiles, year) {
  list_of_raster_glad <- as.list(NULL)
  for (i in 1:length(selected_tiles)) {
    list_of_raster_glad[[i]] <- raster(paste0(
      "https://storage.googleapis.com/earthenginepartners-hansen/GLADalert/C2/current/alertDate",
      substr(year, 3, 4), "_", selected_tiles[i], ".tif"
    ))
  }
  return(list_of_raster_glad)
}

# Read tiles from disk -------------------------
read_from_disk <- function(dsn, selected_tiles, year) {
  list_of_raster_glad <- as.list(NULL)
  for (i in 1:length(selected_tiles)) {
    list_of_raster_glad[[i]] <- raster(paste0(
      dsn, "/alertDate", substr(year, 3, 4), "_", selected_tiles[i], ".tif"
    ))
  }
  return(list_of_raster_glad)
}


# Load tiles and mask ---------------

load_masked_glad <- function(country, year, dsn = NA) {
  countries <- st_read("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson")|>
      dplyr::select(ADMIN, ISO_A3, geometry)
  glad_tiles <- st_read("https://raw.githubusercontent.com/georgeportoferreira/ubcdown/main/GLAD_GRID.geojson")
  countries <- countries[which(!is.na(lapply(
    st_intersects(st_make_valid(countries), glad_tiles, sparse = TRUE)
    , first))),]
  if (country %in% countries$ADMIN) {
      aoi <- countries[countries$ADMIN == country, ]$geometry
  } else if (country %in% countries$ISO_A3) {
      aoi <- countries[countries$ISO_A3 == country, ]$geometry
  } else {
      return(print("No data for selected country"))
  }
  tiles_country <- st_intersects(glad_tiles, aoi, sparse = FALSE)
  selected_tiles <- glad_tiles[tiles_country, ]$id
  list_of_raster_glad <- as.list(NULL)
  length_selected_tiles <- length(selected_tiles)
  if (!is.na(dsn)) {
      for (i in 1:length_selected_tiles) {
        print(paste0("Loading from disk tile ", i, "/", length_selected_tiles, " at ", Sys.time()))
        list_of_raster_glad[[i]] <- mask(raster(paste0(
      dsn, "/alertDate", substr(year, 3, 4), "_", selected_tiles[i], ".tif"
    )), as_Spatial(aoi))
      } 
    }else {
  for (i in 1:length_selected_tiles) {
    print(paste0("Downloading tile ", i, "/", length_selected_tiles, " at ", Sys.time()))
    list_of_raster_glad[[i]] <- mask(raster(paste0(
      "https://storage.googleapis.com/earthenginepartners-hansen/GLADalert/C2/current/alertDate",
      substr(year, 3, 4), "_", selected_tiles[i], ".tif"
    )), as_Spatial(aoi))
  }
}
  return(list_of_raster_glad)
}


# Clump and sieve pixels ----------------
clump_n_sieve <- function(list_of_rasters, pixels = 16) {
  list_of_clumped <- as.list(NULL)
  list_of_sieved <- as.list(NULL)
  list_of_f_sieved <- as.list(NULL)
  length_list_of_rasters <- length(list_of_rasters)
  for (i in 1:length_list_of_rasters) {
    print(paste0("Clumping tile ", i, "/", length_list_of_rasters, " at ", Sys.time()))
    list_of_clumped[[i]] <- clump(list_of_rasters[[i]], progress = "text")
    print(paste0("Stats for clumped ", i, "/", length_list_of_rasters, " at ", Sys.time()))
    f_clumped_glad <- as.data.frame(
      freq(list_of_clumped[[i]], useNA = "no", progress = "text")
    )
    sieved_glad <- list_of_clumped[[i]]
    print(paste0("Sieving tile ", i, "/", length_list_of_rasters, " at ", Sys.time()))
    excludeID <- f_clumped_glad$value[which(f_clumped_glad$count <= pixels)] # 16 pixels = 1.44 hectare
    sieved_glad[list_of_clumped[[i]] %in% excludeID] <- NA
    print(paste0("Stats for sieved ", i, "/", length_list_of_rasters, " at ", Sys.time()))
    list_of_f_sieved[[i]] <- as.data.frame(
      freq(sieved_glad, useNA = "no", progress = "text")
    )
    list_of_sieved[[i]] <- sieved_glad
  }
  return(list(list_of_sieved, list_of_f_sieved))
}

## start and end dates ---------------------
get_start_end_date <- function(glad_raster, sieved_glad, year) {
  start_end_d <- as.list(NULL)
  length_sieved_glad <- length(sieved_glad[[2]])
  for (i in 1:length_sieved_glad) {
    print(paste0("Processing Tile ", i, "/", length_sieved_glad, " for dates at ", Sys.time()))
    if (sieved_glad[[1]][[i]]@data@min == Inf) next
    sieved_matrix <- as.matrix(sieved_glad[[1]][[i]])
    glad_matrix <- as.matrix(glad_raster[[i]])
    start_end_dates <- data.frame(id = as.numeric(NA), start = as.POSIXct.Date(NA), end = as.POSIXct.Date(NA), countpixel = as.numeric(NA))
    nrow_sieved_glad <- nrow(sieved_glad[[2]][[i]])
    for (j in 1:nrow_sieved_glad) {
      start_end_dates[j, ] <- c(
        sieved_glad[[2]][[i]][j, 1],
        strftime(as.Date(min(glad_matrix[which(sieved_matrix[] == sieved_glad[[2]][[i]][j, 1])]),
          origin = paste0(year,"-01-01")
        ), format = "%Y-%m-%d"),
        strftime(as.Date(max(glad_matrix[which(sieved_matrix[] == sieved_glad[[2]][[i]][j, 1])]),
          origin = paste0(year,"-01-01")
        ), format = "%Y-%m-%d"),
        sieved_glad[[2]][[i]][j, 2]
      )
      print(paste0("Tile ", i, "/", length_sieved_glad, " collecting date", j, "/", nrow_sieved_glad, " at ", Sys.time()))
    }
    start_end_d[[i]] <- start_end_dates
  }
  return(start_end_d)
}


## CREATE POLYGONS ------------
create_geometry_deforestation <- function(sieved_glad) {
  list_of_deforestation <- as.list(NULL)
  length_sieved_glad <- length(sieved_glad[[1]])  
  for (j in 1:length_sieved_glad) {
    print(paste0("Processing Tile ", j, "/", length_sieved_glad, " for deforestation  at ", Sys.time()))
    if (sieved_glad[[1]][[j]]@data@min == Inf) next
    vetor <- st_sf(data.frame(id = as.numeric(NA), tile = as.character(NA), geometry = st_sfc(st_polygon()) ))
    print("Converting raster to points")
    points <- rasterToPoints(sieved_glad[[1]][[j]], progress = TRUE)
    ids <- unique(points[,3])
    tile <- as.character(extent(sieved_glad[[1]][[j]]))
    length_ids <- length(ids)
    print("Creating polygon geometries")
    for (i in 1:length_ids){
      print(paste0("Deforestation ", i, "/", length_ids, " from tile ",j,"/", length_sieved_glad, " at ", Sys.time()))
      ch <- points[which(points[,3] == ids[i]),1:2][,1:2]
      geometry <- st_polygon(list(ch[c(chull(ch), chull(ch)[1]),]))
      vetor[i,] <- c(ids[i], tile, st_geometry(geometry))
    }
  list_of_deforestation[[j]] <- vetor
  }
  return(list_of_deforestation)
}


## ISDO ---------------------

isdo<- function(country, year, pixels = 16, dsn = NA) {
  print(paste0("PROCESS 1/5 - Loading and masking tiles at ", Sys.time()))
  tiles <- load_masked_glad(country, year,dsn)
  print(paste0("PROCESS 2/5 - Clumping and sieving tiles at ", Sys.time()))
  clumped_sieved <- clump_n_sieve(tiles, pixels) 
  print(paste0("PROCESS 3/5 - Colecting dates at", Sys.time()))
  dates <- get_start_end_date(tiles, clumped_sieved, year)
  rm(tiles)
  print(paste0("PROCESS 4/5 - Creating deforestation geometry at", Sys.time()))
  deforestation <- create_geometry_deforestation(clumped_sieved)
  print(paste0("PROCESS 5/5 - Gathering results at", Sys.time()))
  isdo_result <- cbind(do.call("rbind", dates), do.call("rbind", deforestation))
  isdo_result <- isdo_result[-5] %>% 
    mutate(area_vector = st_area(.$geometry), 
           persistence = as.numeric(trunc(difftime(isdo_result$end, isdo_result$start, units = "day")) +1),
           age = as.numeric(trunc(difftime(Sys.Date(), isdo_result$end, units = "day"))),
           isdo = round(as.numeric(countpixel)/as.numeric(persistence), 2),
           completeness_index = round(as.numeric(countpixel)*900/area_vector,1)
           )  
  return(isdo_result[,c(1:5,7:11,6)])
}
