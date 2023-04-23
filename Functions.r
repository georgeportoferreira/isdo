library(raster)
library(sf)
library(dplyr)

# countries <- st_read("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson")
# glad_tiles <- st_read("https://raw.githubusercontent.com/georgeportoferreira/ubcdown/main/GLAD_GRID.geojson")
# nm_countries <- c("Afghanistan", "Algeria", "Angola", "Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "Ashmore and Cartier Islands", "Australia", "Bahrain", "Bajo Nuevo Bank (Petrel Is.)", "Bangladesh", "Barbados", "Belize", "Benin", "Bhutan", "Bolivia", "Botswana", "Brazil", "British Virgin Islands", "Brunei", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Clipperton Island", "Colombia", "Comoros", "Coral Sea Islands", "Costa Rica", "Cuba", "Curaçao", "Democratic Republic of the Congo", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Ethiopia", "Federated States of Micronesia", "Fiji", "France", "French Southern and Antarctic Lands", "Gabon", "Gambia", "Ghana", "Grenada", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong S.A.R.", "India", "Indonesia", "Iran", "Iraq", "Israel", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kenya", "Kiribati", "Kuwait", "Laos", "Lesotho", "Liberia", "Libya", "Macao S.A.R", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Mauritania", "Mauritius", "Mexico", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "Nicaragua", "Niger", "Nigeria", "Norfolk Island", "North Korea", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Puerto Rico", "Qatar", "Republic of Congo", "Russia", "Rwanda", "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", "Sao Tome and Principe", "Saudi Arabia", "Scarborough Reef", "Senegal", "Serranilla Bank", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Solomon Islands", "Somalia", "Somaliland", "South Africa", "South Sudan", "Spain", "Spratly Islands", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Taiwan", "Thailand", "The Bahamas", "Togo", "Trinidad and Tobago", "Turks and Caicos Islands", "Uganda", "United Arab Emirates", "United Republic of Tanzania", "United States Minor Outlying Islands", "United States of America", "United States Virgin Islands", "Uruguay", "US Naval Base Guantanamo Bay", "Vanuatu", "Venezuela", "Vietnam", "Western Sahara", "Yemen", "Zambia", "Zimbabwe")
# a3_countries <- c("ABW", "AFG", "AGO", "AIA", "ARE", "ARG", "ATF", "ATG", "AUS", "BDI", "BEN", "BFA", "BGD", "BHR", "BHS", "BLM", "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", "CAF", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV", "CRI", "CUB", "CUW", "CYM", "DJI", "DMA", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", "ETH", "FJI", "FRA", "FSM", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRD", "GTM", "GUY", "HKG", "HND", "HTI", "IDN", "IND", "IRN", "IRQ", "ISR", "JAM", "JOR", "JPN", "KEN", "KHM", "KIR", "KNA", "KWT", "LAO", "LBR", "LBY", "LCA", "LKA", "LSO", "MAC", "MAF", "MAR", "MDG", "MDV", "MEX", "MLI", "MMR", "MOZ", "MRT", "MSR", "MUS", "MWI", "MYS", "NAM", "NCL", "NER", "NFK", "NGA", "NIC", "NLD", "NPL", "NRU", "OMN", "PAK", "PAN", "PER", "PHL", "PLW", "PNG", "PRI", "PRK", "PRY", "QAT", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SLB", "SLE", "SLV", "SOM", "SSD", "STP", "SUR", "SWZ", "SXM", "SYC", "TCA", "TCD", "TGO", "THA", "TLS", "TTO", "TWN", "TZA", "UGA", "UMI", "URY", "USA", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", "YEM", "ZAF", "ZMB", "ZWE")


# Select Tiles -------------------
select_tiles <- function(country) {
  if (country %in% nm_countries) {
    tiles_country <- st_intersects(glad_tiles, countries[countries$ADMIN == country, ]$geometry, sparse = FALSE)
  } else if (country %in% a3_countries) {
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
  if (!is.na(dsn)) {
      for (i in 1:length(selected_tiles)) {
    list_of_raster_glad[[i]] <- mask(raster(paste0(
      dsn, "/alertDate", substr(year, 3, 4), "_", selected_tiles[i], ".tif"
    )), as_Spatial(aoi))
      } 
    }else {
  for (i in 1:length(selected_tiles)) {
    list_of_raster_glad[[i]] <- mask(raster(paste0(
      "https://storage.googleapis.com/earthenginepartners-hansen/GLADalert/C2/current/alertDate",
      substr(year, 3, 4), "_", selected_tiles[i], ".tif"
    )), as_Spatial(aoi))
  }
}
  #list_of_raster_glad[length(selected_tiles)+1] <- aoi
  return(list_of_raster_glad)
}


# Clump and sieve pixels ----------------
clump_n_sieve <- function(list_of_rasters, pixels = 16) {
  list_of_clumped <- as.list(NULL)
  list_of_sieved <- as.list(NULL)
  list_of_f_sieved <- as.list(NULL)
  for (i in 1:length(list_of_rasters)) {
    list_of_clumped[[i]] <- clump(list_of_rasters[[i]])
    f_clumped_glad <- as.data.frame(
      freq(list_of_clumped[[i]], useNA = "no", progress = "text")
    )
    sieved_glad <- list_of_clumped[[i]]
    excludeID <- f_clumped_glad$value[which(f_clumped_glad$count <= pixels)] # 16 pixels = 1.44 hectare
    sieved_glad[list_of_clumped[[i]] %in% excludeID] <- NA
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
  length_sieved_glad <- length(sieved_glad)
  for (i in 1:length_sieved_glad) {
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
      print(paste0("Tile ", i, "/", length_sieved_glad, " collecting ", j, " from ", nrow_sieved_glad, " dates at ", Sys.time()))
    }
    start_end_d[[i]] <- start_end_dates
  }
  return(start_end_d)
}


## CREATE POLYGONS ------------
create_geometry_deforestation <- function(sieved_glad) {
  list_of_deforestation <- as.list(NULL)
  length_sieved_glad <- length(sieved_glad)  
  for (j in 1:length_sieved_glad) {
    vetor <- st_sf(data.frame(id = as.numeric(NA), geometry = st_sfc(list(st_polygon()))))
    points <- rasterToPoints(sieved_glad[[1]][[j]], dissolve = TRUE)
    ids <- unique(points[,3])
    for (i in 1:length(ids)){
      ch <- points[which(points[,3] == ids[i]),1:2][,1:2]
      geometry <- st_polygon(list(ch[c(chull(ch), chull(ch)[1]),]))
      vetor[i,] <- c(ids[i], st_geometry(geometry))
    }
  list_of_deforestation[[j]] <- vetor
  }
  return(list_of_deforestation)
}


## ISDO ---------------------

isdo<- function(country, year, pixels = 16, dsn = NA) {
  tiles <- load_masked_glad(country, year,dsn)
  clumped_sieved <- clump_n_sieve(tiles, pixels) 
  dates <- get_start_end_date(tiles, clumped_sieved, year)
  rm(tiles)
  deforestation <- create_geometry_deforestation(clumped_sieved)
  return(cbind(do.call("rbind", dates), do.call("rbind", deforestation)))
}
