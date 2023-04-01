library(raster)
library(sf)

countries <- st_read("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson")
glad_tiles <- st_read("https://raw.githubusercontent.com/georgeportoferreira/ubcdown/main/GLAD_GRID.geojson")
tile_ids <- c("000E_00N_010E_10N", "000E_10N_010E_20N", "000E_10S_010E_00N", "000E_20N_010E_30N", "010E_00N_020E_10N", "010E_10N_020E_20N", "010E_10S_020E_00N", "010E_20N_020E_30N", "010E_20S_020E_10S", "010E_30S_020E_20S", "010W_00N_000E_10N", "010W_10N_000E_20N", "010W_20N_000E_30N", "020E_00N_030E_10N", "020E_10N_030E_20N", "020E_10S_030E_00N", "020E_20N_030E_30N", "020E_20S_030E_10S", "020E_30S_030E_20S", "020W_00N_010W_10N", "020W_10N_010W_20N", "020W_20N_010W_30N", "030E_00N_040E_10N", "030E_10N_040E_20N", "030E_10S_040E_00N", "030E_20N_040E_30N", "030E_20S_040E_10S", "030E_30S_040E_20S", "030W_10N_020W_20N", "040E_00N_050E_10N", "040E_10N_050E_20N", "040E_10S_050E_00N", "040E_20N_050E_30N", "040E_20S_050E_10S", "040E_30S_050E_20S", "040W_10S_030W_00N", "040W_20S_030W_10S", "050E_00N_060E_10N", "050E_10N_060E_20N", "050E_10S_060E_00N", "050E_20N_060E_30N", "050E_20S_060E_10S", "050E_30S_060E_20S", "050W_00N_040W_10N", "050W_10S_040W_00N", "050W_20S_040W_10S", "050W_30S_040W_20S", "060E_20N_070E_30N", "060W_00N_050W_10N", "060W_10N_050W_20N", "060W_10S_050W_00N", "060W_20S_050W_10S", "060W_30S_050W_20S", "060W_40S_050W_30S", "070E_00N_080E_10N", "070E_10N_080E_20N", "070E_20N_080E_30N", "070W_00N_060W_10N", "070W_10N_060W_20N", "070W_10S_060W_00N", "070W_20S_060W_10S", "070W_30S_060W_20S", "080E_00N_090E_10N", "080E_10N_090E_20N", "080E_20N_090E_30N", "080W_00N_070W_10N", "080W_10N_070W_20N", "080W_10S_070W_00N", "080W_20N_070W_30N", "080W_20S_070W_10S", "080W_30S_070W_20S", "090E_00N_100E_10N", "090E_10N_100E_20N", "090E_10S_100E_00N", "090E_20N_100E_30N", "090W_00N_080W_10N", "090W_10N_080W_20N", "090W_10S_080W_00N", "090W_20N_080W_30N", "100E_00N_110E_10N", "100E_10N_110E_20N", "100E_10S_110E_00N", "100E_20N_110E_30N", "100W_10N_090W_20N", "100W_20N_090W_30N", "110E_00N_120E_10N", "110E_10N_120E_20N", "110E_10S_120E_00N", "110E_20N_120E_30N", "110E_20S_120E_10S", "110E_30S_120E_20S", "110W_10N_100W_20N", "110W_20N_100W_30N", "120E_00N_130E_10N", "120E_10N_130E_20N", "120E_10S_130E_00N", "120E_20N_130E_30N", "120E_20S_130E_10S", "120E_30S_130E_20S", "120W_20N_110W_30N", "130E_00N_140E_10N", "130E_10S_140E_00N", "130E_20N_140E_30N", "130E_20S_140E_10S", "130E_30S_140E_20S", "140E_10S_150E_00N", "140E_20S_150E_10S", "140E_30S_150E_20S", "150E_10S_160E_00N", "150E_20S_160E_10S", "150E_30S_160E_20S", "160E_10S_170E_00N", "160E_20S_170E_10S", "160E_30S_170E_20S", "170E_20S_180E_10S")
nm_countries <- c("Afghanistan", "Algeria", "Angola", "Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "Ashmore and Cartier Islands", "Australia", "Bahrain", "Bajo Nuevo Bank (Petrel Is.)", "Bangladesh", "Barbados", "Belize", "Benin", "Bhutan", "Bolivia", "Botswana", "Brazil", "British Virgin Islands", "Brunei", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Clipperton Island", "Colombia", "Comoros", "Coral Sea Islands", "Costa Rica", "Cuba", "Curaçao", "Democratic Republic of the Congo", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Ethiopia", "Federated States of Micronesia", "Fiji", "France", "French Southern and Antarctic Lands", "Gabon", "Gambia", "Ghana", "Grenada", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong S.A.R.", "India", "Indonesia", "Iran", "Iraq", "Israel", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kenya", "Kiribati", "Kuwait", "Laos", "Lesotho", "Liberia", "Libya", "Macao S.A.R", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Mauritania", "Mauritius", "Mexico", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "Nicaragua", "Niger", "Nigeria", "Norfolk Island", "North Korea", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Puerto Rico", "Qatar", "Republic of Congo", "Russia", "Rwanda", "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", "Sao Tome and Principe", "Saudi Arabia", "Scarborough Reef", "Senegal", "Serranilla Bank", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Solomon Islands", "Somalia", "Somaliland", "South Africa", "South Sudan", "Spain", "Spratly Islands", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Taiwan", "Thailand", "The Bahamas", "Togo", "Trinidad and Tobago", "Turks and Caicos Islands", "Uganda", "United Arab Emirates", "United Republic of Tanzania", "United States Minor Outlying Islands", "United States of America", "United States Virgin Islands", "Uruguay", "US Naval Base Guantanamo Bay", "Vanuatu", "Venezuela", "Vietnam", "Western Sahara", "Yemen", "Zambia", "Zimbabwe")
a3_countries <- c("ABW", "AFG", "AGO", "AIA", "ARE", "ARG", "ATF", "ATG", "AUS", "BDI", "BEN", "BFA", "BGD", "BHR", "BHS", "BLM", "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", "CAF", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV", "CRI", "CUB", "CUW", "CYM", "DJI", "DMA", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", "ETH", "FJI", "FRA", "FSM", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRD", "GTM", "GUY", "HKG", "HND", "HTI", "IDN", "IND", "IRN", "IRQ", "ISR", "JAM", "JOR", "JPN", "KEN", "KHM", "KIR", "KNA", "KWT", "LAO", "LBR", "LBY", "LCA", "LKA", "LSO", "MAC", "MAF", "MAR", "MDG", "MDV", "MEX", "MLI", "MMR", "MOZ", "MRT", "MSR", "MUS", "MWI", "MYS", "NAM", "NCL", "NER", "NFK", "NGA", "NIC", "NLD", "NPL", "NRU", "OMN", "PAK", "PAN", "PER", "PHL", "PLW", "PNG", "PRI", "PRK", "PRY", "QAT", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SLB", "SLE", "SLV", "SOM", "SSD", "STP", "SUR", "SWZ", "SXM", "SYC", "TCA", "TCD", "TGO", "THA", "TLS", "TTO", "TWN", "TZA", "UGA", "UMI", "URY", "USA", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", "YEM", "ZAF", "ZMB", "ZWE")


#Select Tiles

select_tiles <- function(country) {
  if (country %in% nm_countries) tiles_country <- st_intersects(glad_tiles, countries[countries$ADMIN == country,]$geometry, sparse = FALSE)
   else if (country %in% a3_countries) 
  tiles_country <- st_intersects(glad_tiles, countries[countries$ISO_A3 == country,]$geometry, sparse = FALSE)
   else return(print("No data for selected country"))
  return(glad_tiles[tiles_country,]$id)
}


# Download Tiles

download_tiles <- function (selected_tiles, year){
  list_of_raster_glad <- as.list(NULL)
    for (i in 1:length(selected_tiles)) {
    list_of_raster_glad[[i]] <- raster(paste0(
      'https://storage.googleapis.com/earthenginepartners-hansen/GLADalert/C2/current/alertDate',
      substr(year,3,4),'_',selected_tiles[i],'.tif'))
  }
  return(list_of_raster_glad)
}  


# Clump and sieve pixels
clump_n_sieve <- function(list_of_downloaded_tiles){
  list_of_clumped <- as.list(NULL)
  list_of_sieved <- as.list(NULL)
  list_of_f_sieved <- as.list(NULL)
  for (i in 1:length(list_of_downloaded_tiles)) {
    list_of_clumped[[i]] <- clump(list_of_downloaded_tiles[[i]])
    f_clumped_glad <- as.data.frame(
    freq(list_of_clumped[[i]], useNA = 'no', progress = 'text')
    )    
    sieved_glad <- list_of_clumped[[i]]
    excludeID <- f_clumped_glad$value[which(f_clumped_glad$count <= 16)] #16 pixels = 1.44 hectare
    sieved_glad[list_of_clumped[[i]] %in% excludeID] <- NA
    list_of_f_sieved[[i]] <- as.data.frame(
      freq(sieved_glad, useNA = 'no', progress = 'text')
    )
    list_of_sieved[[i]] <- sieved_glad
  return(list(list_of_sieved, list_of_f_sieved))
  }
}

# start and end dates
get_start_end_date <- function(glad_raster, sieved_glad, fsg){
  start_end_d <- as.list(NULL)
  for (i in 1:length(sieved_glad)) {
    sieved_matrix <- as.matrix(sieved_glad[[i]])
    glad_matrix <- as.matrix(glad_raster[[i]])
    start_end_dates <- data.frame(id=as.character(NA), start=as.POSIXct.Date(NA), end=as.POSIXct.Date(NA))#, speed=as.numeric(NA))
    for (i in 1:length(fsg[,1])) {
      start_end_dates[i,] <- c(fsg[i,1], 
                               strftime(as.Date(min(glad_matrix[which(sieved_matrix[]==fsg[i,1])]),origin="2022-01-01"), format = "%Y-%m-%d"), 
                               strftime(as.Date(max(glad_matrix[which(sieved_matrix[]==fsg[i,1])]),origin="2022-01-01"), format = "%Y-%m-%d")
      )
      print(paste(i, 'from',length(fsg[,1]),'at', Sys.time()))
    }
    start_end_d[[i]] <- start_end_dates
  }
  return(start_end_d)
}
