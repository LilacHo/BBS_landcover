library(here) #setup
library(terra) #raster data 
# library(bbsBayes2)
library(sf)
library(tidyverse)

here::i_am("code/5_SinglePoint_v1.R")
source("code/functions/pre_processing.R")

## input_file_path ####
product <- "LndCov"
year <- 2024
version <- 1
# 
# if (input_file_path != "") {
#   input_file_path <- ""
# }

# LndChg2023 <- rast(here("data","Annual_NLCD_LndChg_2023_CU_C1V1", "Annual_NLCD_LndChg_2023_CU_C1V1.tif"))
input_file_path <- input_file_path_server(product, year, version) #eg: ("LndChg", 2023, 0)
# input_file_path <- input_file_path_server(product, year, version)

# print(input_file_path)
if(file.exists(input_file_path)){
  input_file_name <- paste0(product, year, "V", version) # eg: LndChg2023V0
  # automatically assign the raster to a changeable variable name
  # assign(input_file_name, rast(input_file_path)) # equivalent to: LndChg2023V0 <- rast(input_file_path)
  nlcd <- rast(input_file_path)
} else{
  print(paste0(product, year,"V", version, " is not available"))
}

## Routes ####

Routes <- read.csv(here::here("data","Routes_2025Release.csv"), header = TRUE)

# Exclude routes outside of USA and Alaska
Routes_doable <- Routes %>%
  filter(CountryNum == 840) %>%
  filter(StateNum != 3)

# Add attributes
lut <- data.frame(
  value = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95),
  class = c(
    "Open Water",
    "Perennial Ice/Snow",
    "Developed, Open Space",
    "Developed, Low Intensity",
    "Developed, Medium Intensity",
    "Developed, High Intensity",
    "Barren Land",
    "Deciduous Forest",
    "Evergreen Forest",
    "Mixed Forest",
    "Shrub/Scrub",
    "Grassland/Herbaceous",
    "Pasture/Hay",
    "Cultivated Crops",
    "Woody Wetlands",
    "Emergent Herbaceous Wetlands"
  )
)

levels(nlcd) <- lut

# # Single route for test
# # get an exemplary route
# lat <- Routes$Latitude[1]
# lon <- Routes$Longitude[1]
# 
# # Create sf point object
# point_sf <- st_sf(
#   geometry = st_sfc(
#     st_point(c(lon, lat)),
#     crs = 4269 # for North America
#   )
# )
# 
# # Transform to a projected CRS for meters
# point_proj <- st_transform(point_sf, 32616) # cautious: high accuracy for a small region
# 
# # Buffer: 30 km = 30,000 meters
# buffer_30km <- st_buffer(point_proj, dist = 30000)
# 
# # Transform to the raster crs
# buffer_30km_proj <- st_transform(
#   buffer_30km,
#   crs = crs(nlcd)
# )
# 
# # Convert sf object to SpatVector for terra
# buffer_vect <- vect(buffer_30km_proj)
# 
# # # Crop raster to polygon extent (fast, rectangular)
# lc_cropped <- crop(nlcd, buffer_vect)
# 
# # Mask raster with polygon to get exact circular shape (slow but precise)
# lc_circular <- mask(lc_cropped, buffer_vect)
# 
# # plot(lc_cropped)
# # plot(lc_circular)
# 
# # unique(values(lc_circular))
# 
# freq_table <- freq(lc_circular)
# # print(freq_table)
# 
# freq_table2 <- freq_table %>%
#   rename(class = value) %>%
#   left_join(lut, by = "class") %>%
#   select(layer, value, class, count)
# 
# # freq_table2
# 
# ouput_file_name <- paste0(input_file_name,"_", Routes$CountryNum[1], "_", Routes$StateNum[1], "_", Routes$Route[1], ".rds")
# saveRDS(freq_table2, here::here("output","routes",year, ouput_file_name))


  


for (i in 1:nrow(Routes_doable)){
  print(i)


  # get an exemplary route
  lat <- Routes_doable$Latitude[i] 
  lon <- Routes_doable$Longitude[i]
  
  # Create sf point object
  point_sf <- st_sf(
    geometry = st_sfc(
      st_point(c(lon, lat)),
      crs = 4269 # for North America
    )
  )
  
  # Transform to a projected CRS for meters
  point_proj <- st_transform(point_sf, 32616) # cautious: high accuracy for a small region
  
  # Buffer: 30 km = 30,000 meters
  buffer_30km <- st_buffer(point_proj, dist = 30000)
  
  # Transform to the raster crs
  buffer_30km_proj <- st_transform(
    buffer_30km, 
    crs = crs(nlcd)
  )
  
  # Convert sf object to SpatVector for terra
  buffer_vect <- vect(buffer_30km_proj)
  
  # # Crop raster to polygon extent (fast, rectangular)
  lc_cropped <- crop(nlcd, buffer_vect)
  
  # Mask raster with polygon to get exact circular shape (slow but precise)
  lc_circular <- mask(lc_cropped, buffer_vect)
  
  # plot(lc_cropped)
  # plot(lc_circular)
  
  # unique(values(lc_circular))
  
  freq_table <- freq(lc_circular)
  # print(freq_table)
  
  freq_table2 <- freq_table %>%
    rename(class = value) %>%
    left_join(lut, by = "class") %>%
    select(layer, value, class, count)
  print(freq_table2)
  
  ouput_file_name <- paste0(input_file_name,"_", Routes_doable$StateNum[i], "_", Routes_doable$Route[i], ".rds") 
  saveRDS(freq_table, here::here("output","routes",year, ouput_file_name))
}
