# 0_2_prepare_route_buffer_1km.R
# Create 1-km buffers around the route lines.
# Reproject the buffers to match the CRS of the NLCD product.
# ---------------------------------------------------------------

library(here)
library(sf)
library(tidyverse)

here::i_am("code/0_2_prepare_route_buffer_1km.R")
source("code/functions/pre_processing.R")

## Settings ####
product <- "LndCov"
version <- 1
year <- 2024 

## Input NLCD raster ####
input_file_path <- input_nlcd_path(product, year, version)
input_file_name <- paste0(product, year, "V", version)

if (!file.exists(input_file_path)) {
  stop(paste("Skipping –", input_file_name, "not available"))
}

nlcd <- rast(input_file_path)


## Read matched route lines ####
lines <- st_read(here::here("output", "result_routes", "result_routes.shp"))

# Join StateNum and Route from the original points CSV via pt_id
pts_df <- read_csv(here::here("data", "Routes_2025Release.csv"))
pts_df <- pts_df %>% mutate(pt_id = row_number())
lines <- lines %>%
  left_join(pts_df %>% 
              # filter(CountryNum == 840) %>%
              # filter(StateNum != 3) %>%
              select(pt_id, StateNum, Route), 
            by = "pt_id")

# Project to meter-based CRS for buffering
lines_proj <- st_transform(lines, 5070) # NAD83 / Conus Albers

# Pre-compute 1 km buffers (same geometry every year)
cat("Buffering", nrow(lines_proj), "lines by 1 km...\n")
buffer_1km <- st_buffer(lines_proj, dist = 1000)

# Transform to the raster crs
buffer_1km_proj <- st_transform(
  buffer_1km,
  crs = crs(nlcd)
)

st_write(buffer_1km_proj, 
         here::here("data", "buffer_1km", "buffer_1km_proj.shp"))

