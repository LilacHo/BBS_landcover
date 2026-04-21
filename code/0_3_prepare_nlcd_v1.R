# 0_3_prepare_nlcd_v1.R
# Output the frequency table of the NLCD land cover pixels within a 1 km buffer
# around each route line. Loops over years, one output file per route per year.
# ---------------------------------------------------------------


library(here)
library(terra)
library(sf)
library(tidyverse)

here::i_am("code/0_3_prepare_nlcd_v1.R")
source("code/functions/pre_processing.R")

## Read 1-km buffers around routes and exclude Alaska ####
buffer_1km_proj <- st_read(here::here("data", "buffer_1km", "buffer_1km_proj.shp"))
buffer_1km_proj <- buffer_1km_proj %>%
  filter(StateNum != 3)

## Settings ####
product <- "LndCov"
version <- 1
years   <- 2010:2024  


# NLCD land cover lookup
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


# ## single year for test ####
# year <- 2024
# 
# input_file_path <- input_nlcd_path(product, year, version)
# input_file_name <- paste0(product, year, "V", version)
# 
# if (!file.exists(input_file_path)) {
#   stop(paste("Skipping –", input_file_name, "not available"))
# }
# 
# nlcd <- rast(input_file_path)
# levels(nlcd) <- lut
# 
# # Output directory for per-route rds files
# dir.create(here::here("output", "routes_1km", year),
#            showWarnings = FALSE, recursive = TRUE)
# 
# results <- vector("list", nrow(buffer_1km_proj))
# 
# for (i in seq_len(nrow(buffer_1km_proj))) {
#   if (i %% 100 == 0) cat("  Processing line", i, "of", nrow(buffer_1km_proj), "\n")
#   
#   # Transform buffer to raster CRS
#   buf_raster_crs <- st_transform(buffer_1km_proj[i, ], crs = crs(nlcd))
#   # buf_raster_crs <- st_transform(buffer_1km_proj[1, ], crs = crs(nlcd))
#   buf_vect <- vect(buf_raster_crs)
#   
#   # Crop and mask raster to buffer (skip if outside raster extent)
#   freq_table <- tryCatch({
#     lc_cropped <- crop(nlcd, buf_vect)
#     lc_masked  <- mask(lc_cropped, buf_vect)
#     freq(lc_masked)
#   }, error = function(e) {
#     cat("  Skipped", buffer_1km_proj$StateNum[i], "_", buffer_1km_proj$Route[i], ":", conditionMessage(e), "\n")
#     NULL
#   })
#   
#   # print(freq_table)
# 
#   freq_table2 <- freq_table %>%
#     rename(class = value) %>%
#     left_join(lut, by = "class") %>%
#     select(layer, value, class, count)
# 
#   # freq_table2
#   
#   # Save full frequency table per route
#   rds_name <- paste0(input_file_name, "_", buffer_1km_proj$StateNum[i], "_", buffer_1km_proj$Route[i], ".rds")
#   saveRDS(freq_table2, here::here("output", "routes_1km", year, rds_name))
# }



## Loop over years ####
for (year in years) {

  cat("\n========== Year:", year, "==========\n")

  input_file_path <- input_nlcd_path(product, year, version)
  input_file_name <- paste0(product, year, "V", version)

  if (!file.exists(input_file_path)) {
    cat("  Skipping –", input_file_name, "not available\n")
    next
  }

  nlcd <- rast(input_file_path)
  levels(nlcd) <- lut

  # Output directory for per-route rds files
  dir.create(here::here("output", "routes_1km", year),
             showWarnings = FALSE, recursive = TRUE)

  results <- vector("list", nrow(buffer_1km_proj))

  for (i in seq_len(nrow(buffer_1km_proj))) {
    if (i %% 100 == 0) cat("  Processing line", i, "of", nrow(buffer_1km_proj), "\n")
    
    # Transform buffer to raster CRS
    buf_raster_crs <- st_transform(buffer_1km_proj[i, ], crs = crs(nlcd))
    # buf_raster_crs <- st_transform(buffer_1km_proj[1, ], crs = crs(nlcd))
    buf_vect <- vect(buf_raster_crs)
    
    # Crop and mask raster to buffer (skip if outside raster extent)
    freq_table <- tryCatch({
      lc_cropped <- crop(nlcd, buf_vect)
      lc_masked  <- mask(lc_cropped, buf_vect)
      freq(lc_masked)
    }, error = function(e) {
      cat("  Skipped", buffer_1km_proj$StateNum[i], "_", buffer_1km_proj$Route[i], ":", conditionMessage(e), "\n")
      NULL
    })
    
    freq_table2 <- freq_table %>%
      rename(class = value) %>%
      left_join(lut, by = "class") %>%
      select(layer, value, class, count)
    
    
    # Save full frequency table per route
    rds_name <- paste0(input_file_name, "_", buffer_1km_proj$StateNum[i], "_", buffer_1km_proj$Route[i], ".rds")
    saveRDS(freq_table2, here::here("output", "routes_1km", year, rds_name))
  
  }
  cat("  Year", year, "done.", "\n")
}

cat("\nAll years complete.\n")
