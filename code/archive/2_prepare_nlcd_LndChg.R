# ===============================================================
# 2_prepare_nlcd_LndChg.R
# ---------------------------------------------------------------
# PURPOSE
#   For each route buffer and each year, tabulate the frequency of
#   NLCD land-cover classes among the raster pixels falling inside
#   the 1-km buffer. Writes one .rds frequency table per route/year.
#
# INPUT
#   data/buffer_1km/buffer_1km_proj.shp   (1-km buffers from step 1, in
#       EPSG:5070; must contain StateNum and Route columns)
#   Annual NLCD rasters (.tif) for each year, located via
#       input_nlcd_path(product, year, version).
#   Version 2 denotes the Annual NLCD Conterminous U.S. (CU) Collection
#       Version 2 (1.2).
#
# OUTPUT
#   output/routes_1km/<year>/<product><year>V<version>_<StateNum>_<Route>.rds
#       Each .rds holds a data.frame with columns:
#         layer, value (NLCD code), class (label), count (pixel count)
#
# SETTINGS
#   product / version / years control which rasters are processed.
# ---------------------------------------------------------------


library(here)
library(terra)
library(sf)
library(tidyverse)

here::i_am("code/archive/2_prepare_nlcd_LndChg.R")
source("code/functions/pre_processing.R")

## Read 1-km buffers around routes ####
# Buffers are in EPSG:5070; each is reprojected to the raster CRS below.
buffer_1km_proj <- st_read(here::here("data", "buffer_1km", "buffer_1km_proj.shp"))

## Settings ####
product <- "LndChg"
version <- 2
years   <- 2010:2025


# NLCD land cover lookup
lut <- data.frame(
  value = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95,
            1112, 1121, 1122, 1123, 1124, 1131, 1141, 1142, 1143, 1152, 1171, 1181, 1182, 1190, 1195,
            1211, 1221, 1222, 1223, 1224, 1231, 1241, 1242, 1243, 1252, 1271, 1281, 1282, 1290, 1295,
            2111, 2112, 2122, 2123, 2124, 2131, 2141, 2142, 2143, 2152, 2171, 2181, 2182, 2190, 2195,
            2211, 2212, 2221, 2223, 2224, 2231, 2241, 2242, 2243, 2252, 2271, 2281, 2282, 2290, 2295,
            2311, 2312, 2321, 2322, 2324, 2331, 2341, 2342, 2343, 2352, 2371, 2381, 2382, 2390, 2395,
            2411, 2412, 2421, 2422, 2423, 2431, 2441, 2442, 2443, 2452, 2471, 2481, 2482, 2490, 2495,
            3111, 3112, 3121, 3122, 3123, 3124, 3141, 3142, 3143, 3152, 3171, 3181, 3182, 3190, 3195,
            4111, 4112, 4121, 4122, 4123, 4124, 4131, 4142, 4143, 4152, 4171, 4181, 4182, 4190, 4195,
            4211, 4212, 4221, 4222, 4223, 4224, 4231, 4241, 4243, 4252, 4271, 4281, 4282, 4290, 4295,
            4311, 4312, 4321, 4322, 4323, 4324, 4331, 4341, 4342, 4352, 4371, 4381, 4382, 4390, 4395,
            5211, 5212, 5221, 5222, 5223, 5224, 5231, 5241, 5242, 5243, 5271, 5281, 5282, 5290, 5295,
            7111, 7112, 7121, 7122, 7123, 7124, 7131, 7141, 7142, 7143, 7152, 7181, 7182, 7190, 7195,
            8111, 8112, 8121, 8122, 8123, 8124, 8131, 8141, 8142, 8143, 8152, 8171, 8182, 8190, 8195,
            8211, 8212, 8221, 8222, 8223, 8224, 8231, 8241, 8242, 8243, 8252, 8271, 8281, 8290, 8295,
            9011, 9012, 9021, 9022, 9023, 9024, 9031, 9041, 9042, 9043, 9052, 9071, 9081, 9082, 9095,
            9511, 9512, 9521, 9522, 9523, 9524, 9531, 9541, 9542, 9543, 9552, 9571, 9581, 9582, 9590),
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
    "Emergent Herbaceous Wetlands",
    # ---- Change codes (AABB = previous class + current class) ----
    "Open Water to Perennial Ice/Snow", "Open Water to Developed, Open Space", "Open Water to Developed, Low Intensity", "Open Water to Developed, Medium Intensity", "Open Water to Developed, High Intensity", "Open Water to Barren Land", "Open Water to Deciduous Forest", "Open Water to Evergreen Forest", "Open Water to Mixed Forest", "Open Water to Shrub/Scrub", "Open Water to Grassland/Herbaceous", "Open Water to Pasture/Hay", "Open Water to Cultivated Crops", "Open Water to Woody Wetlands", "Open Water to Emergent Herbaceous Wetlands",
    "Perennial Ice/Snow to Open Water", "Perennial Ice/Snow to Developed, Open Space", "Perennial Ice/Snow to Developed, Low Intensity", "Perennial Ice/Snow to Developed, Medium Intensity", "Perennial Ice/Snow to Developed, High Intensity", "Perennial Ice/Snow to Barren Land", "Perennial Ice/Snow to Deciduous Forest", "Perennial Ice/Snow to Evergreen Forest", "Perennial Ice/Snow to Mixed Forest", "Perennial Ice/Snow to Shrub/Scrub", "Perennial Ice/Snow to Grassland/Herbaceous", "Perennial Ice/Snow to Pasture/Hay", "Perennial Ice/Snow to Cultivated Crops", "Perennial Ice/Snow to Woody Wetlands", "Perennial Ice/Snow to Emergent Herbaceous Wetlands",
    "Developed, Open Space to Open Water", "Developed, Open Space to Perennial Ice/Snow", "Developed, Open Space to Developed, Low Intensity", "Developed, Open Space to Developed, Medium Intensity", "Developed, Open Space to Developed, High Intensity", "Developed, Open Space to Barren Land", "Developed, Open Space to Deciduous Forest", "Developed, Open Space to Evergreen Forest", "Developed, Open Space to Mixed Forest", "Developed, Open Space to Shrub/Scrub", "Developed, Open Space to Grassland/Herbaceous", "Developed, Open Space to Pasture/Hay", "Developed, Open Space to Cultivated Crops", "Developed, Open Space to Woody Wetlands", "Developed, Open Space to Emergent Herbaceous Wetlands",
    "Developed, Low Intensity to Open Water", "Developed, Low Intensity to Perennial Ice/Snow", "Developed, Low Intensity to Developed, Open Space", "Developed, Low Intensity to Developed, Medium Intensity", "Developed, Low Intensity to Developed, High Intensity", "Developed, Low Intensity to Barren Land", "Developed, Low Intensity to Deciduous Forest", "Developed, Low Intensity to Evergreen Forest", "Developed, Low Intensity to Mixed Forest", "Developed, Low Intensity to Shrub/Scrub", "Developed, Low Intensity to Grassland/Herbaceous", "Developed, Low Intensity to Pasture/Hay", "Developed, Low Intensity to Cultivated Crops", "Developed, Low Intensity to Woody Wetlands", "Developed, Low Intensity to Emergent Herbaceous Wetlands",
    "Developed, Medium Intensity to Open Water", "Developed, Medium Intensity to Perennial Ice/Snow", "Developed, Medium Intensity to Developed, Open Space", "Developed, Medium Intensity to Developed, Low Intensity", "Developed, Medium Intensity to Developed, High Intensity", "Developed, Medium Intensity to Barren Land", "Developed, Medium Intensity to Deciduous Forest", "Developed, Medium Intensity to Evergreen Forest", "Developed, Medium Intensity to Mixed Forest", "Developed, Medium Intensity to Shrub/Scrub", "Developed, Medium Intensity to Grassland/Herbaceous", "Developed, Medium Intensity to Pasture/Hay", "Developed, Medium Intensity to Cultivated Crops", "Developed, Medium Intensity to Woody Wetlands", "Developed, Medium Intensity to Emergent Herbaceous Wetlands",
    "Developed, High Intensity to Open Water", "Developed, High Intensity to Perennial Ice/Snow", "Developed, High Intensity to Developed, Open Space", "Developed, High Intensity to Developed, Low Intensity", "Developed, High Intensity to Developed, Medium Intensity", "Developed, High Intensity to Barren Land", "Developed, High Intensity to Deciduous Forest", "Developed, High Intensity to Evergreen Forest", "Developed, High Intensity to Mixed Forest", "Developed, High Intensity to Shrub/Scrub", "Developed, High Intensity to Grassland/Herbaceous", "Developed, High Intensity to Pasture/Hay", "Developed, High Intensity to Cultivated Crops", "Developed, High Intensity to Woody Wetlands", "Developed, High Intensity to Emergent Herbaceous Wetlands",
    "Barren Land to Open Water", "Barren Land to Perennial Ice/Snow", "Barren Land to Developed, Open Space", "Barren Land to Developed, Low Intensity", "Barren Land to Developed, Medium Intensity", "Barren Land to Developed, High Intensity", "Barren Land to Deciduous Forest", "Barren Land to Evergreen Forest", "Barren Land to Mixed Forest", "Barren Land to Shrub/Scrub", "Barren Land to Grassland/Herbaceous", "Barren Land to Pasture/Hay", "Barren Land to Cultivated Crops", "Barren Land to Woody Wetlands", "Barren Land to Emergent Herbaceous Wetlands",
    "Deciduous Forest to Open Water", "Deciduous Forest to Perennial Ice/Snow", "Deciduous Forest to Developed, Open Space", "Deciduous Forest to Developed, Low Intensity", "Deciduous Forest to Developed, Medium Intensity", "Deciduous Forest to Developed, High Intensity", "Deciduous Forest to Barren Land", "Deciduous Forest to Evergreen Forest", "Deciduous Forest to Mixed Forest", "Deciduous Forest to Shrub/Scrub", "Deciduous Forest to Grassland/Herbaceous", "Deciduous Forest to Pasture/Hay", "Deciduous Forest to Cultivated Crops", "Deciduous Forest to Woody Wetlands", "Deciduous Forest to Emergent Herbaceous Wetlands",
    "Evergreen Forest to Open Water", "Evergreen Forest to Perennial Ice/Snow", "Evergreen Forest to Developed, Open Space", "Evergreen Forest to Developed, Low Intensity", "Evergreen Forest to Developed, Medium Intensity", "Evergreen Forest to Developed, High Intensity", "Evergreen Forest to Barren Land", "Evergreen Forest to Deciduous Forest", "Evergreen Forest to Mixed Forest", "Evergreen Forest to Shrub/Scrub", "Evergreen Forest to Grassland/Herbaceous", "Evergreen Forest to Pasture/Hay", "Evergreen Forest to Cultivated Crops", "Evergreen Forest to Woody Wetlands", "Evergreen Forest to Emergent Herbaceous Wetlands",
    "Mixed Forest to Open Water", "Mixed Forest to Perennial Ice/Snow", "Mixed Forest to Developed, Open Space", "Mixed Forest to Developed, Low Intensity", "Mixed Forest to Developed, Medium Intensity", "Mixed Forest to Developed, High Intensity", "Mixed Forest to Barren Land", "Mixed Forest to Deciduous Forest", "Mixed Forest to Evergreen Forest", "Mixed Forest to Shrub/Scrub", "Mixed Forest to Grassland/Herbaceous", "Mixed Forest to Pasture/Hay", "Mixed Forest to Cultivated Crops", "Mixed Forest to Woody Wetlands", "Mixed Forest to Emergent Herbaceous Wetlands",
    "Shrub/Scrub to Open Water", "Shrub/Scrub to Perennial Ice/Snow", "Shrub/Scrub to Developed, Open Space", "Shrub/Scrub to Developed, Low Intensity", "Shrub/Scrub to Developed, Medium Intensity", "Shrub/Scrub to Developed, High Intensity", "Shrub/Scrub to Barren Land", "Shrub/Scrub to Deciduous Forest", "Shrub/Scrub to Evergreen Forest", "Shrub/Scrub to Mixed Forest", "Shrub/Scrub to Grassland/Herbaceous", "Shrub/Scrub to Pasture/Hay", "Shrub/Scrub to Cultivated Crops", "Shrub/Scrub to Woody Wetlands", "Shrub/Scrub to Emergent Herbaceous Wetlands",
    "Grassland/Herbaceous to Open Water", "Grassland/Herbaceous to Perennial Ice/Snow", "Grassland/Herbaceous to Developed, Open Space", "Grassland/Herbaceous to Developed, Low Intensity", "Grassland/Herbaceous to Developed, Medium Intensity", "Grassland/Herbaceous to Developed, High Intensity", "Grassland/Herbaceous to Barren Land", "Grassland/Herbaceous to Deciduous Forest", "Grassland/Herbaceous to Evergreen Forest", "Grassland/Herbaceous to Mixed Forest", "Grassland/Herbaceous to Shrub/Scrub", "Grassland/Herbaceous to Pasture/Hay", "Grassland/Herbaceous to Cultivated Crops", "Grassland/Herbaceous to Woody Wetlands", "Grassland/Herbaceous to Emergent Herbaceous Wetlands",
    "Pasture/Hay to Open Water", "Pasture/Hay to Perennial Ice/Snow", "Pasture/Hay to Developed, Open Space", "Pasture/Hay to Developed, Low Intensity", "Pasture/Hay to Developed, Medium Intensity", "Pasture/Hay to Developed, High Intensity", "Pasture/Hay to Barren Land", "Pasture/Hay to Deciduous Forest", "Pasture/Hay to Evergreen Forest", "Pasture/Hay to Mixed Forest", "Pasture/Hay to Shrub/Scrub", "Pasture/Hay to Grassland/Herbaceous", "Pasture/Hay to Cultivated Crops", "Pasture/Hay to Woody Wetlands", "Pasture/Hay to Emergent Herbaceous Wetlands",
    "Cultivated Crops to Open Water", "Cultivated Crops to Perennial Ice/Snow", "Cultivated Crops to Developed, Open Space", "Cultivated Crops to Developed, Low Intensity", "Cultivated Crops to Developed, Medium Intensity", "Cultivated Crops to Developed, High Intensity", "Cultivated Crops to Barren Land", "Cultivated Crops to Deciduous Forest", "Cultivated Crops to Evergreen Forest", "Cultivated Crops to Mixed Forest", "Cultivated Crops to Shrub/Scrub", "Cultivated Crops to Grassland/Herbaceous", "Cultivated Crops to Pasture/Hay", "Cultivated Crops to Woody Wetlands", "Cultivated Crops to Emergent Herbaceous Wetlands",
    "Woody Wetlands to Open Water", "Woody Wetlands to Perennial Ice/Snow", "Woody Wetlands to Developed, Open Space", "Woody Wetlands to Developed, Low Intensity", "Woody Wetlands to Developed, Medium Intensity", "Woody Wetlands to Developed, High Intensity", "Woody Wetlands to Barren Land", "Woody Wetlands to Deciduous Forest", "Woody Wetlands to Evergreen Forest", "Woody Wetlands to Mixed Forest", "Woody Wetlands to Shrub/Scrub", "Woody Wetlands to Grassland/Herbaceous", "Woody Wetlands to Pasture/Hay", "Woody Wetlands to Cultivated Crops", "Woody Wetlands to Emergent Herbaceous Wetlands",
    "Emergent Herbaceous Wetlands to Open Water", "Emergent Herbaceous Wetlands to Perennial Ice/Snow", "Emergent Herbaceous Wetlands to Developed, Open Space", "Emergent Herbaceous Wetlands to Developed, Low Intensity", "Emergent Herbaceous Wetlands to Developed, Medium Intensity", "Emergent Herbaceous Wetlands to Developed, High Intensity", "Emergent Herbaceous Wetlands to Barren Land", "Emergent Herbaceous Wetlands to Deciduous Forest", "Emergent Herbaceous Wetlands to Evergreen Forest", "Emergent Herbaceous Wetlands to Mixed Forest", "Emergent Herbaceous Wetlands to Shrub/Scrub", "Emergent Herbaceous Wetlands to Grassland/Herbaceous", "Emergent Herbaceous Wetlands to Pasture/Hay", "Emergent Herbaceous Wetlands to Cultivated Crops", "Emergent Herbaceous Wetlands to Woody Wetlands"
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
      cat("  Skipped ", as.integer(buffer_1km_proj$StateNum[i]), "-",
          as.integer(buffer_1km_proj$Route[i]), ": ", conditionMessage(e), "\n", sep = "")
      NULL
    })

    # Skip saving when the buffer fell outside the raster (freq_table is NULL)
    if (is.null(freq_table)) next

    freq_table2 <- freq_table %>%
      rename(class = value) %>%
      left_join(lut, by = "class") %>%
      select(layer, value, class, count)


    # Save full frequency table per route.
    # Coerce StateNum/Route to integers so leading zeros (e.g. "02", "001")
    # are stripped, keeping filenames consistent with 4_calculate_nlcd.R.
    state_num <- as.integer(buffer_1km_proj$StateNum[i])
    route_num <- as.integer(buffer_1km_proj$Route[i])
    rds_name <- paste0(input_file_name, "_", state_num, "_", route_num, ".rds")
    saveRDS(freq_table2, here::here("output", "routes_1km", year, rds_name))

  }
  cat("  Year", year, "done.", "\n")
}

cat("\nAll years complete.\n")
