# 0_prepare_landcover_shapefile.R
# Calculate the proportion of target land cover pixels within a 1 km buffer
# around each route line. Loops over years, one output file per year.
# ---------------------------------------------------------------
# To switch categories, edit the two settings below:
#   target_name   – a short label used in output file/column names
#   target_values – vector of NLCD pixel values to count
# ---------------------------------------------------------------

library(here)
library(terra)
library(sf)
library(tidyverse)

here::i_am("code/0_2_nlcd_line_1km_v1.R")
source("code/functions/pre_processing.R")

## Settings ####
product <- "LndCov"
version <- 1
years   <- 2010:2024  

# ---- TARGET CATEGORY (edit here to switch) ----
# Examples:
#   Grassland:  target_name <- "grassland";  target_values <- c(71)
#   Developed:  target_name <- "developed";  target_values <- c(21, 22, 23, 24)
#   Forest:     target_name <- "forest";     target_values <- c(41, 42, 43)
#   Cropland:   target_name <- "cropland";   target_values <- c(81, 82)
target_name   <- "grassland"
target_values <- c(71)

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

## Read matched route lines ####
lines <- st_read(here::here("output", "result_routes", "result_routes.shp"))
lines <- lines %>% mutate(line_id = row_number())

# Join StateNum and Route from the original points CSV via pt_id
pts_df <- read_csv(here::here("data", "Routes_2025Release.csv"))
pts_df <- pts_df %>% mutate(pt_id = row_number())
lines <- lines %>%
  left_join(pts_df %>% select(pt_id, StateNum, Route), by = "pt_id")

# Project to metre-based CRS for buffering
lines_proj <- st_transform(lines, 3857)

# Pre-compute 1 km buffers (same geometry every year)
cat("Buffering", nrow(lines_proj), "lines by 1 km...\n")
buffers <- st_buffer(lines_proj, dist = 1000)



## single point ####
year <- 2024

input_file_path <- input_nlcd_path(product, year, version)
input_file_name <- paste0(product, year, "V", version)

if (!file.exists(input_file_path)) {
  stop(paste("Skipping –", input_file_name, "not available"))
}

nlcd <- rast(input_file_path)
levels(nlcd) <- lut

# # Output directory for per-route rds files
# dir.create(here::here("output", "routes_1km", year),
#            showWarnings = FALSE, recursive = TRUE)

results <- vector("list", nrow(lines_proj))

for (i in seq_len(nrow(lines_proj))) {
  if (i %% 100 == 0) cat("  Processing line", i, "of", nrow(lines_proj), "\n")
  
  # Transform buffer to raster CRS
  buf_raster_crs <- st_transform(buffers[i, ], crs = crs(nlcd))
  buf_vect <- vect(buf_raster_crs)
  
  # Crop and mask raster to buffer (skip if outside raster extent)
  freq_table <- tryCatch({
    lc_cropped <- crop(nlcd, buf_vect)
    lc_masked  <- mask(lc_cropped, buf_vect)
    freq(lc_masked)
  }, error = function(e) {
    cat("  Skipped", lines_proj$StateNum[i], "_", lines_proj$Route[i], ":", conditionMessage(e), "\n")
    NULL
  })
  
  if (is.null(freq_table)) {
    results[[i]] <- tibble(
      line_id      = lines_proj$line_id[i],
      RTENAME      = lines_proj$RTENAME[i],
      total_pixels = 0L,
      !!paste0(target_name, "_pixels") := 0L,
      !!paste0(target_name, "_prop")   := NA_real_
    )
    next
  }
  
  # Calculate target category proportion
  total_pixels  <- sum(freq_table$count)
  target_pixels <- sum(freq_table$count[freq_table$value %in% target_values])
  target_prop   <- if (total_pixels > 0) target_pixels / total_pixels else NA_real_
  
  results[[i]] <- tibble(
    line_id      = lines_proj$line_id[i],
    RTENAME      = lines_proj$RTENAME[i],
    total_pixels = total_pixels,
    !!paste0(target_name, "_pixels") := target_pixels,
    !!paste0(target_name, "_prop")   := round(target_prop, 6)
  )
  
  # Save full frequency table per route
  rds_name <- paste0(input_file_name, "_", lines_proj$StateNum[i], "_", lines_proj$Route[i], ".rds")
  saveRDS(freq_table, here::here("output", "routes_1km", year, rds_name))
}

# Write one summary CSV per year
results_df <- bind_rows(results)
output_csv <- here::here("output",
                         paste0(target_name, "_proportion_1km_", input_file_name, ".csv"))
write.csv(results_df, output_csv, row.names = FALSE)

prop_col <- paste0(target_name, "_prop")
cat("  Year", year, "done.", nrow(results_df), "lines.",
    "Mean", target_name, "proportion:",
    round(mean(results_df[[prop_col]], na.rm = TRUE), 4), "\n")




## Loop over years ####
for (year in years) {

  cat("\n========== Year:", year, "==========\n")

  input_file_path <- input_nlcd_path_server(product, year, version)
  input_file_name <- paste0(product, year, "V", version)

  if (!file.exists(input_file_path)) {
    cat("  Skipping –", input_file_name, "not available\n")
    next
  }

  nlcd <- rast(input_file_path)
  levels(nlcd) <- lut

  # Output directory for per-route rds files
  dir.create(here::here("output", "routes_shapefile", year),
             showWarnings = FALSE, recursive = TRUE)

  results <- vector("list", nrow(lines_proj))

  for (i in seq_len(nrow(lines_proj))) {
    if (i %% 100 == 0) cat("  Processing line", i, "of", nrow(lines_proj), "\n")

    # Transform buffer to raster CRS
    buf_raster_crs <- st_transform(buffers[i, ], crs = crs(nlcd))
    buf_vect <- vect(buf_raster_crs)

    # Crop and mask raster to buffer (skip if outside raster extent)
    freq_table <- tryCatch({
      lc_cropped <- crop(nlcd, buf_vect)
      lc_masked  <- mask(lc_cropped, buf_vect)
      freq(lc_masked)
    }, error = function(e) {
      cat("  Skipped", lines_proj$StateNum[i], "_", lines_proj$Route[i], ":", conditionMessage(e), "\n")
      NULL
    })

    if (is.null(freq_table)) {
      results[[i]] <- tibble(
        line_id      = lines_proj$line_id[i],
        RTENAME      = lines_proj$RTENAME[i],
        total_pixels = 0L,
        !!paste0(target_name, "_pixels") := 0L,
        !!paste0(target_name, "_prop")   := NA_real_
      )
      next
    }

    # Calculate target category proportion
    total_pixels  <- sum(freq_table$count)
    target_pixels <- sum(freq_table$count[freq_table$value %in% target_values])
    target_prop   <- if (total_pixels > 0) target_pixels / total_pixels else NA_real_

    results[[i]] <- tibble(
      line_id      = lines_proj$line_id[i],
      RTENAME      = lines_proj$RTENAME[i],
      total_pixels = total_pixels,
      !!paste0(target_name, "_pixels") := target_pixels,
      !!paste0(target_name, "_prop")   := round(target_prop, 6)
    )

    # Save full frequency table per route
    rds_name <- paste0(input_file_name, "_", lines_proj$StateNum[i], "_", lines_proj$Route[i], ".rds")
    saveRDS(freq_table, here::here("output", "routes_shapefile", year, rds_name))
  }

  # Write one summary CSV per year
  results_df <- bind_rows(results)
  output_csv <- here::here("output",
                            paste0(target_name, "_proportion_1km_", input_file_name, ".csv"))
  write.csv(results_df, output_csv, row.names = FALSE)

  prop_col <- paste0(target_name, "_prop")
  cat("  Year", year, "done.", nrow(results_df), "lines.",
      "Mean", target_name, "proportion:",
      round(mean(results_df[[prop_col]], na.rm = TRUE), 4), "\n")
}

cat("\nAll years complete.\n")
