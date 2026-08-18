# ===============================================================
# 3b_calculate_nlcd_LndCov.R
# ---------------------------------------------------------------
# PURPOSE
#   Mimics 3_calculate_nlcd.R, but instead of grouping pixel values
#   into a combined category (e.g. "developed" = 21+22+23+24), this
#   computes the proportion of EACH INDIVIDUAL LndCov class on its
#   own, for every route and year, and writes one CSV per class.
#
# INPUT
#   data/Routes_2026Release.csv   (route list; filtered to USA
#       CountryNum == 840 and excluding Alaska StateNum == 3)
#   output/routes_1km/<year>/<product><year>V<version>_<StateNum>_<Route>.rds
#       (per-route frequency tables produced by 2_prepare_nlcd.R,
#       run with product <- "LndCov")
#
# OUTPUT
#   output/LndCov_<value>_<category name>.csv  - one file per individual
#       LndCov class, all years row-bound; route rows with a <category name>
#       column = class_pixels / total_pixels
#
# NOTE
#   Each per-route .rds file is only read ONCE per route/year (all 16
#   individual class proportions are computed in the same pass), then
#   the combined results are split into per-class CSVs at the end.
# ---------------------------------------------------------------

library(here)
library(tidyverse)

here::i_am("code/3b_calculate_nlcd_LndCov.R")

## Settings ####
product <- "LndCov"
version <- 2
years   <- 2010:2025

# ---- INDIVIDUAL LndCov CLASS INDEX (NLCD value -> output category name) ----
lndcov_index <- tibble::tribble(
  ~value, ~category_name,
  11,     "OpenWater",
  12,     "PerennialIceSnow",
  21,     "DevelopedOpenSpace",
  22,     "DevelopedLowIntensity",
  23,     "DevelopedMediumIntensity",
  24,     "DevelopedHighIntensity",
  31,     "BarrenLand",
  41,     "DeciduousForest",
  42,     "EvergreenForest",
  43,     "MixedForest",
  52,     "ShrubScrub",
  71,     "GrasslandHerbaceous",
  81,     "PastureHay",
  82,     "CultivatedCrops",
  90,     "WoodyWetlands",
  95,     "EmergentHerbaceousWetlands"
)

## Routes ####
Routes <- read.csv(here::here("data","Routes_2026Release.csv"), header = TRUE)

Routes <- Routes %>%
  filter(CountryNum == 840) %>%
  filter(StateNum != 3)

## Loop over years ####
yearly_results <- vector("list", length(years))

for (j in seq_along(years)) {

  year <- years[j]
  cat("\n========== Year:", year, "==========\n")

  route_expanded <- Routes %>%
    crossing(year = year)

  # Add one NA numeric column per individual class up front
  for (category_name in lndcov_index$category_name) {
    route_expanded[[category_name]] <- NA_real_
  }

  for (i in 1:nrow(route_expanded)){

    product_file_name <- paste0(product, route_expanded$year[i], "V", version)
    # Coerce StateNum/Route to integers so leading zeros are stripped,
    # matching the filenames written by 2_prepare_nlcd.R.
    state_num <- as.integer(route_expanded$StateNum[i])
    route_num <- as.integer(route_expanded$Route[i])
    product_file_fullname <- paste0(product_file_name, "_", state_num, "_", route_num, ".rds")
    product_file_path <- here::here("output","routes_1km", route_expanded$year[i], product_file_fullname)

    # Skip if file doesn't exist
    if (!file.exists(product_file_path)) {
      next
    }

    # Read file only if it exists
    product_file <- readRDS(product_file_path)

    # Calculate total pixel count
    total_pixels <- sum(product_file$count)

    # Calculate the proportion of every individual class in one pass
    for (k in seq_len(nrow(lndcov_index))) {
      class_value    <- lndcov_index$value[k]
      category_name  <- lndcov_index$category_name[k]

      class_pixels <- sum(product_file$count[product_file$value == class_value])
      route_expanded[[category_name]][i] <- class_pixels / total_pixels
    }
  }

  yearly_results[[j]] <- route_expanded
}

## Combine all years ####
combined_data <- bind_rows(yearly_results)

## Write one CSV per individual LndCov class ####
id_cols <- setdiff(names(combined_data), lndcov_index$category_name)

for (k in seq_len(nrow(lndcov_index))) {
  class_value   <- lndcov_index$value[k]
  category_name <- lndcov_index$category_name[k]

  out_data <- combined_data %>%
    select(all_of(id_cols), all_of(category_name))

  output_file_path <- here::here("output", paste0("LndCov_", class_value, "_", category_name, ".csv"))
  write.csv(out_data, output_file_path, row.names = FALSE)
  cat("Wrote", output_file_path, "\n")
}
