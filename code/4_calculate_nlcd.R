# ===============================================================
# 4_calculate_nlcd.R
# ---------------------------------------------------------------
# PURPOSE
#   Compute, for each route and year, the proportion of pixels within
#   the 1-km buffer that belong to a chosen land-cover category. Writes
#   one CSV per year, then combines them into a single CSV.
#
# INPUT
#   data/Routes_2025Release.csv   (route list; filtered to USA
#       CountryNum == 840 and excluding Alaska StateNum == 3)
#   output/routes_1km/<year>/<product><year>V<version>_<StateNum>_<Route>.rds
#       (per-route frequency tables produced by 3_prepare_nlcd.R)
#
# OUTPUT
#   output/<target_name>.csv  - all years row-bound into one file;
#       route rows with a <target_name> column = target_pixels / total_pixels
#
# CONFIGURE THE TARGET CATEGORY
#   Set `target_name` to one of the labels defined in `target_index`
#   below. The matching NLCD pixel values are looked up automatically.
#   To add a new category, add a row to `target_index`.
# ---------------------------------------------------------------

library(here)
library(tidyverse)

here::i_am("code/4_calculate_nlcd.R")

## Settings ####
product <- "LndCov"
version <- 1
years   <- 2011:2024

# ---- TARGET CATEGORY INDEX (label -> NLCD pixel values) ----
target_index <- tibble::tribble(
  ~target_name,  ~target_values,
  "developed",   c(21, 22, 23, 24),
  "aridlands",    c(31, 52),
  "grasslands",   c(71),
  "forest",      c(41, 42, 43),
  "cropland",    c(81, 82)
)

# ---- Pick the category by name; values are assigned automatically ----
target_name <- "aridlands"

if (!target_name %in% target_index$target_name) {
  stop("Unknown target_name '", target_name, "'. Available: ",
       paste(target_index$target_name, collapse = ", "))
}
target_values <- target_index$target_values[[which(target_index$target_name == target_name)]]


# ## test RDS ####
# test_rds <- readRDS(here::here("output","routes_1km", "2024", "LndCov2024V1_14_30.rds"))

## Routes ####
Routes <- read.csv(here::here("data","Routes_2025Release.csv"), header = TRUE)

Routes <- Routes %>%
  filter(CountryNum == 840) %>%
  filter(StateNum != 3)

# # Add year
# year <- 2011 # Edit year

## Loop over years ####
yearly_results <- vector("list", length(years))

for (j in seq_along(years)) {

  year <- years[j]
  cat("\n========== Year:", year, "==========\n")

  route_expanded <- Routes %>%
    crossing(year = year) %>%
    mutate(!!target_name := NA_real_)
  
  for (i in 1:nrow(route_expanded)){
    
    product_file_name <- paste0(product, route_expanded$year[i], "V", version)
    # Coerce StateNum/Route to integers so leading zeros are stripped,
    # matching the filenames written by 3_prepare_nlcd.R.
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
    
    # Calculate total count for the selected classes
    target_pixels <- sum(product_file$count[product_file$value %in% target_values])
    
    # Calculate percentage
    target_proportion <- target_pixels / total_pixels
    
    route_expanded[[target_name]][i] <- target_proportion
  }

  yearly_results[[j]] <- route_expanded
}

## Combine and write single output ####
combined_data <- bind_rows(yearly_results)

output_combinedfile_path <- here::here("output", paste0(target_name, ".csv"))
write.csv(combined_data, output_combinedfile_path, row.names = FALSE)
