# 0_4_calculate_nlcd.R
# Proportion of target land cover within 1 km of each route;
# Iterates over all years, producing individual yearly outputs 
# and a final combined dataset.
# ---------------------------------------------------------------
# To switch categories, edit the two settings below:
#   target_name   – a short label used in output file/column names
#   target_values – vector of NLCD pixel values to count
# ---------------------------------------------------------------

library(here)
library(tidyverse)

here::i_am("code/0_4_calculate_nlcd.R")

## Settings ####
product <- "LndCov"
version <- 1
years   <- 2011:2024

# ---- TARGET CATEGORY (edit here to switch) ----
# Examples:
#   Grassland:  target_name <- "grassland";  target_values <- c(71)
#   Developed:  target_name <- "developed";  target_values <- c(21, 22, 23, 24)
#   Forest:     target_name <- "forest";     target_values <- c(41, 42, 43)
#   Cropland:   target_name <- "cropland";   target_values <- c(81, 82)

target_name   <- "developed"
target_values <- c(21, 22, 23, 24)

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
for (year in years) {
  
  cat("\n========== Year:", year, "==========\n")
  
  route_expanded <- Routes %>%
    crossing(year = year) %>%
    mutate(!!target_name := NA_real_)
  
  for (i in 1:nrow(route_expanded)){
    
    product_file_name <- paste0(product, route_expanded$year[i], "V", version)
    product_file_fullname <- paste0(product_file_name, "_", route_expanded$StateNum[i], "_", route_expanded$Route[i], ".rds")
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
  
  output_file_fullname <- paste0(target_name, year, ".csv")
  output_file_path <- here::here("output", target_name, output_file_fullname)
  write.csv(route_expanded, output_file_path, row.names = FALSE)
}

## Combine####
files <- list.files(
  here::here("output", target_name),
  pattern = "\\.csv$",
  full.names = TRUE
)

combined_data <- files %>%
  lapply(read_csv) %>%
  bind_rows()

output_combinedfile_fullname <- paste0(target_name, ".csv")
output_combinedfile_path <- here::here("output", target_name, output_combinedfile_fullname)
write.csv(combined_data, output_combinedfile_path, row.names = FALSE)
