library(here) #setup
library(tidyverse)

here::i_am("code/0_prepare_landcover2.R")

## Routes ####
Routes <- read.csv(here::here("data","Routes_2025Release.csv"), header = TRUE)

Routes <- Routes %>%
  filter(CountryNum == 840)



##Version0#####
product <- "LndCov"
version <- 0

# Add year
years_consider <- 2010 #### change year
# years_consider <- firstYear:lastYear   

# target class
target_Classes <- "developed" #### change class


route_expanded <- Routes %>%
  crossing(year = years_consider) %>%
  mutate(developed = NA)
# route_expanded$developed <- NA

for (i in 1:nrow(route_expanded)){

  product_file_name <- paste0(product, route_expanded$year[i], "V", version)
  product_file_fullname <- paste0(product_file_name, "_", route_expanded$StateNum[i], "_", route_expanded$Route[i], ".rds")
  product_file_path <- here::here("output","routes", route_expanded$year[i], product_file_fullname)
  
  # Skip if file doesn't exist
  if (!file.exists(product_file_path)) {
    next
  }
  
  # Read file only if it exists
  product_file <- readRDS(product_file_path)
  
  
  # Define the classes you're interested in
  target_classes <- c(21, 22, 23, 24) #### change class value
  
  # Calculate total pixel count
  total_pixels <- sum(product_file$count)
  
  # Calculate total count for the selected classes
  target_pixels <- sum(product_file$count[product_file$value %in% target_classes])
  
  # Calculate percentage
  target_proportion <- target_pixels / total_pixels
  
  route_expanded$developed[i] <- target_proportion
}

output_file_fullname <- paste0(target_Classes, years_consider, ".csv")
output_file_path <- here::here("output", target_Classes, output_file_fullname)
write.csv(route_expanded, output_file_path, row.names = FALSE)






##Version1#####
product <- "LndCov"
version <- 1

# Add year
years_consider <- 2024
# years_consider <- firstYear:lastYear   

# target class
target_Classes <- "developed"


route_expanded <- Routes %>%
  crossing(year = years_consider) %>%
  mutate(developed = NA)
# route_expanded$developed <- NA

for (i in 1:nrow(route_expanded)){
  
  product_file_name <- paste0(product, route_expanded$year[i], "V", version)
  product_file_fullname <- paste0(product_file_name, "_", route_expanded$StateNum[i], "_", route_expanded$Route[i], ".rds")
  product_file_path <- here::here("output","routes", route_expanded$year[i], product_file_fullname)
  
  # Skip if file doesn't exist
  if (!file.exists(product_file_path)) {
    next
  }
  
  # Read file only if it exists
  product_file <- readRDS(product_file_path)
  
  
  
  
  # Define the classes you're interested in
  target_classes <- c("Developed, Open Space", "Developed, Low Intensity",
                      "Developed, Medium Intensity", "Developed, High Intensity")
  
  # Calculate total pixel count
  total_pixels <- sum(product_file$count)
  
  # Calculate total count for the selected classes
  target_pixels <- sum(product_file$count[product_file$value %in% target_classes])
  
  # Calculate percentage
  target_proportion <- target_pixels / total_pixels
  
  route_expanded$developed[i] <- target_proportion
}

output_file_fullname <- paste0(target_Classes, years_consider, ".csv")
output_file_path <- here::here("output", target_Classes, output_file_fullname)
write.csv(route_expanded, output_file_path, row.names = FALSE)


## Combine####
files <- list.files(
  here::here("output", target_Classes),
  pattern = "\\.csv$",
  full.names = TRUE
)

combined_data <- files %>%
  lapply(read_csv) %>%
  bind_rows()

output_combinedfile_fullname <- paste0(target_Classes, ".csv")
output_combinedfile_path <- here::here("output", target_Classes, output_combinedfile_fullname)
write.csv(combined_data, output_combinedfile_path, row.names = FALSE)
