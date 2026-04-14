# Compare the routes with the map with shape info
library(here) #setup
library(sf)
library(tidyverse)

here::i_am("code/0_prepare_routes.R")


## 1-to-1 relationship between point and line ####

# read in files
lines <- st_read(here::here("data", "BBS_USA_Routes_WGS84","BBS_USA_Routes_WGS84.shp"))

pts_df <- read_csv(here::here("data","Routes_2025Release.csv"))

# Convert csv to points
pts <- st_as_sf(
  pts_df,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# set coordinate
pts <- st_transform(pts, st_crs(lines))
# st_crs(pts)

# To ensure the reliability of distance units, 
# convert to a meter-based projected coordinate system.
lines_proj <- st_transform(lines, 3857)
pts_proj   <- st_transform(pts, 3857)

# create a unique line ID if one does not already exist
lines_proj <- lines_proj %>%
  mutate(line_id = row_number())

# Find the nearest line
nearest_id <- st_nearest_feature(pts_proj, lines_proj)

result <- bind_cols(
  pts_proj,
  st_drop_geometry(lines_proj[nearest_id, ])
)

# Calculate the distance between point and line
distances <- st_distance(pts_proj, lines_proj[nearest_id, ], by_element = TRUE)

result$dist_m <- as.numeric(distances)

# Set a acceptable distance threshold
result$matched_50m <- result$dist_m <= 50
result$matched_100m <- result$dist_m <= 100
result$matched_1km <- result$dist_m <= 1000

# Output full results
# st_write(result, here::here("output","matched_point_full", "matched_points_full.shp"))
  # delete_dsn = TRUE: delete the existing shapefile and write a fresh new one
write.csv(st_drop_geometry(result), here::here("output","matched_points_full.csv"), row.names = FALSE)

##Examine details####
# 1) matched_1km == TRUE but RouteName != RTENAME (case-insensitive)
library(stringi)
library(stringr)

clean_str <- function(x) {
  x %>%
    enc2utf8() %>%                          # ensure UTF-8 encoding
    stri_trans_tolower() %>%                # convert to lowercase
    str_replace_all("[[:punct:]]", " ") %>% # remove punctuation (replace with space)
    str_squish()                            # trim and reduce extra spaces
}

result_1 <- result %>%
  filter(
    matched_1km,
    clean_str(RouteName) != clean_str(RTENAME)
  )


write.csv(st_drop_geometry(result_1), here::here("output","matched_points_1.csv"), row.names = FALSE)

# 2) matched_1km == FALSE
result_2 <- result %>%
  filter(matched_1km == FALSE)

# 3) create a dataframe where the same line is used more than once
result_line_reused <- result %>%
  st_drop_geometry() %>%
  count(line_id, name = "n_points_on_same_line") %>%
  filter(n_points_on_same_line > 1) %>%
  left_join(result, by = "line_id")
