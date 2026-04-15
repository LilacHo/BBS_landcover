# Compare the routes with the map with shape info
library(here) #setup
library(sf)
library(tidyverse)
library(stringi)
library(stringr)

here::i_am("code/0_prepare_routes.R")

# ---- helper: normalise route names for comparison ----
clean_str <- function(x) {
  x %>%
    enc2utf8() %>%
    stri_trans_tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

# ---- read data ----
lines <- st_read(here::here("data", "BBS_USA_Routes_WGS84", "BBS_USA_Routes_WGS84.shp"))
# write.csv(st_drop_geometry(lines), here::here("data","BBS_USA_Routes_WGS84.csv"), row.names = FALSE)
pts_df <- read_csv(here::here("data", "Routes_2025Release.csv"))

# Convert csv to points
pts <- st_as_sf(
  pts_df,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Align CRS, then project to metre-based CRS for distance calculations
pts <- st_transform(pts, st_crs(lines))
lines_proj <- st_transform(lines, 3857)
pts_proj   <- st_transform(pts, 3857)

# Add unique IDs
pts_proj <- pts_proj %>% mutate(pt_id = row_number())
lines_proj <- lines_proj %>% mutate(line_id = row_number())


# ============================================================
# STEP 1 – Perfect match: same name AND point within 1 km
# ============================================================

# Normalised names for matching
pts_proj   <- pts_proj   %>% mutate(name_clean = clean_str(RouteName))
lines_proj <- lines_proj %>% mutate(name_clean = clean_str(RTENAME))

# Buffer each line by 1 km
lines_buf <- st_buffer(lines_proj, dist = 1000)

# Spatial join: points that fall inside a 1 km buffer of any line
pts_in_buf <- st_join(pts_proj, lines_buf, join = st_within, suffix = c("", ".line"))

# Keep only rows where the cleaned names match
perfect0 <- pts_in_buf %>%
  filter(name_clean == name_clean.line)

# De-duplicate: if a point matched multiple lines with the same name,
# keep the one whose line geometry is closest
if (any(duplicated(perfect0$pt_id))) {
  # compute distance to each matched line
  perfect <- perfect0 %>%
    mutate(
      dist_to_line = as.numeric(
        st_distance(
          geometry,
          lines_proj$geometry[line_id],
          by_element = TRUE
        )
      )
    ) %>%
    group_by(pt_id) %>%
    slice_min(dist_to_line, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# Check the row difference between perfect0 and perfect
diff_removed <- anti_join(
  st_drop_geometry(perfect0),
  st_drop_geometry(perfect),
  by = colnames(st_drop_geometry(perfect0))
)

# ---- Check 1: one line matched to more than one point ----
check1 <- perfect %>%
  st_drop_geometry() %>%
  count(line_id, name = "n_pts") %>%
  filter(n_pts > 1) %>%
  left_join(st_drop_geometry(perfect), by = "line_id")

# write.csv(check1, here::here("output", "result_check1.csv"), row.names = FALSE)
cat("Step 1 check – lines matched to >1 point:", nrow(check1), "rows\n")

# ---- Check 1b: same name but NOT within 1 km ----
# These are point–line pairs that share a name but the point fell outside
# every same-name line's 1 km buffer (i.e. name matched, distance didn't).
# We cross-join points and lines on cleaned name, then remove pairs already
# captured in the perfect-match set, and compute the actual distance.


samename_pairs <- inner_join(
  pts_proj %>% st_drop_geometry() %>% select(pt_id, RouteName, name_clean),
  lines_proj %>% st_drop_geometry() %>% select(line_id, RTENAME, name_clean),
  by = "name_clean",
  relationship = "many-to-many"
)


# Remove pairs that were already perfect matches
perfect_pairs <- perfect %>%
  st_drop_geometry() %>%
  select(pt_id, line_id)


samename_notperfect <- anti_join(samename_pairs, perfect_pairs, by = c("pt_id", "line_id"))


if (nrow(samename_notperfect) > 0) {
  samename_notperfect$dist_m <- as.numeric(
    st_distance(
      pts_proj$geometry[samename_notperfect$pt_id],
      lines_proj$geometry[samename_notperfect$line_id],
      by_element = TRUE
    )
  )
}

write.csv(samename_notperfect,
          here::here("output", "result_check1_samename.csv"), row.names = FALSE)
cat("Step 1 check – same name but >1 km:", nrow(samename_notperfect), "rows\n")
# 
# # ---- Check 2: same-name pairs that ARE within 1 km but missed perfect match ----
# # These slipped through Step 1 (e.g. point was inside buffer of a *different*
# # same-name line that won the de-duplicate, but also <1 km from another line).
# check2 <- samename_notperfect %>%
#   filter(dist_m < 1000, !(pt_id %in% perfect$pt_id))
# write.csv(check2, here::here("output", "result_check2.csv"), row.names = FALSE)
# cat("Step 1 check – same name & <1 km, not in perfect:", nrow(check2), "rows\n")


# Write perfect matches
write.csv(st_drop_geometry(perfect), here::here("output", "result_perfect.csv"), row.names = FALSE)
cat("Step 1 – perfect matches:", n_distinct(perfect$pt_id), "points\n")

# ============================================================
# STEP 2 – Remaining points: nearest-line within 1 km
# ============================================================

remaining_ids <- setdiff(pts_proj$pt_id, perfect$pt_id)
pts_remaining <- pts_proj %>% filter(pt_id %in% remaining_ids)

nearest_id <- st_nearest_feature(pts_remaining, lines_proj)

buffer_result <- bind_cols(
  pts_remaining,
  st_drop_geometry(lines_proj[nearest_id, ])
)

distances <- st_distance(pts_remaining, lines_proj[nearest_id, ], by_element = TRUE)
buffer_result$dist_m <- as.numeric(distances)
buffer_result$match_1km <- buffer_result$dist_m <= 1000

# Keep only those within 1 km
result_buffer <- buffer_result %>% filter(match_1km)
write.csv(st_drop_geometry(result_buffer),
          here::here("output", "result_buffer.csv"), row.names = FALSE)
cat("Step 2 – buffer matches (<=1 km):", nrow(result_buffer), "points\n")

# ============================================================
# STEP 3 – Failures: points that could not be matched
# ============================================================
matched_buffer_ids <- result_buffer$pt_id
failure_ids <- setdiff(pts_remaining$pt_id, matched_buffer_ids)
result_failure <- pts_proj %>% filter(pt_id %in% failure_ids)

write.csv(st_drop_geometry(result_failure),
          here::here("output", "result_failure.csv"), row.names = FALSE)
cat("Step 3 – unmatched points:", nrow(result_failure), "\n")


cat("Done.\n")
