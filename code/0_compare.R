# 0_compare.R
# Run both matching approaches and compare results.
# v1: exact same name matching
# v2: fuzzy similar name matching (function-based)

library(here)
library(sf)
library(tidyverse)
library(stringi)
library(stringr)
library(stringdist)

here::i_am("code/0_compare.R")

dir.create(here::here("output"), showWarnings = FALSE, recursive = TRUE)

# ---- helper: normalise route names ----
clean_str <- function(x) {
  x %>%
    enc2utf8() %>%
    stri_trans_tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

# ---- read data ----
lines <- st_read(here::here("data", "BBS_USA_Routes_WGS84", "BBS_USA_Routes_WGS84.shp"))
pts_df <- read_csv(here::here("data", "Routes_2025Release.csv"))

pts <- st_as_sf(pts_df, coords = c("Longitude", "Latitude"), crs = 4326)
pts <- st_transform(pts, st_crs(lines))

lines_proj <- st_transform(lines, 3857)
pts_proj   <- st_transform(pts, 3857)

pts_proj   <- pts_proj   %>% mutate(pt_id      = row_number(),
                                     name_clean = clean_str(RouteName))
lines_proj <- lines_proj %>% mutate(line_id    = row_number(),
                                     name_clean = clean_str(RTENAME))

# ============================================================
# V1 – Exact same name + within 1 km
# ============================================================
cat("=== Running V1 (exact name + within 1 km) ===\n")

lines_buf  <- st_buffer(lines_proj, dist = 1000)
pts_in_buf <- st_join(pts_proj, lines_buf, join = st_within, suffix = c("", ".line"))

v1 <- pts_in_buf %>% filter(name_clean == name_clean.line)

if (any(duplicated(v1$pt_id))) {
  v1 <- v1 %>%
    mutate(dist_to_line = as.numeric(
      st_distance(geometry, lines_proj$geometry[line_id], by_element = TRUE)
    )) %>%
    group_by(pt_id) %>%
    slice_min(dist_to_line, n = 1, with_ties = FALSE) %>%
    ungroup()
}
cat("V1 matched:", n_distinct(v1$pt_id), "points\n")

v1_all <- v1 %>% st_drop_geometry() %>% select(pt_id, line_id)

# ============================================================
# V2 – Similar name (Jaro-Winkler >= 0.6) + within 1 km
# ============================================================
cat("\n=== Running V2 (similar name + within 1 km) ===\n")

# Use the same spatial join (points inside 1 km buffer of lines),
# but match on fuzzy name similarity instead of exact equality.
v2 <- pts_in_buf %>%
  mutate(name_similarity = stringsim(name_clean, name_clean.line, method = "jw")) %>%
  filter(name_similarity >= 0.6)

if (any(duplicated(v2$pt_id))) {
  v2 <- v2 %>%
    mutate(dist_to_line = as.numeric(
      st_distance(geometry, lines_proj$geometry[line_id], by_element = TRUE)
    )) %>%
    group_by(pt_id) %>%
    arrange(desc(name_similarity), dist_to_line) %>%
    slice(1) %>%
    ungroup()
}
cat("V2 matched:", n_distinct(v2$pt_id), "points\n")

v2_all <- v2 %>% st_drop_geometry() %>% select(pt_id, line_id)

# ============================================================
# COMPARE
# ============================================================
cat("\n=== Comparison ===\n")

# pairs in v1 but not v2
in_v1_not_v2 <- anti_join(v1_all, v2_all, by = c("pt_id", "line_id"))
cat("Pairs in v1 but not v2:", nrow(in_v1_not_v2), "\n")

# pairs in v2 but not v1
in_v2_not_v1 <- anti_join(v2_all, v1_all, by = c("pt_id", "line_id"))
cat("Pairs in v2 but not v1:", nrow(in_v2_not_v1), "\n")

# points matched in both but to different lines
common_pts <- inner_join(
  v1_all %>% rename(line_id_v1 = line_id),
  v2_all %>% rename(line_id_v2 = line_id),
  by = "pt_id"
)
diff_line <- common_pts %>% filter(line_id_v1 != line_id_v2)
cat("Points matched in both but to different lines:", nrow(diff_line), "\n")

# ---- name lookup tables ----
pt_names   <- pts_proj   %>% st_drop_geometry() %>% select(pt_id, RouteName)
line_names <- lines_proj %>% st_drop_geometry() %>% select(line_id, RTENAME)

# ---- write outputs ----
in_v1_not_v2 <- in_v1_not_v2 %>%
  left_join(pt_names,   by = "pt_id") %>%
  left_join(line_names, by = "line_id")

in_v2_not_v1 <- in_v2_not_v1 %>%
  left_join(pt_names,   by = "pt_id") %>%
  left_join(line_names, by = "line_id")

diff_line <- diff_line %>%
  left_join(pt_names, by = "pt_id") %>%
  left_join(line_names %>% rename(line_id_v1 = line_id, RTENAME_v1 = RTENAME), by = "line_id_v1") %>%
  left_join(line_names %>% rename(line_id_v2 = line_id, RTENAME_v2 = RTENAME), by = "line_id_v2")

write.csv(in_v1_not_v2, here::here("output", "compare_in_v1_not_v2.csv"), row.names = FALSE)
write.csv(in_v2_not_v1, here::here("output", "compare_in_v2_not_v1.csv"), row.names = FALSE)
write.csv(diff_line,     here::here("output", "compare_diff_line.csv"),    row.names = FALSE)

cat("Done.\n")
