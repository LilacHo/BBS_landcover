# ===============================================================
# compare_crs_3857_vs_5070.R
# ---------------------------------------------------------------
# PURPOSE
#   Run the same 3-step point-to-line matching used in
#   1_prepare_routes.R under two projected CRS - EPSG:3857
#   (Web Mercator) and EPSG:5070 (NAD83 / CONUS Albers, equal area) -
#   and report whether the resulting route matches differ.
#
#   Why they can differ: 3857 inflates projected distances by roughly
#   1 / cos(latitude), so a "1000 m" or "40000 m" threshold in 3857 is
#   smaller on the ground (and varies with latitude). 5070 preserves
#   true area/distance across the contiguous US. Points near the
#   1 km / 40 km thresholds can therefore flip between CRS.
#
# INPUT  (same as 1_prepare_routes.R)
#   data/BBS_USA_Routes_WGS84/BBS_USA_Routes_WGS84.shp
#   data/Routes_2025Release.csv
#
# OUTPUT
#   Console summary of how many points match in each CRS and how many
#   differ, plus:
#   output/crs_comparison.csv  - per-point comparison
#       (pt_id, RouteName, line_id/match_step for each CRS, changed flag)
# ===============================================================

library(here)
library(sf)
library(tidyverse)
library(stringi)
library(stringr)
library(stringdist)
library(lwgeom)

here::i_am("code/compare_crs_3857_vs_5070.R")

clean_str <- function(x) {
  x %>%
    enc2utf8() %>%
    stri_trans_tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

# ---- read data once ----
lines <- st_read(here::here("data", "BBS_USA_Routes_WGS84", "BBS_USA_Routes_WGS84.shp"))
pts_df <- read_csv(here::here("data", "Routes_2025Release.csv"))

pts <- st_as_sf(pts_df, coords = c("Longitude", "Latitude"), crs = 4326)
pts <- st_transform(pts, st_crs(lines))

# ---------------------------------------------------------------
# run_matching(): the 3-step funnel, parameterised by CRS.
# Returns a data.frame: pt_id, line_id, match_step (one row per
# matched point; unmatched points are absent).
# ---------------------------------------------------------------
run_matching <- function(target_crs) {

  lines_proj <- st_transform(lines, target_crs) %>%
    mutate(line_id = row_number(), name_clean = clean_str(RTENAME))
  pts_proj <- st_transform(pts, target_crs) %>%
    mutate(pt_id = row_number(), name_clean = clean_str(RouteName))

  ## STEP 1 - similar name + within 1 km ----
  lines_buf_1km <- st_buffer(lines_proj, dist = 1000)
  pts_in_1km <- st_join(pts_proj, lines_buf_1km, join = st_within, suffix = c("", ".line"))

  step1 <- pts_in_1km %>%
    mutate(name_similarity = stringsim(name_clean, name_clean.line, method = "jw")) %>%
    filter(name_similarity >= 0.6)

  if (any(duplicated(step1$pt_id))) {
    step1 <- step1 %>%
      mutate(dist_to_line = as.numeric(
        st_distance(geometry, lines_proj$geometry[line_id], by_element = TRUE)
      )) %>%
      group_by(pt_id) %>%
      arrange(desc(name_similarity), dist_to_line) %>%
      slice(1) %>%
      ungroup()
  }
  step1_ids <- step1$pt_id

  ## STEP 2 - exact same name + within 40 km of closer endpoint ----
  pts_remaining2 <- pts_proj %>% filter(!(pt_id %in% step1_ids))

  lines_single <- st_cast(lines_proj, "LINESTRING")
  line_endpoints <- lines_single %>%
    mutate(start_pt = lwgeom::st_startpoint(geometry),
           end_pt   = lwgeom::st_endpoint(geometry))

  step2_candidates <- inner_join(
    pts_remaining2 %>% st_drop_geometry() %>% select(pt_id, name_clean),
    line_endpoints %>% st_drop_geometry() %>% select(line_id, name_clean),
    by = "name_clean",
    relationship = "many-to-many"
  )

  if (nrow(step2_candidates) > 0) {
    step2_candidates$dist_start <- as.numeric(st_distance(
      pts_remaining2$geometry[match(step2_candidates$pt_id, pts_remaining2$pt_id)],
      line_endpoints$start_pt[match(step2_candidates$line_id, line_endpoints$line_id)],
      by_element = TRUE))
    step2_candidates$dist_end <- as.numeric(st_distance(
      pts_remaining2$geometry[match(step2_candidates$pt_id, pts_remaining2$pt_id)],
      line_endpoints$end_pt[match(step2_candidates$line_id, line_endpoints$line_id)],
      by_element = TRUE))
    step2_candidates$dist_closer_endpoint <- pmin(step2_candidates$dist_start, step2_candidates$dist_end)

    step2 <- step2_candidates %>%
      filter(dist_closer_endpoint <= 40000) %>%
      group_by(pt_id) %>%
      slice_min(dist_closer_endpoint, n = 1, with_ties = FALSE) %>%
      ungroup()
    step2_ids <- step2$pt_id
  } else {
    step2 <- tibble(pt_id = integer(0), line_id = integer(0))
    step2_ids <- integer(0)
  }

  ## STEP 3 - nearest within 1 km (any name) ----
  pts_remaining3 <- pts_proj %>% filter(!(pt_id %in% c(step1_ids, step2_ids)))

  if (nrow(pts_remaining3) > 0) {
    nearest_id <- st_nearest_feature(pts_remaining3, lines_proj)
    distances  <- as.numeric(st_distance(pts_remaining3, lines_proj[nearest_id, ], by_element = TRUE))
    step3 <- pts_remaining3 %>%
      st_drop_geometry() %>%
      mutate(line_id = lines_proj$line_id[nearest_id], dist_m = distances) %>%
      filter(dist_m <= 1000)
  } else {
    step3 <- tibble(pt_id = integer(0), line_id = integer(0))
  }

  bind_rows(
    tibble(pt_id = step1$pt_id, line_id = step1$line_id, match_step = "step1"),
    tibble(pt_id = step2$pt_id, line_id = step2$line_id, match_step = "step2"),
    tibble(pt_id = step3$pt_id, line_id = step3$line_id, match_step = "step3")
  )
}

# ---- run both CRS ----
cat("=== Running matching in EPSG:3857 ===\n")
res_3857 <- run_matching(3857)
cat("=== Running matching in EPSG:5070 ===\n")
res_5070 <- run_matching(5070)

# ---- compare ----
all_pt_ids <- seq_len(nrow(pts))

cmp <- tibble(pt_id = all_pt_ids) %>%
  left_join(res_3857 %>% rename(line_id_3857 = line_id, step_3857 = match_step), by = "pt_id") %>%
  left_join(res_5070 %>% rename(line_id_5070 = line_id, step_5070 = match_step), by = "pt_id") %>%
  mutate(
    RouteName = pts_df$RouteName[pt_id],
    matched_3857 = !is.na(line_id_3857),
    matched_5070 = !is.na(line_id_5070),
    # compare line assignment, treating unmatched (NA) as its own value
    line_changed  = !(coalesce(as.character(line_id_3857), "NA") ==
                      coalesce(as.character(line_id_5070), "NA")),
    match_changed = matched_3857 != matched_5070
  )

n_total       <- nrow(cmp)
n_3857        <- sum(cmp$matched_3857)
n_5070        <- sum(cmp$matched_5070)
n_line_diff   <- sum(cmp$line_changed)
n_match_diff  <- sum(cmp$match_changed)
n_only_3857   <- sum(cmp$matched_3857 & !cmp$matched_5070)
n_only_5070   <- sum(!cmp$matched_3857 & cmp$matched_5070)

cat("\n================ COMPARISON SUMMARY ================\n")
cat("Total points:                 ", n_total, "\n")
cat("Matched in 3857:              ", n_3857, "\n")
cat("Matched in 5070:              ", n_5070, "\n")
cat("Matched in only 3857:         ", n_only_3857, "\n")
cat("Matched in only 5070:         ", n_only_5070, "\n")
cat("Different line assignment:    ", n_line_diff,
    "  (", round(100 * n_line_diff / n_total, 2), "% )\n", sep = "")
cat("Matched/unmatched flips:      ", n_match_diff, "\n")
cat("===================================================\n")

if (n_line_diff == 0) {
  cat("\nNo differences: the two CRS produce identical route matches.\n")
} else {
  cat("\nThe two CRS produce DIFFERENT route matches for the points above.\n")
}

dir.create(here::here("output"), showWarnings = FALSE, recursive = TRUE)
write.csv(
  cmp %>% filter(line_changed) %>%
    select(pt_id, RouteName, line_id_3857, step_3857, line_id_5070, step_5070),
  here::here("output", "crs_comparison.csv"),
  row.names = FALSE
)
cat("\nDiffering points written to output/crs_comparison.csv\n")
