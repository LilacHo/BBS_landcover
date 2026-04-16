# Link BBS route points to route lines via a 3-step funnel.
# Step 1: similar name (Jaro-Winkler >= 0.6) + within 1 km buffer
# Step 2: exact same name + within 40 km of closer endpoint of line
# Step 3: within 1 km buffer (any name)
# One point → one line; one line → multiple points OK.

library(here)
library(sf)
library(tidyverse)
library(stringi)
library(stringr)
library(stringdist)
library(lwgeom)

here::i_am("code/0_prepare_route.R")

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

dir.create(here::here("output"), showWarnings = FALSE, recursive = TRUE)

# ============================================================
# STEP 1 – Similar name + within 1 km
# ============================================================
cat("=== Step 1: similar name + within 1 km ===\n")

lines_buf_1km <- st_buffer(lines_proj, dist = 1000)
pts_in_1km <- st_join(pts_proj, lines_buf_1km, join = st_within, suffix = c("", ".line"))

step1 <- pts_in_1km %>%
  mutate(name_similarity = stringsim(name_clean, name_clean.line, method = "jw")) %>%
  filter(name_similarity >= 0.6)

# de-duplicate: one point → best name similarity, then closest line
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

step1_out <- step1 %>% st_drop_geometry()
write.csv(step1_out, here::here("output", "result_perfect.csv"), row.names = FALSE)
cat("Step 1 matched:", n_distinct(step1$pt_id), "points\n")

# ============================================================
# STEP 2 – Exact same name + within 40 km of closer endpoint
# ============================================================
cat("\n=== Step 2: exact same name + within 40 km of endpoint ===\n")

pts_remaining2 <- pts_proj %>% filter(!(pt_id %in% step1$pt_id))

# extract start and end points of each line
# cast to LINESTRING in case geometry is MULTILINESTRING
lines_single <- st_cast(lines_proj, "LINESTRING")
line_endpoints <- lines_single %>%
  mutate(
    start_pt = lwgeom::st_startpoint(geometry),
    end_pt   = lwgeom::st_endpoint(geometry)
  )

# cross-join remaining points with lines on exact cleaned name
step2_candidates <- inner_join(
  pts_remaining2 %>% st_drop_geometry() %>% select(pt_id, name_clean),
  line_endpoints %>% st_drop_geometry() %>% select(line_id, name_clean),
  by = "name_clean",
  relationship = "many-to-many"
)

if (nrow(step2_candidates) > 0) {
  # compute distance from point to each endpoint, keep the closer one
  step2_candidates$dist_start <- as.numeric(
    st_distance(
      pts_remaining2$geometry[match(step2_candidates$pt_id, pts_remaining2$pt_id)],
      line_endpoints$start_pt[match(step2_candidates$line_id, line_endpoints$line_id)],
      by_element = TRUE
    )
  )
  step2_candidates$dist_end <- as.numeric(
    st_distance(
      pts_remaining2$geometry[match(step2_candidates$pt_id, pts_remaining2$pt_id)],
      line_endpoints$end_pt[match(step2_candidates$line_id, line_endpoints$line_id)],
      by_element = TRUE
    )
  )
  step2_candidates$dist_closer_endpoint <- pmin(step2_candidates$dist_start, step2_candidates$dist_end)

  # filter: within 40 km of closer endpoint
  step2 <- step2_candidates %>%
    filter(dist_closer_endpoint <= 40000) %>%
    group_by(pt_id) %>%
    slice_min(dist_closer_endpoint, n = 1, with_ties = FALSE) %>%
    ungroup()

  # attach full attributes
  step2_out <- step2 %>%
    left_join(st_drop_geometry(pts_proj),   by = "pt_id", suffix = c("", ".pt")) %>%
    left_join(st_drop_geometry(lines_proj), by = "line_id", suffix = c("", ".line"))

  write.csv(step2_out, here::here("output", "result_samename.csv"), row.names = FALSE)
  cat("Step 2 matched:", n_distinct(step2$pt_id), "points\n")

  step2_ids <- unique(step2$pt_id)
} else {
  write.csv(data.frame(), here::here("output", "result_samename.csv"), row.names = FALSE)
  cat("Step 2 matched: 0 points\n")
  step2_ids <- integer(0)
}

# ============================================================
# STEP 3 – Within 1 km (any name)
# ============================================================
cat("\n=== Step 3: within 1 km (any name) ===\n")

pts_remaining3 <- pts_proj %>% filter(!(pt_id %in% c(step1$pt_id, step2_ids)))

if (nrow(pts_remaining3) > 0) {
  nearest_id <- st_nearest_feature(pts_remaining3, lines_proj)
  distances  <- as.numeric(st_distance(pts_remaining3, lines_proj[nearest_id, ], by_element = TRUE))

  step3_result <- bind_cols(
    pts_remaining3,
    st_drop_geometry(lines_proj[nearest_id, ])
  ) %>%
    mutate(dist_m = distances) %>%
    filter(dist_m <= 1000)

  step3_out <- step3_result %>% st_drop_geometry()
  write.csv(step3_out, here::here("output", "result_1km.csv"), row.names = FALSE)
  cat("Step 3 matched:", nrow(step3_result), "points\n")

  step3_ids <- step3_result$pt_id
} else {
  write.csv(data.frame(), here::here("output", "result_1km.csv"), row.names = FALSE)
  cat("Step 3: no remaining points\n")
  step3_ids <- integer(0)
}

# ============================================================
# COMBINED RESULTS + FAILURES
# ============================================================
cat("\n=== Final output ===\n")

all_matched_ids <- c(step1$pt_id, step2_ids, step3_ids)

result_routes <- bind_rows(
  step1_out %>% select(pt_id, line_id) %>% mutate(match_step = "step1_similar_name_1km"),
  if (length(step2_ids) > 0)
    step2 %>% select(pt_id, line_id) %>% mutate(match_step = "step2_exact_name_40km_endpoint"),
  if (length(step3_ids) > 0)
    step3_out %>% select(pt_id, line_id) %>% mutate(match_step = "step3_nearest_1km")
) %>%
  left_join(st_drop_geometry(pts_proj)   %>% select(pt_id, RouteName), by = "pt_id") %>%
  left_join(st_drop_geometry(lines_proj) %>% select(line_id, RTENAME),  by = "line_id")

write.csv(result_routes, here::here("output", "result_routes.csv"), row.names = FALSE)
cat("Total matched:", nrow(result_routes), "points\n")

# shapefile: join matched results with line geometries
result_routes_sf <- result_routes %>%
  left_join(lines_proj %>% select(line_id, geometry), by = "line_id") %>%
  st_as_sf()
st_write(result_routes_sf, here::here("output", "result_routes"， "result_routes.shp"), delete_dsn = TRUE)

# failures
failure_ids <- setdiff(pts_proj$pt_id, all_matched_ids)
result_failure <- pts_proj %>% filter(pt_id %in% failure_ids) %>% st_drop_geometry()
write.csv(result_failure, here::here("output", "result_failure.csv"), row.names = FALSE)
cat("Unmatched:", nrow(result_failure), "points\n")

cat("Done.\n")
