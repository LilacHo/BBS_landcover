# BBS_landcover

Compute the proportion of land-cover types within 1 km of North American
Breeding Bird Survey (BBS) routes, using the annual
[NLCD](https://www.usgs.gov/centers/eros/science/national-land-cover-database)
land-cover product.

The workflow matches BBS route point records to the official BBS route line
geometries, buffers each line by 1 km, tabulates NLCD pixels inside each
buffer per year, and finally summarises the proportion of a chosen land-cover
category per route per year.

## Pipeline

Run the scripts in `code/` in order. Each one consumes the output of the
previous step.

| Step | Script | Purpose |
|------|--------|---------|
| 1 | `1_prepare_routes.R` | Match BBS route points to route lines (3-step funnel) |
| 2 | `2_prepare_route_buffer_1km.R` | Build 1-km buffers around matched lines, reprojected to the NLCD CRS |
| 3 | `3_prepare_nlcd.R` | Tabulate NLCD pixel frequencies inside each buffer, per year |
| 4 | `4_calculate_nlcd.R` | Compute the proportion of a target land-cover category per route/year |

`code/functions/pre_processing.R` holds helper functions that build the file
paths to the NLCD rasters.

### Step 1 — `1_prepare_routes.R`
Matches each route point to a route line using a 3-step funnel; each step only
considers points still unmatched by the previous step.

1. Similar name (Jaro-Winkler ≥ 0.6) **and** within a 1 km buffer.
2. Exact (cleaned) name **and** within 40 km of the closer line endpoint.
3. Nearest line within a 1 km buffer (any name).

- **Input:** `data/BBS_USA_Routes_WGS84/BBS_USA_Routes_WGS84.shp`,
  `data/Routes_2025Release.csv`
- **Output:** `output/result_perfect.csv`, `output/result_samename.csv`,
  `output/result_1km.csv`, `output/result_routes.csv`,
  `output/result_routes/result_routes.shp`, `output/result_failure.csv`

Distance/buffer operations use EPSG:5070 (NAD83 / CONUS Albers, equal area),
so the 1 km / 40 km thresholds are measured in true meters.

### Step 2 — `2_prepare_route_buffer_1km.R`
Buffers the matched route lines by 1 km (in EPSG:5070, equal area) and
reprojects the buffers to the NLCD raster CRS.

- **Input:** `output/result_routes/result_routes.shp`,
  `data/Routes_2025Release.csv`, one NLCD raster (for its CRS)
- **Output:** `data/buffer_1km/buffer_1km_proj.shp`

### Step 3 — `3_prepare_nlcd.R`
For each buffer and each year, crops/masks the NLCD raster to the buffer and
records a frequency table of land-cover classes. Alaska (`StateNum == 3`) is
excluded.

- **Input:** `data/buffer_1km/buffer_1km_proj.shp`, annual NLCD rasters
- **Output:** `output/routes_1km/<year>/<product><year>V<version>_<StateNum>_<Route>.rds`
  (columns: `layer`, `value`, `class`, `count`)

### Step 4 — `4_calculate_nlcd.R`
Computes, per route and year, the proportion of buffer pixels belonging to a
chosen land-cover category, and writes a single combined CSV.

- **Input:** `data/Routes_2025Release.csv` (filtered to USA `CountryNum == 840`,
  excluding Alaska `StateNum == 3`), the `.rds` tables from Step 3
- **Output:** `output/<target_name>.csv` (e.g. `output/aridland.csv`)

Select the target category by editing the settings near the top of the script:

```r
target_name   <- "aridland"
target_values <- c(31, 52)
```

Common categories (NLCD pixel values):

| Category | `target_values` |
|----------|-----------------|
| Grassland | `c(71)` |
| Developed | `c(21, 22, 23, 24)` |
| Aridland | `c(31, 52)` |
| Forest | `c(41, 42, 43)` |
| Cropland | `c(81, 82)` |

## Data

- `data/Routes_2025Release.csv` — BBS route records. Columns include
  `CountryNum`, `StateNum`, `Route`, `RouteName`, `Active`, `Latitude`,
  `Longitude`, `Stratum`, `BCR`, `RouteTypeID`, `RouteTypeDetailID`.
- `data/BBS_USA_Routes_WGS84/` — BBS route line shapefile (WGS84).
- `data/Annual_NLCD_<product>_<year>_CU_C1V<version>/` — annual NLCD rasters.
  These are **not** included in the repository; download them and place each
  raster in its own folder, e.g.
  `data/Annual_NLCD_LndCov_2024_CU_C1V1/Annual_NLCD_LndCov_2024_CU_C1V1.tif`.
  Path construction is handled by `input_nlcd_path()` in
  `code/functions/pre_processing.R`.

## Requirements

- R (≥ 4.1 recommended)
- R packages: `here`, `sf`, `terra`, `tidyverse`, `stringi`, `stringr`,
  `stringdist`, `lwgeom`

```r
install.packages(c("here", "sf", "terra", "tidyverse",
                   "stringi", "stringr", "stringdist", "lwgeom"))
```

The project uses [`here`](https://here.r-lib.org/) for path management, so run
the scripts from the project root (or open the project so `here` can locate the
root automatically).
