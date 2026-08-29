

library(tidyverse)
library(foreign)
library(sf)

# ============================================================
# Generic dLUC emission-factor (EF) pipeline
#
# Computes dLUC emission factors (tonnes gas / tonne of product)
# for ANY commodity, from a carbon flux model (geotrellis tool) emissions export joined to a location
# attribute table (key, Location_Name, yield).
#
# Geotrellis itself runs outside R (it's an external zonal-statistics tool).
# This script covers everything on the R side:
#   - PART 0 (shared functions): read the geotrellis output, compute the 20-year
#     linearly discounted (LD) emissions, join production, compute the EF. Identical whether your input locations
#     were polygons or buffered points.
#   - PART A: use this when you already have POLYGON locations (e.g. drawn
#     concession/farm boundaries) that were submitted to geotrellis as-is.
#   - PART B: use this when you have POINT locations that need to be turned
#     into circular buffer polygons (a chosen radius) BEFORE being submitted
#     to geotrellis. Adds one extra step (create_point_buffers); everything
#     after that is the same shared pipeline as Part A.
#
# Run Part 0 first (defines the functions), then either Part A or Part B
# (or both, for different commodities/geometries) below it.
# ============================================================

# ---- User settings ----
# Folder containing your inputs (attribute dbf/shapefiles, geotrellis output,
# optional production/IDs/Pro-export files) and where outputs are written.
data_dir <- "path/to/your/data/folder"

commodity <- "RUBB"     # short code used in file names and columns, e.g. "RUBB", "COCO", "OILP", "SOYB"
years <- 2020:2024      # years to compute LD/EF for

# ============================================================
# PART 0: Shared functions
# ============================================================

# Linear discount (LD): linearly increasing weight (0.0025 for the
# oldest conversion year up to 0.0975 for the most recent). LD_<year> sums
# that year's conversion and each of the prior 19 years' emissions, weighted
# by recency. `prefix` is the commodity code used in the yearly emission
# column names (<prefix>_<year>).
calculate_LD <- function(data, prefix, start_year, end_year) {
  LD_weights <- seq(0.0025, 0.0975, by = 0.005)
  
  for (year in start_year:end_year) {
    ld_col <- paste0("LD_", year)
    data[[ld_col]] <- 0
    
    for (j in 1:20) {
      source_col <- paste0(prefix, "_", year - j + 1)
      if (source_col %in% names(data)) {
        data[[ld_col]] <- data[[ld_col]] + data[[source_col]] * LD_weights[20 - j + 1]
      }
    }
  }
  data
}

# Insert an underscore between a trailing letter block and digit block,
# e.g. "RUBB12" -> "RUBB_12". Safe to run on names that already have the
# underscore (e.g. "RUBB_12") — the pattern won't match, so nothing changes.
format_location_names <- function(df, col = "Location_Name") {
  df[[col]] <- str_replace(df[[col]], "([A-Z]+)([0-9]+)", "\\1_\\2")
  df
}

# Parse a GFW Pro "production_pro_<commodity>.txt" export (comma-separated
# lines like `Fid 2063, 2063, RUBB, production 12.5`) and attach the
# Location_Name that goes with each location_id via an id-mapping file.
# id_offset lets you correct an off-by-one between the production export's
# ids and your ids file (e.g. pass -1 if the export is 1-based but your ids
# file is 0-based).
read_pro_production <- function(production_file, ids_file, id_offset = 0) {
  ids <- read.delim(ids_file, sep = "\t")
  if (id_offset != 0) ids$location_id <- ids$location_id + id_offset
  
  lines <- readLines(production_file)
  split_lines <- strsplit(lines, ",")
  
  parsed <- lapply(split_lines, function(x) {
    data.frame(
      location_id = as.numeric(trimws(x[2])),
      production  = as.numeric(gsub("production ", "", trimws(x[4]))),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, parsed) %>% left_join(ids, by = "location_id")
}

# Read a geotrellis output file and keep only the columns needed: the join
# key, area, and every gas's yearly emission column (matched by pattern
# rather than column position, so column order in the raw file doesn't matter).
load_geotrellis_output <- function(file) {
  emission_pattern <- "^gfw_forest_carbon_gross_emissions_(all_gases|CO2|CH4|N2O)_[0-9]{4}__Mg_CO2e$"
  read.table(file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = "") %>%
    select(feature__id, area__ha, matches(emission_pattern))
}

# Split the joined attribute+geotrellis table into one wide table per gas,
# with that gas's yearly columns renamed to <prefix>_<year> (what calculate_LD expects).
build_gas_tables <- function(joined_data, prefix) {
  gas_keys <- c(CO2e = "all_gases", CO2 = "CO2", CH4 = "CH4", N2O = "N2O")
  
  gas_tables <- list()
  for (gas in names(gas_keys)) {
    pattern <- paste0("^gfw_forest_carbon_gross_emissions_", gas_keys[[gas]], "_[0-9]{4}__Mg_CO2e$")
    emission_cols <- names(joined_data)[grepl(pattern, names(joined_data))]
    
    gas_df <- joined_data %>%
      select(key, Location_Name, yield, area__ha, all_of(emission_cols))
    
    years_found <- str_extract(emission_cols, "[0-9]{4}")
    names(gas_df)[match(emission_cols, names(gas_df))] <- paste0(prefix, "_", years_found)
    
    gas_tables[[gas]] <- gas_df
  }
  gas_tables
}

# Join production, compute EF_<year>_<gas> = LD_<year> / production, and
# reshape to long format (one row per location x year) with the year pulled
# out of the column name.
compute_ef_long <- function(ld_data, gas_label, production_df, years) {
  ld_data <- ld_data %>% left_join(production_df, by = "Location_Name")
  
  for (year in years) {
    ld_data[[paste0("EF_", year, "_", gas_label)]] <- ld_data[[paste0("LD_", year)]] / ld_data$production
  }
  
  ef_cols <- paste0("EF_", years, "_", gas_label)
  # Only the CO2e table needs to carry the shared metadata columns forward —
  # the other gases just need "key" (and Year, added by pivot_longer below)
  # so the final inner_join across gases doesn't duplicate columns.
  keep_cols <- if (gas_label == "CO2e") c("key", "Location_Name", "area__ha", "yield", ef_cols) else c("key", ef_cols)
  
  long <- ld_data %>%
    select(all_of(keep_cols)) %>%
    pivot_longer(cols = all_of(ef_cols), names_to = "Year", values_to = paste0(gas_label, "_Emission_Factor"))
  
  long$Year <- str_extract(long$Year, "\\d{4}")
  long
}

# Full pipeline: attribute table (key, Location_Name, yield) + geotrellis
# output -> LD -> EF, for all four gases. Works the same whether
# attribute_table came from a polygon shapefile or a buffered-points shapefile.
run_ef_pipeline <- function(attribute_table, geotrellis_file, prefix, years,
                            production_df = NULL, yield_is_kg_per_ha = TRUE,
                            output_file = NULL) {
  
  cat("=== Running EF pipeline for", prefix, "===\n")
  
  geotrellis <- load_geotrellis_output(geotrellis_file)
  
  joined <- attribute_table %>%
    select(key, Location_Name, yield) %>%
    inner_join(geotrellis, by = c("key" = "feature__id"))
  
  cat("Joined data:", nrow(joined), "rows\n")
  
  # Fall back to yield x area if no reported production values were supplied
  # (yield assumed to be kg/ha unless yield_is_kg_per_ha = FALSE)
  if (is.null(production_df)) {
    cat("No production data supplied - estimating production as yield x area\n")
    production_df <- joined %>%
      distinct(Location_Name, yield, area__ha) %>%
      mutate(production = if (yield_is_kg_per_ha) (yield / 1000) * area__ha else yield * area__ha) %>%
      select(Location_Name, production)
  }
  
  gas_tables <- build_gas_tables(joined, prefix)
  
  long_tables <- list()
  for (gas in names(gas_tables)) {
    ld_data <- calculate_LD(gas_tables[[gas]], prefix, min(years), max(years))
    long_tables[[gas]] <- compute_ef_long(ld_data, gas, production_df, years)
  }
  
  ef_long <- reduce(long_tables, inner_join) %>%
    filter(complete.cases(.))
  
  cat("Final EF table:", nrow(ef_long), "rows\n")
  
  if (!is.null(output_file)) {
    write.csv(ef_long, output_file, row.names = FALSE)
    cat("Saved EF results to", output_file, "\n")
  }
  
  ef_long
}


# ============================================================
# PART A: Polygon geometry (no buffering needed)
# ============================================================
# Use this when your locations are already polygons submitted to geotrellis
# as-is (e.g. "COCO_poly.dbf").

polygon_dbf <- file.path(data_dir, paste0(commodity, "_poly.dbf"))
polygon_attributes <- read.dbf(polygon_dbf) %>%
  rename(Location_Name = Location_N) # dbf truncates "Location_Name" to 10 chars

geotrellis_file_poly <- file.path(data_dir, "geotrellis_results", paste0(commodity, "_output.csv"))

# Optional: real reported production values (set to NULL to fall back to
# yield x area instead)
production_file <- file.path(data_dir, paste0("production_pro_", tolower(commodity), ".txt"))
ids_file        <- file.path(data_dir, paste0(commodity, "_IDS.txt"))

production_df <- if (file.exists(production_file) && file.exists(ids_file)) {
  read_pro_production(production_file, ids_file) %>% format_location_names()
} else {
  NULL # run_ef_pipeline will fall back to yield x area
}

results_polygon <- run_ef_pipeline(
  attribute_table = polygon_attributes,
  geotrellis_file = geotrellis_file_poly,
  prefix          = commodity,
  years           = years,
  production_df   = production_df,
  output_file     = file.path(data_dir, paste0("QA_", commodity, "_poly_EF.csv"))
)


# ============================================================
# PART B: Point + radius (buffer points into polygons first)
# ============================================================
# Use this when your locations are points that need to become circular
# polygons before being submitted to geotrellis. Everything after the buffer
# step reuses the exact same shared pipeline as Part A.

buffer_radius_m <- 1000 # buffer radius in meters — set to your project's radius

points_file        <- file.path(data_dir, paste0(commodity, "_points.shp"))
buffered_shapefile <- file.path(data_dir, paste0(commodity, "_points_buffer.shp"))

# Buffer each point by `radius_m` and write the result as a polygon
# shapefile ready to submit to geotrellis.
create_point_buffers <- function(points_file, radius_m, output_file, buffer_crs = 3857) {
  points <- st_read(points_file, quiet = TRUE)
  original_crs <- st_crs(points)
  
  # Buffering requires a projected (metric) CRS. 3857 (Web Mercator) is a
  # rough default; for accurate buffer areas, replace it with a local UTM
  # zone or an equal-area projection appropriate to your study area.
  buffered <- points %>%
    st_transform(buffer_crs) %>%
    st_buffer(dist = radius_m) %>%
    st_transform(original_crs) %>%
    mutate(key = row_number() - 1) # 0-based — confirm this matches how your
  # geotrellis submission tool assigns feature__id before relying on it
  
  st_write(buffered, output_file, delete_layer = TRUE)
  cat("Buffered", nrow(buffered), "points by", radius_m, "m ->", output_file, "\n")
  buffered
}

buffered_polygons <- create_point_buffers(points_file, buffer_radius_m, buffered_shapefile)

# ---- Submit `buffered_shapefile` to geotrellis outside R. -----------------
# Once results come back, continue below with the buffer's attribute table
# (the .dbf written alongside buffered_shapefile) and geotrellis's output file.

buffer_dbf <- file.path(data_dir, paste0(commodity, "_points_buffer.dbf"))
buffer_attributes <- read.dbf(buffer_dbf) %>%
  rename(Location_Name = Location_N)

geotrellis_file_pts <- file.path(data_dir, "geotrellis_results", paste0(commodity, "_points_output.csv"))

results_points <- run_ef_pipeline(
  attribute_table = buffer_attributes,
  geotrellis_file = geotrellis_file_pts,
  prefix          = commodity,
  years           = years,
  production_df   = production_df,
  output_file     = file.path(data_dir, paste0("QA_", commodity, "_points_EF.csv"))
)
