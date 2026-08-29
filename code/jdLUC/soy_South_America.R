library(tidyverse)
library(foreign)

# ============================================================
# Soy (SOYB) emission factor (EF) calculation for Brazil, Argentina,
# Paraguay, Bolivia, Uruguay
#
# Part 1: build per-gas, per-year "linear discount" (LD) land-use-change
#         emissions tables at admin0/admin1/admin2 level, from raw
#         geotrellis emissions CSVs (one per year) joined to that year's
#         admin boundaries.
# Part 2: combine those LD tables with a yield table to compute production
#         and emission factors (tonnes gas per tonne of soy produced), at
#         all three admin levels.
#
# ============================================================

# ---- User settings ----
# Set this to the folder containing your input files (soy_<year>_intersect/
# subfolders with their .dbf boundaries, GHG_emissions_soy_<year>.csv files,
# and yield_area_gadm2_spam20v2.csv) and where all intermediate and final
# outputs should be written.
# Example (Windows): "C:/Users/<your_username>/Downloads"
# Example (Mac/Linux): "/Users/<your_username>/Downloads"
data_dir <- "path/to/your/data/folder"

PROCESSING_YEARS <- c(2020, 2021, 2022, 2023, 2024)
GAS_TYPES <- c("CO2e", "CO2", "CH4", "N2O")

# ============================================================
# PART 1: Build LD emissions tables, one year at a time
# ============================================================

# Linear discount (LD) calculation for a single target year.
# Linearly increasing weight (0.0025 for the oldest conversion year up to
# 0.0975 for the most recent). LD_<year> sums that year's conversion and each
# of the prior 19 years' emissions, weighted by how recently each happened.
calculate_LD_for_year <- function(crop_data, target_year, start_year) {
  LD_weights <- seq(0.0025, 0.0975, by = 0.005)
  ld_col_name <- paste0("LD_", target_year)
  crop_data[[ld_col_name]] <- 0
  
  for (i in 1:20) {
    emission_year <- start_year + i - 1
    emission_col_name <- paste0("SOYB_", emission_year)
    if (emission_col_name %in% names(crop_data)) {
      crop_data[[ld_col_name]] <- crop_data[[ld_col_name]] +
        crop_data[[emission_col_name]] * LD_weights[i]
    }
  }
  return(crop_data)
}

# Pull one gas's per-year emission columns out of the raw emissions table,
# attach admin boundaries, and fill in any missing 2001-2024 year with 0 so
# every gas table has the same shape before the LD step.
extract_gas_emissions_for_year <- function(gas_type, emission_data, base_info, soy_attribute_table) {
  if (gas_type == "CO2e") {
    pattern <- "gfw_forest_carbon_gross_emissions_all_gases_\\d{4}__Mg_CO2e$"
  } else if (gas_type == "CO2") {
    pattern <- "gfw_forest_carbon_gross_emissions_CO2_\\d{4}__Mg_CO2e$"
  } else {
    pattern <- paste0("gfw_forest_carbon_gross_emissions_", gas_type, "_\\d{4}__Mg_CO2e$")
  }
  
  emission_cols <- names(emission_data)[grepl(pattern, names(emission_data))]
  if (length(emission_cols) == 0) {
    stop("No ", gas_type, " columns found in emission data")
  }
  
  gas_emissions <- emission_data %>%
    select(feature__id, all_of(emission_cols))
  
  extracted_years <- str_extract(emission_cols, "\\d{4}")
  new_col_names <- paste0("SOYB_", extracted_years)
  if (any(duplicated(new_col_names))) {
    stop("Duplicate years found: ", paste(new_col_names[duplicated(new_col_names)], collapse = ", "))
  }
  names(gas_emissions) <- c("Key", new_col_names)
  
  gas_data <- base_info %>%
    left_join(gas_emissions, by = "Key")
  
  gas_data <- soy_attribute_table %>%
    inner_join(gas_data, by = "Key")
  
  # Ensure every year 2001-2024 has a column, even if this file didn't have it
  all_years <- 2001:2024
  expected_cols <- paste0("SOYB_", all_years)
  for (col in expected_cols) {
    if (!col %in% names(gas_data)) {
      gas_data[[col]] <- 0
    }
  }
  
  final_cols <- c("Key", "GID_0", "GID_1", "GID_2", "Country_Code", "area__ha", expected_cols)
  gas_data %>%
    select(all_of(final_cols)) %>%
    replace(is.na(.), 0)
}

# Sum a gas's LD table up to admin2, then roll that up to admin1 and admin0
aggregate_admin_levels <- function(gas_ld_data) {
  admin2 <- gas_ld_data %>%
    group_by(GID_0, GID_1, GID_2) %>%
    summarise(across(c(area__ha, starts_with("SOYB_"), starts_with("LD_")), sum, na.rm = TRUE),
              pixel_count = n(), .groups = "drop")
  
  admin1 <- admin2 %>%
    group_by(GID_0, GID_1) %>%
    summarise(across(c(area__ha, pixel_count, starts_with("SOYB_"), starts_with("LD_")), sum, na.rm = TRUE),
              .groups = "drop")
  
  admin0 <- admin1 %>%
    group_by(GID_0) %>%
    summarise(across(c(area__ha, pixel_count, starts_with("SOYB_"), starts_with("LD_")), sum, na.rm = TRUE),
              .groups = "drop")
  
  list(admin2 = admin2, admin1 = admin1, admin0 = admin0)
}

# Full Part 1 pipeline for one year: load that year's boundaries + raw
# emissions, extract all four gases, compute LD, aggregate to all three
# admin levels, and write SOYB_<year>_<gas>_LD_<level>.csv (12 files).
process_soy_year <- function(year) {
  cat("=== Processing year", year, "===\n")
  emission_start_year <- year - 19
  
  # Step 1: admin boundaries for this year
  soy_shapefile_path <- file.path(data_dir, paste0("soy_", year, "_intersect"), paste0("soy_", year, "_intersect.dbf"))
  if (!file.exists(soy_shapefile_path)) {
    cat("  Soy shapefile not found, skipping:", soy_shapefile_path, "\n")
    return(invisible(NULL))
  }
  
  soy_attribute_table <- read.dbf(soy_shapefile_path) %>%
    select(Key, GID_0, GID_1, GID_2, Country = NAME_0) %>%
    mutate(
      Country_Code = case_when(
        grepl("Brazil|Brasil", Country, ignore.case = TRUE) ~ "BRA",
        grepl("Argentina", Country, ignore.case = TRUE) ~ "ARG",
        grepl("Paraguay", Country, ignore.case = TRUE) ~ "PRY",
        grepl("Bolivia", Country, ignore.case = TRUE) ~ "BOL",
        grepl("Uruguay", Country, ignore.case = TRUE) ~ "URY",
        TRUE ~ substr(Country, 1, 3)
      )
    ) %>%
    select(Key, GID_0, GID_1, GID_2, Country_Code)
  
  cat("  Boundaries loaded:", nrow(soy_attribute_table), "features\n")
  
  # Step 2: raw emissions for this year (tab-delimited, quoted column names)
  emission_file <- file.path(data_dir, paste0("GHG_emissions_soy_", year, ".csv"))
  if (!file.exists(emission_file)) {
    cat("  Emission file not found, skipping:", emission_file, "\n")
    return(invisible(NULL))
  }
  
  emission_raw <- read.table(emission_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = "")
  names(emission_raw) <- gsub('^"|"$', "", names(emission_raw))
  cat("  Emissions loaded:", nrow(emission_raw), "rows\n")
  
  base_info <- emission_raw %>%
    select(feature__id, area__ha) %>%
    rename(Key = feature__id)
  
  # Step 3-4: extract each gas and compute its LD_<year> column
  gas_tables <- list()
  for (gas in GAS_TYPES) {
    gas_data <- extract_gas_emissions_for_year(gas, emission_raw, base_info, soy_attribute_table)
    gas_tables[[gas]] <- calculate_LD_for_year(gas_data, year, emission_start_year)
  }
  
  # Step 5-6: aggregate to admin0/1/2 and write one file per gas x level
  for (gas in GAS_TYPES) {
    aggregated <- aggregate_admin_levels(gas_tables[[gas]])
    for (level in c("admin0", "admin1", "admin2")) {
      out_file <- file.path(data_dir, paste0("SOYB_", year, "_", gas, "_LD_", level, ".csv"))
      write.csv(aggregated[[level]], out_file, row.names = FALSE)
    }
  }
  
  cat("  Saved LD tables for", year, "(admin0/1/2 x", length(GAS_TYPES), "gases)\n")
}

for (year in PROCESSING_YEARS) {
  process_soy_year(year)
}


# ============================================================
# PART 2: Combine yield + LD emissions tables into emission factors
# ============================================================

# Load yield data ----
yield_file <- file.path(data_dir, "yield_area_gadm2_spam20v2.csv")
yield_data <- read.csv(yield_file, stringsAsFactors = FALSE)

# Keep only soy; standardize GID columns and fill in missing admin1/admin2
# codes with "ALL" so the same rows can be aggregated up to admin0/admin1 later.
yield_clean <- yield_data %>%
  filter(commodity == "SOYB") %>%
  mutate(
    GID_0 = trimws(as.character(GID_0)),
    GID_1 = trimws(as.character(GID_1)),
    GID_2 = trimws(as.character(GID_2)),
    GID_1 = ifelse(is.na(GID_1) | GID_1 == "", "ALL", GID_1),
    GID_2 = ifelse(is.na(GID_2) | GID_2 == "", "ALL", GID_2),
    yield_mt_ha = ifelse(is.na(yield_mt_ha), 0, yield_mt_ha)
  ) %>%
  filter(yield_mt_ha > 0)

cat("Yield data loaded:", nrow(yield_clean), "regions\n")

# Load one gas's LD table for a single year and admin level (Part 1's output) ----
load_gas_year <- function(gas, year, admin_level) {
  file_path <- file.path(data_dir, paste0("SOYB_", year, "_", gas, "_LD_", admin_level, ".csv"))
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  read.csv(file_path, stringsAsFactors = FALSE) %>%
    mutate(
      GID_0 = trimws(as.character(GID_0)),
      GID_1 = if ("GID_1" %in% names(.)) trimws(as.character(GID_1)) else "ALL",
      GID_2 = if ("GID_2" %in% names(.)) trimws(as.character(GID_2)) else "ALL",
      GID_1 = ifelse(is.na(GID_1) | GID_1 == "", "ALL", GID_1),
      GID_2 = ifelse(is.na(GID_2) | GID_2 == "", "ALL", GID_2),
      across(starts_with("LD_"), ~ ifelse(is.na(.), 0, .)),
      area__ha = ifelse(is.na(area__ha), 0, area__ha)
    )
}

# Because Part 1 wrote one file per year (not one file with all years), stitch a single gas's per-year LD_<year> files back
# together into one wide table: GID columns + area__ha + LD_<year> for every
# year that was found.
build_gas_table <- function(gas, admin_level, join_by) {
  year_tables <- PROCESSING_YEARS %>%
    map(~ load_gas_year(gas, .x, admin_level)) %>%
    compact()
  
  if (length(year_tables) == 0) {
    cat("No LD files found for", gas, admin_level, "\n")
    return(NULL)
  }
  
  # Base columns (area, pixel_count) come from the first available year
  gas_table <- year_tables[[1]] %>% select(all_of(join_by), area__ha, pixel_count)
  
  for (yr_data in year_tables) {
    ld_col <- names(yr_data)[grepl("^LD_", names(yr_data))]
    new_name <- paste0(ld_col, "_", gas)
    yr_subset <- yr_data %>%
      select(all_of(join_by), all_of(ld_col)) %>%
      rename(!!new_name := !!ld_col)
    gas_table <- gas_table %>% full_join(yr_subset, by = join_by)
  }
  
  gas_table
}

# Process one admin level: build all four gas tables, join yield, compute EF
process_admin <- function(admin_level) {
  cat("\n--- Processing", admin_level, "---\n")
  
  join_by <- switch(admin_level,
                    admin0 = "GID_0",
                    admin1 = c("GID_0", "GID_1"),
                    admin2 = c("GID_0", "GID_1", "GID_2")
  )
  
  # Use CO2e as the base table (it carries area__ha and pixel_count)
  result <- build_gas_table("CO2e", admin_level, join_by)
  if (is.null(result)) return(NULL)
  
  # Merge in the other three gases (already named LD_<year>_<gas>)
  for (gas in c("CO2", "CH4", "N2O")) {
    gas_table <- build_gas_table(gas, admin_level, join_by)
    if (!is.null(gas_table)) {
      ld_cols <- names(gas_table)[grepl("^LD_", names(gas_table))]
      result <- result %>%
        left_join(gas_table[, c(join_by, ld_cols)], by = join_by)
    }
  }
  
  # Aggregate the (admin2-resolution) yield table up to the requested admin
  # level. Admin0/admin1 rows get GID_1/GID_2 (or just GID_2) set to "ALL" so
  # the join keys line up with the gas tables at that level.
  if (admin_level == "admin0") {
    agg_yield <- yield_clean %>%
      group_by(GID_0) %>%
      summarise(yield_mt_ha = mean(yield_mt_ha, na.rm = TRUE), .groups = "drop") %>%
      mutate(GID_1 = "ALL", GID_2 = "ALL")
  } else if (admin_level == "admin1") {
    agg_yield <- yield_clean %>%
      group_by(GID_0, GID_1) %>%
      summarise(yield_mt_ha = mean(yield_mt_ha, na.rm = TRUE), .groups = "drop") %>%
      mutate(GID_2 = "ALL")
  } else {
    agg_yield <- yield_clean
  }
  
  result <- result %>%
    inner_join(agg_yield, by = join_by)
  
  cat("After yield join:", nrow(result), "regions\n")
  if (nrow(result) == 0) return(NULL)
  
  # Production (tonnes) = yield (tonnes/ha) x area (ha).
  # Area and yield are static across years, so production is one column,
  # not one per year.
  result$production <- result$yield_mt_ha * result$area__ha
  
  # Emission factor (EF) = LD emissions / production, per year and gas.
  # EF_<year>_<gas> = LD_<year>_<gas> / production
  for (year in PROCESSING_YEARS) {
    for (gas in GAS_TYPES) {
      ld_col <- paste0("LD_", year, "_", gas)
      ef_col <- paste0("EF_", year, "_", gas)
      if (ld_col %in% names(result)) {
        result[[ef_col]] <- ifelse(result$production > 0,
                                   result[[ld_col]] / result$production,
                                   NA)
      }
    }
  }
  
  cat("Final result:", nrow(result), "regions with", ncol(result), "columns\n")
  result
}

# Run for all three admin levels and save ----
results <- list()
for (level in c("admin0", "admin1", "admin2")) {
  results[[level]] <- process_admin(level)
}

for (level in names(results)) {
  if (!is.null(results[[level]])) {
    output_file <- file.path(data_dir, paste0("SOY_EF_", level, "_final.csv"))
    write.csv(results[[level]], output_file, row.names = FALSE)
    cat("Saved", level, "to", basename(output_file), "\n")
    
    # Spot-check the 2023 CO2e emission factor range for plausibility
    data <- results[[level]]
    if ("EF_2023_CO2e" %in% names(data)) {
      ef_vals <- data$EF_2023_CO2e[!is.na(data$EF_2023_CO2e) & is.finite(data$EF_2023_CO2e)]
      if (length(ef_vals) > 0) {
        cat("  EF_2023_CO2e range:", round(min(ef_vals), 3), "-", round(max(ef_vals), 3), "\n")
      }
    }
  }
}