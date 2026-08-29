library(tidyverse)
library(foreign)

# ============================================================
# Oil palm (OILP) jdLUC emission factor (EF) calculation
#
# Part 1: build per-gas, per-year "linearly discounted" (LD) land use change
#         emissions tables at admin0/admin1/admin2 level, from a raw
#         carbon flux model (geotrellis tool) emissions CSV joined to admin boundaries.
# Part 2: combine those LD tables with a yield table to compute production
#         and emission factors (tonnes gas per tonne of oil palm produced).
#
# Part 2 depends on Part 1's output files, so run this script top to bottom.
# ============================================================

# ---- User settings ----
# Set this to the folder containing your input files (oilp_jdLUC_raw_emissions.csv,
# palm_erase_intersect_250813.dbf, yield_area_gadm2_spam20v2.csv) and where all
# intermediate and final outputs should be written.
# Example (Windows): "C:/Users/<your_username>/Downloads"
# Example (Mac/Linux): "/Users/<your_username>/Downloads"
data_dir <- "path/to/your/data/folder"

# ============================================================
# PART 1: Build LD emissions tables from raw geotrellis output
# ============================================================

# The raw export has each line wrapped in a stray pair of quotes, which
# breaks a normal tab-delimited read. Strip the leading/trailing quote from
# every line, write the result to a temp file, then read that.
raw_lines <- readLines(file.path(data_dir, "oilp_jdLUC_raw_emissions.csv"))

clean_lines <- gsub('^"(.*)$', '\\1', raw_lines)   # Remove leading quote
clean_lines <- gsub('(.*)"$', '\\1', clean_lines)  # Remove trailing quote

temp_file <- tempfile(fileext = ".tsv")
writeLines(clean_lines, temp_file)

geotrellis <- read.table(temp_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = "")

unlink(temp_file)

# Admin boundaries (GID_0/1/2) keyed by the same pixel/feature id as the carbon flux model (geotrellis tool) output
boundaries <- read.dbf(file.path(data_dir, "palm_erase_intersect_250813.dbf"))

cat("CSV data:", nrow(geotrellis), "rows x", ncol(geotrellis), "columns\n")
cat("DBF data:", nrow(boundaries), "rows x", ncol(boundaries), "columns\n")

# Attach GID_0/1/2 to every geotrellis row (boundaries$key == geotrellis$feature__id)
palm_data <- boundaries %>%
  select(key, GID_0, GID_1, GID_2) %>%
  inner_join(geotrellis, by = c("key" = "feature__id"))

cat("Joined data:", nrow(palm_data), "rows\n")

# Identify the per-year emission columns for each gas by column-name pattern.

all_cols <- names(palm_data)
co2e_cols <- all_cols[grepl("all_gases_20[0-9][0-9]__Mg_CO2e$", all_cols)]
co2_cols <- all_cols[grepl("emissions_CO2_20[0-9][0-9]__Mg_CO2e$", all_cols)]
ch4_cols <- all_cols[grepl("emissions_CH4_20[0-9][0-9]__Mg_CO2e$", all_cols)]
n2o_cols <- all_cols[grepl("emissions_N2O_20[0-9][0-9]__Mg_CO2e$", all_cols)]

cat("Found emission columns:\n")
cat("CO2e:", length(co2e_cols), "\n")
cat("CO2:", length(co2_cols), "\n")
cat("CH4:", length(ch4_cols), "\n")
cat("N2O:", length(n2o_cols), "\n")

# Split into one table per gas, keeping the shared ID/area columns plus that
# gas's year columns
base_cols <- c("key", "GID_0", "GID_1", "GID_2", "area__ha")

OILP_CO2e <- palm_data %>% select(all_of(c(base_cols, co2e_cols)))
OILP_CO2 <- palm_data %>% select(all_of(c(base_cols, co2_cols)))
OILP_CH4 <- palm_data %>% select(all_of(c(base_cols, ch4_cols)))
OILP_N2O <- palm_data %>% select(all_of(c(base_cols, n2o_cols)))

# Rename each gas's messy year columns to a plain "OILP_<year>" so downstream
# code can refer to them consistently across gases
rename_emissions <- function(data, emission_cols) {
  new_names <- names(data)
  for (col in emission_cols) {
    year <- regmatches(col, regexpr("20[0-9][0-9]", col))
    if (length(year) > 0) {
      new_names[which(names(data) == col)] <- paste0("OILP_", year)
    }
  }
  names(data) <- new_names
  return(data)
}

OILP_CO2e <- rename_emissions(OILP_CO2e, co2e_cols)
OILP_CO2 <- rename_emissions(OILP_CO2, co2_cols)
OILP_CH4 <- rename_emissions(OILP_CH4, ch4_cols)
OILP_N2O <- rename_emissions(OILP_N2O, n2o_cols)

# Linear discounting (LD) calculation.
# Linearly increasing weight (0.0025 for the oldest conversion year up
# to 0.0975 for the most recent). For each target year, LD_<year> sums the
# emissions from that year's conversion and each of the prior 19 years,
# weighted by how recently the conversion happened.
calc_LD <- function(data, start_year, end_year) {
  LD <- seq(0.0025, 0.0975, by = 0.005)
  
  for (year in start_year:end_year) {
    col_name <- paste0("LD_", year)
    data[[col_name]] <- 0
    
    for (j in 1:20) {
      crop_col <- paste0("OILP_", year - j + 1)
      if (crop_col %in% names(data)) {
        data[[col_name]] <- data[[col_name]] + data[[crop_col]] * LD[20 - j + 1]
      }
    }
  }
  return(data)
}

# Calculate LD for each gas, for 2020-2024
OILP_CO2e_LD <- calc_LD(OILP_CO2e, 2020, 2024)
OILP_CO2_LD <- calc_LD(OILP_CO2, 2020, 2024)
OILP_CH4_LD <- calc_LD(OILP_CH4, 2020, 2024)
OILP_N2O_LD <- calc_LD(OILP_N2O, 2020, 2024)

# Sum pixel-level values up to admin0/admin1/admin2
agg_admin <- function(data, level) {
  if (level == "admin0") {
    data %>%
      group_by(GID_0) %>%
      summarise(across(c(area__ha, starts_with("OILP_"), starts_with("LD_")), sum, na.rm = TRUE),
                pixel_count = n(), .groups = "drop")
  } else if (level == "admin1") {
    data %>%
      group_by(GID_0, GID_1) %>%
      summarise(across(c(area__ha, starts_with("OILP_"), starts_with("LD_")), sum, na.rm = TRUE),
                pixel_count = n(), .groups = "drop")
  } else {
    data %>%
      group_by(GID_0, GID_1, GID_2) %>%
      summarise(across(c(area__ha, starts_with("OILP_"), starts_with("LD_")), sum, na.rm = TRUE),
                pixel_count = n(), .groups = "drop")
  }
}

# Write one CSV per gas x admin level (12 files): OILP_<gas>_LD_<level>.csv
gases <- list(CO2e = OILP_CO2e_LD, CO2 = OILP_CO2_LD, CH4 = OILP_CH4_LD, N2O = OILP_N2O_LD)
levels <- c("admin0", "admin1", "admin2")

for (gas_name in names(gases)) {
  for (level in levels) {
    result <- agg_admin(gases[[gas_name]], level)
    filename <- file.path(data_dir, paste0("OILP_", gas_name, "_LD_", level, ".csv"))
    write.csv(result, filename, row.names = FALSE)
  }
}

cat("Generated", length(gases) * length(levels), "files\n")

# Diagnostic: confirm which raw columns actually matched "CO2" (sanity check
# on the regex patterns above)
# co2_patterns <- names(palm_data)[grepl("CO2", names(palm_data))]
# print(co2_patterns)


# ============================================================
# PART 2: Combine yield + LD emissions tables into LUC emission factors
# ============================================================

# Load yield data ----
yield_file <- file.path(data_dir, "yield_area_gadm2_spam20v2.csv")
yield_data <- read.csv(yield_file, stringsAsFactors = FALSE)

# Keep only oil palm; standardize GID columns and fill in missing admin1/admin2
# codes with "ALL" so the same rows can be aggregated up to admin0/admin1 later.
yield_clean <- yield_data %>%
  filter(commodity == "OILP") %>%
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
cat("Countries in yield data:", paste(unique(yield_clean$GID_0), collapse = ", "), "\n")

# Load one gas's LD table (written by Part 1) for a given admin level ----
# gas: "CO2e", "CO2", "CH4", or "N2O"
# admin_level: "admin0", "admin1", or "admin2"
load_gas_data <- function(gas, admin_level) {
  file_path <- file.path(data_dir, paste0("OILP_", gas, "_LD_", admin_level, ".csv"))
  
  if (!file.exists(file_path)) {
    cat("File not found:", basename(file_path), "\n")
    return(NULL)
  }
  
  data <- read.csv(file_path, stringsAsFactors = FALSE) %>%
    mutate(
      GID_0 = trimws(as.character(GID_0)),
      # Lower admin levels won't have a GID_1/GID_2 column at all, so default to "ALL"
      GID_1 = if ("GID_1" %in% names(.)) trimws(as.character(GID_1)) else "ALL",
      GID_2 = if ("GID_2" %in% names(.)) trimws(as.character(GID_2)) else "ALL",
      GID_1 = ifelse(is.na(GID_1) | GID_1 == "", "ALL", GID_1),
      GID_2 = ifelse(is.na(GID_2) | GID_2 == "", "ALL", GID_2),
      across(starts_with("LD_"), ~ ifelse(is.na(.), 0, .))
    )
  
  # Some files may name the area column differently (e.g. "area_ha" instead
  # of "area__ha") — fall back to the first column with "area" in its name.
  if (!"area__ha" %in% names(data)) {
    area_cols <- names(data)[grepl("area", names(data), ignore.case = TRUE)]
    if (length(area_cols) > 0) {
      data$area__ha <- data[[area_cols[1]]]
    } else {
      cat("No area found for", gas, admin_level, "\n")
      return(NULL)
    }
  }
  
  data$area__ha <- ifelse(is.na(data$area__ha), 0, data$area__ha)
  data <- data %>% filter(area__ha > 0)
  
  cat("Loaded", gas, admin_level, ":", nrow(data), "regions\n")
  cat("  Countries:", paste(sort(unique(data$GID_0)), collapse = ", "), "\n")
  return(data)
}

# Process one admin level: join yield + all four gases, compute EF per year ----
process_admin <- function(admin_level) {
  cat("\n--- Processing", admin_level, "---\n")
  
  # Load CO2e first to see which countries actually have emission data
  base_data <- load_gas_data("CO2e", admin_level)
  if (is.null(base_data)) return(NULL)
  
  # The emissions and yield tables may not cover exactly the same countries
  # (e.g. one has data for a country the other doesn't) — restrict to the
  # overlap so joins below don't silently drop rows.
  available_countries <- unique(base_data$GID_0)
  yield_countries <- unique(yield_clean$GID_0)
  common_countries <- intersect(available_countries, yield_countries)
  
  cat("Available countries in emission data:", paste(available_countries, collapse = ", "), "\n")
  cat("Available countries in yield data:", paste(yield_countries, collapse = ", "), "\n")
  cat("Common countries:", paste(common_countries, collapse = ", "), "\n")
  
  if (length(common_countries) == 0) {
    cat("No common countries found - cannot proceed\n")
    return(NULL)
  }
  
  base_data <- base_data %>% filter(GID_0 %in% common_countries)
  yield_subset <- yield_clean %>% filter(GID_0 %in% common_countries)
  
  # Aggregate the (admin2-resolution) yield table up to the requested admin
  # level. Admin0/admin1 rows get GID_1/GID_2 (or just GID_2) set to "ALL" so
  # the join keys line up with the gas tables at that level.
  if (admin_level == "admin0") {
    agg_yield <- yield_subset %>%
      group_by(GID_0) %>%
      summarise(yield_mt_ha = mean(yield_mt_ha, na.rm = TRUE), .groups = "drop") %>%
      mutate(GID_1 = "ALL", GID_2 = "ALL")
    join_by <- "GID_0"
  } else if (admin_level == "admin1") {
    agg_yield <- yield_subset %>%
      group_by(GID_0, GID_1) %>%
      summarise(yield_mt_ha = mean(yield_mt_ha, na.rm = TRUE), .groups = "drop") %>%
      mutate(GID_2 = "ALL")
    join_by <- c("GID_0", "GID_1")
  } else {
    agg_yield <- yield_subset
    join_by <- c("GID_0", "GID_1", "GID_2")
  }
  
  result <- base_data %>%
    inner_join(agg_yield, by = join_by)
  
  cat("After yield join:", nrow(result), "regions\n")
  if (nrow(result) == 0) {
    cat("No regions after joining - check if GID codes match between datasets\n")
    return(NULL)
  }
  
  # Production (tonnes) = yield (tonnes/ha) x area (ha).
  # Area and yield are static across years, so production is one column,
  # not one per year.
  result$production <- result$yield_mt_ha * result$area__ha
  
  # Rename CO2e's LD_<year> columns to LD_<year>_CO2e before merging in the
  # other gases, so each gas keeps distinct columns.
  ld_cols <- names(result)[grepl("^LD_", names(result))]
  for (col in ld_cols) {
    new_name <- paste0(col, "_CO2e")
    result <- result %>% rename(!!new_name := !!col)
  }
  
  # Bring in the other three gases (also restricted to common_countries) and
  # rename their LD_<year> columns the same way
  for (gas in c("CO2", "CH4", "N2O")) {
    gas_data <- load_gas_data(gas, admin_level)
    if (!is.null(gas_data)) {
      gas_data <- gas_data %>% filter(GID_0 %in% common_countries)
      
      ld_cols <- names(gas_data)[grepl("^LD_", names(gas_data))]
      join_cols <- c(join_by, ld_cols)
      gas_subset <- gas_data[, join_cols]
      
      for (col in ld_cols) {
        new_name <- paste0(col, "_", gas)
        gas_subset <- gas_subset %>% rename(!!new_name := !!col)
      }
      
      result <- result %>%
        left_join(gas_subset, by = join_by)
      
      cat("Added", gas, "data\n")
    }
  }
  
  # Emission factor (EF) = LD emissions / production, per year and gas.
  # EF_<year>_<gas> = LD_<year>_<gas> / production
  years <- c("2020", "2021", "2022", "2023", "2024")
  gases <- c("CO2e", "CO2", "CH4", "N2O")
  
  for (year in years) {
    for (gas in gases) {
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
  return(result)
}

# Run for all three admin levels ----
results <- list()
for (level in c("admin0", "admin1", "admin2")) {
  results[[level]] <- process_admin(level)
}

# Save one output CSV per admin level, plus a quick sanity check of the results ----
for (level in names(results)) {
  if (!is.null(results[[level]])) {
    output_file <- file.path(data_dir, paste0("OILP_EF_", level, "_final.csv"))
    write.csv(results[[level]], output_file, row.names = FALSE)
    cat("Saved", level, "to", basename(output_file), "\n")
    
    # Spot-check the 2023 CO2e emission factor range/mean for plausibility
    data <- results[[level]]
    if ("EF_2023_CO2e" %in% names(data)) {
      ef_vals <- data$EF_2023_CO2e[!is.na(data$EF_2023_CO2e) & is.finite(data$EF_2023_CO2e)]
      if (length(ef_vals) > 0) {
        cat("  EF_2023_CO2e range:", round(min(ef_vals), 3), "-", round(max(ef_vals), 3), "\n")
        cat("  Mean EF_2023_CO2e:", round(mean(ef_vals), 3), "\n")
      }
    }
    
    cat("  Countries included:", paste(sort(unique(data$GID_0)), collapse = ", "), "\n")
    cat("  Total production (mt):", round(sum(data$production, na.rm = TRUE), 0), "\n")
    cat("  Average yield (mt/ha):", round(mean(data$yield_mt_ha, na.rm = TRUE), 3), "\n")
  }
}

# Summary of all results
cat("\n=== SUMMARY ===\n")
for (level in names(results)) {
  if (!is.null(results[[level]])) {
    cat(level, ": ", nrow(results[[level]]), " regions processed\n")
  } else {
    cat(level, ": No data processed\n")
  }
}