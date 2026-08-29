

library(tidyverse)

# ============================================================
# Cocoa emission factor (EF) calculation for Cote d'Ivoire and Ghana
#
# Combines:
#   - a yield table (tonnes/ha by admin region)
#   - linearly discounted ("LD") emissions tables, one per gas (CO2e/CO2/CH4/N2O)
# to compute production and LUC emission factors (tonnes gas per tonne of
# cocoa produced) for admin0, admin1, and admin2 levels, for 2020-2024.
# ============================================================

# ---- User settings ----
# Set this to the folder containing your input CSVs (yield_area_gadm2_spam20v2.csv
# and the COCO_<gas>_LD_<admin_level>.csv files) and where the output should be saved.
# Example (Windows): "C:/Users/<your_username>/Downloads"
# Example (Mac/Linux): "/Users/<your_username>/Downloads"
data_dir <- "path/to/your/data/folder"

# Load yield data ----
yield_file <- file.path(data_dir, "yield_area_gadm2_spam20v2.csv")
yield_data <- read.csv(yield_file, stringsAsFactors = FALSE)

# Keep only cocoa (COCO) in Cote d'Ivoire (CIV) and Ghana (GHA);
# standardize GID columns and fill in missing admin1/admin2 codes with "ALL"
# so the same rows can be aggregated up to admin0/admin1 later.
yield_clean <- yield_data %>%
  filter(commodity == "COCO", GID_0 %in% c("CIV", "GHA")) %>%
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

# Load one gas's LD (linearly discounted emissions) table for a given admin level ----
# gas: "CO2e", "CO2", "CH4", or "N2O"
# admin_level: "admin0", "admin1", or "admin2"
load_gas_data <- function(gas, admin_level) {
  file_path <- file.path(data_dir, paste0("COCO_", gas, "_LD_", admin_level, ".csv"))
  
  if (!file.exists(file_path)) {
    cat("File not found:", basename(file_path), "\n")
    return(NULL)
  }
  
  data <- read.csv(file_path, stringsAsFactors = FALSE) %>%
    filter(GID_0 %in% c("CIV", "GHA")) %>%
    mutate(
      GID_0 = trimws(as.character(GID_0)),
      # Lower admin levels won't have a GID_1/GID_2 column at all, so default to "ALL"
      GID_1 = if ("GID_1" %in% names(.)) trimws(as.character(GID_1)) else "ALL",
      GID_2 = if ("GID_2" %in% names(.)) trimws(as.character(GID_2)) else "ALL",
      GID_1 = ifelse(is.na(GID_1) | GID_1 == "", "ALL", GID_1),
      GID_2 = ifelse(is.na(GID_2) | GID_2 == "", "ALL", GID_2),
      across(starts_with("LD_"), ~ ifelse(is.na(.), 0, .))
    )
  
  # Some files may name the area column differently — fall back to the first column with "area" in its name.
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
  return(data)
}

# Process one admin level: join yield + all four gases, compute EF per year ----
process_admin <- function(admin_level) {
  cat("\n--- Processing", admin_level, "---\n")
  
  # Aggregate the (admin2-resolution) yield table up to the requested admin level.
  # Admin0/admin1 rows get GID_1/GID_2 (or just GID_2) set to "ALL" so the join
  # keys line up with the gas tables at that level.
  if (admin_level == "admin0") {
    agg_yield <- yield_clean %>%
      group_by(GID_0) %>%
      summarise(yield_mt_ha = mean(yield_mt_ha, na.rm = TRUE), .groups = "drop") %>%
      mutate(GID_1 = "ALL", GID_2 = "ALL")
    join_by <- "GID_0"
  } else if (admin_level == "admin1") {
    agg_yield <- yield_clean %>%
      group_by(GID_0, GID_1) %>%
      summarise(yield_mt_ha = mean(yield_mt_ha, na.rm = TRUE), .groups = "drop") %>%
      mutate(GID_2 = "ALL")
    join_by <- c("GID_0", "GID_1")
  } else {
    agg_yield <- yield_clean
    join_by <- c("GID_0", "GID_1", "GID_2")
  }
  
  # Use CO2e as the base table (it carries area__ha, used for production below)
  base_data <- load_gas_data("CO2e", admin_level)
  if (is.null(base_data)) return(NULL)
  
  result <- base_data %>%
    inner_join(agg_yield, by = join_by)
  
  cat("After yield join:", nrow(result), "regions\n")
  if (nrow(result) == 0) return(NULL)
  
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
  
  # Bring in the other three gases and rename their LD_<year> columns the same way
  for (gas in c("CO2", "CH4", "N2O")) {
    gas_data <- load_gas_data(gas, admin_level)
    if (!is.null(gas_data)) {
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
  
  # jdLUC emission factor (EF) = land-use-change emissions / production, per year and gas.
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
    output_file <- file.path(data_dir, paste0("COCO_EF_", level, "_final.csv"))
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
    
    # Confirm there's a single production column (not one per year)
    prod_cols <- names(data)[grepl("^production", names(data))]
    cat("  Production columns:", length(prod_cols), "(was 5, now", length(prod_cols), ")\n")
  }
}
