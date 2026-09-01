library(foreign)
library(tidyverse)
library(data.table)
library(randomForest)

# ============================================================
# SLUC (spatially-explicit land-use-change) emission factors, by crop and year
#
# 1. Non-allocated emissions: read the geotrellis tree-cover-loss/emissions
#    output for the MapSPAM 2020v2 grid and join it to GADM boundaries.
# 2. Allocated emissions: split total grid-cell emissions across crops using
#    product-expansion allocation factors (PAF), then compute the 20-year
#    linear-discounted (LD) emissions for each crop and gas.
# 3. Production: build a production-by-year table per crop/grid-cell by
#    projecting MapSPAM 2020 production forward using FAOSTAT growth trends
#    (with a random-forest estimate for the most recent year, which FAOSTAT
#    doesn't have yet).
# 4. EF calculation: EF = LD / production, aggregated to admin0/1/2, for all
#    four gases (CO2e/CO2/CH4/N2O).
#
# Run top to bottom — later sections depend on files written earlier in the
# same run (Section 2's LD tables feed Section 4; Section 3's production
# table feeds Section 4).
# ============================================================

# ---- User settings ----
# Root folder containing the project's subfolders (geospatial_data/,
# created_tables/, FAO_Stat/, results/, emission_factors/). All paths below
# are built relative to this.
data_dir <- "path/to/your/project/folder"

years     <- 2020:2024   # years to compute LD/EF for
base_year <- min(years)  # the MapSPAM production year used as the anchor (2020)
current_year <- max(years) # most recent year — FAOSTAT lags, so this one is predicted (see Section 3)

results_dir <- file.path(data_dir, "results")           # per-crop LD tables (Section 2 writes, Section 4 reads)
ef_dir      <- file.path(data_dir, "emission_factors")   # final EF tables (Section 4 writes)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(ef_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# Shared functions
# ============================================================

# Allocate a gas's yearly grid-cell emissions to one crop, using that crop's
# product-expansion allocation factor (PAF) for the year's time window:
#   2001-2005 -> PAF0005, 2006-2010 -> PAF0510, 2011-2024 -> PAF1020
# `gas` is the emission column's gas fragment: "all_gases" for CO2e, or
# "CO2"/"CH4"/"N2O". All four gases end up in the same "allocated_emissions_
# <year>" column since one gas is processed (and consumed by calculate_LD)
# at a time.
allocate_emissions <- function(data, gas) {
  emission_cols <- grep(paste0("^gfw_forest_carbon_gross_emissions_", gas, "_"), names(data), value = TRUE)
  
  for (col_name in emission_cols) {
    year <- str_extract(col_name, "\\d{4}(?=__Mg)")
    
    paf_col <- case_when(
      is.na(year) ~ NA_character_,
      as.numeric(year) %in% 2001:2005 ~ "PAF0005",
      as.numeric(year) %in% 2006:2010 ~ "PAF0510",
      as.numeric(year) %in% 2011:2024 ~ "PAF1020",
      TRUE ~ NA_character_
    )
    
    if (is.na(paf_col)) {
      message("No PAF column found for year ", year, " in column ", col_name)
      next
    }
    
    data[[paste0("allocated_emissions_", year)]] <- data[[col_name]] * data[[paf_col]]
  }
  data
}

# Linear discount (LD): land converted to a crop keeps "counting" against it
# for 20 years, with a linearly increasing weight (0.0025 for the oldest
# conversion year up to 0.0975 for the most recent). LD_<year> sums that
# year's conversion and each of the prior 19 years' allocated emissions,
# weighted by recency.
calculate_LD <- function(data,
                         start_year = 2001,
                         end_year   = 2024,
                         weights    = seq(0.0025, 0.0975, by = 0.005)) {
  stopifnot(length(weights) == 20)
  
  all_cols  <- grep("^allocated_emissions_\\d{4}$", names(data), value = TRUE)
  all_years <- sort(as.integer(sub("allocated_emissions_", "", all_cols)))
  all_cols  <- paste0("allocated_emissions_", all_years)
  
  X <- as.matrix(data[, all_cols])
  storage.mode(X) <- "double"
  X[is.na(X)] <- 0
  
  ld_years <- (end_year - 4):end_year
  
  for (ld_year in ld_years) {
    window_years <- (ld_year - 19):ld_year
    idx <- match(window_years, all_years)
    
    if (anyNA(idx)) {
      keep <- !is.na(idx)
      if (!any(keep)) {
        data[[paste0("LD_", ld_year)]] <- 0
        next
      }
      data[[paste0("LD_", ld_year)]] <- as.vector(X[, idx[keep], drop = FALSE] %*% weights[keep])
      warning("Missing some years for LD_", ld_year,
              " (have: ", paste(window_years[keep], collapse = ","), ")")
    } else {
      data[[paste0("LD_", ld_year)]] <- as.vector(X[, idx, drop = FALSE] %*% weights)
    }
  }
  data
}

# Read every per-crop LD file for one gas/admin level (written by Section 2),
# join to grid area + production, compute EF = LD / production for every
# year, and write one CSV per crop plus a combined master table.
compute_ef_table <- function(gas, admin_level, production, grid_area, years, results_dir, ef_dir) {
  group_cols <- switch(admin_level,
                       admin0 = "GID_0",
                       admin1 = c("GID_0", "GID_1"),
                       admin2 = c("GID_0", "GID_1", "GID_2")
  )
  admin_tag <- toupper(sub("admin", "ADM", admin_level))
  
  ld_files <- list.files(results_dir, pattern = paste0("^LD_.*_", gas, "\\.csv$"), full.names = TRUE)
  
  ef_all_crops <- list()
  
  for (file in ld_files) {
    crop <- str_extract(basename(file), paste0("(?<=LD_)[A-Z]+(?=_", gas, "\\.csv)"))
    
    ld_data <- read_csv(file, show_col_types = FALSE)
    prod_crop <- production %>% filter(crop_type == crop)
    if (nrow(prod_crop) == 0) next
    
    joined <- ld_data %>%
      inner_join(grid_area, by = "Id") %>%
      inner_join(prod_crop, by = c("CELL5M", "crop_type"))
    if (nrow(joined) == 0) next
    
    # Area-weighted production for each year — a crop's grid-cell production
    # is scaled by the fraction of the grid cell this feature covers
    for (yr in years) {
      joined[[paste0("adj_prod_", yr)]] <-
        (joined$area__ha / joined$Grid_area_ha) * joined[[paste0("prod_mt_", yr)]]
    }
    
    ef_summary <- joined %>%
      group_by(across(all_of(c(group_cols, "crop_type")))) %>%
      summarise(
        across(all_of(paste0("LD_", years)), ~ sum(.x, na.rm = TRUE), .names = "sum_LD_{substr(.col, 4, 100)}"),
        across(all_of(paste0("adj_prod_", years)), ~ sum(.x, na.rm = TRUE), .names = "sum_adj_prod_{substr(.col, 10, 100)}"),
        .groups = "drop"
      )
    
    for (yr in years) {
      ef_summary[[paste0("EF_", yr)]] <- with(
        ef_summary,
        signif(
          ifelse(get(paste0("sum_adj_prod_", yr)) == 0, NA,
                 get(paste0("sum_LD_", yr)) / get(paste0("sum_adj_prod_", yr))),
          4
        )
      )
    }
    
    ef_output <- ef_summary %>%
      select(all_of(group_cols), crop_type, starts_with("sum_LD_"), starts_with("sum_adj_prod_"), starts_with("EF_")) %>%
      filter(if_all(where(is.numeric), is.finite))
    
    names(ef_output) <- names(ef_output) %>%
      str_replace("^sum_LD_", "LD_") %>%
      str_replace("^sum_adj_prod_", "production_")
    
    write_csv(ef_output, file.path(ef_dir, paste0("EF_", admin_tag, "_", crop, "_", gas, ".csv")))
    ef_all_crops[[crop]] <- ef_output
  }
  
  ef_master <- bind_rows(ef_all_crops)
  write_csv(ef_master, file.path(ef_dir, paste0("emission_factors_", gas, "_", admin_tag, "_master.csv")))
  ef_master
}


# ============================================================
# 1. Non-allocated emissions — Geotrellis
# ============================================================
# Reads and formats the non-allocated emissions output from the geotrellis
# tool for the MapSPAM years.

geotrellis_spam20 <- read_tsv(file.path(data_dir, "geospatial_data", "geotrellis",
                                        "treecoverloss_20250722_1957_spam20v2",
                                        "part-00000-a00515f9-afbd-45af-b1a1-5daf866504b5-c000.csv"))

geotrellis20_simpl <- geotrellis_spam20 %>%
  select(feature__id, area__ha, tcl_driver__class,
         gfw_forest_carbon_gross_emissions_all_gases_2001__Mg_CO2e:gfw_forest_carbon_gross_emissions_N2O_2024__Mg_CO2e)

# Keep only permanent agriculture and shifting cultivation as deforestation drivers
geotrellis20_simpl <- geotrellis20_simpl %>%
  filter(tcl_driver__class == "Permanent agriculture" | tcl_driver__class == "Shifting cultivation")

# Sum per grid cell (feature id) to get total emissions per pixel, before
# they were split out by driver
cols_to_sum <- c(
  "area__ha",
  grep("^gfw_forest_carbon_gross_emissions_", names(geotrellis20_simpl), value = TRUE)
)

geotrellis20_simpl <- geotrellis20_simpl %>%
  group_by(feature__id) %>%
  summarise(across(all_of(cols_to_sum), function(x) sum(x, na.rm = TRUE)))

# write_csv(geotrellis20_simpl, file.path(data_dir, "created_tables", "geotrellis_drivers_map20_v2_24.csv"))

dbf_spam2020 <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "spam20v2_gadm2.dbf"))

geotrellis_gadm2020 <- dbf_spam2020 %>%
  inner_join(geotrellis20_simpl, by = c("key" = "feature__id"))


# ============================================================
# 2. Allocated emissions — product expansion
# ============================================================
# Splits each grid cell's emissions across crops using a product-expansion
# allocation factor (PAF), then computes linear-discounted emissions for
# every crop x gas combination.
#
# PAF time windows (used inside allocate_emissions):
#   PAF 2000-2005: years 2001-2005
#   PAF 2005-2010: years 2006-2010
#   PAF 2010-2020: years 2011-2024
#
# (For reference, a separate "shared responsibility" windowing scheme also
# exists — MapSPAM 2000/2005/2010/2020 snapshots covering years 2001-2003 /
# 2004-2008 / 2009-2015 / 2016-2022 respectively — but is not used by the
# allocation step below.)

paf_product_expansion <- fread(file.path(data_dir, "created_tables", "product_expansion_PAF_SUBS_072325.csv")) %>%
  select(Id, CELL5M, crop_type, PAF0005, PAF0510, PAF1020) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Allocating emissions to the 2020 crop footprint
emissions <- geotrellis_gadm2020 %>%
  select(key, Id, grid_code, GID_0, GID_1, GID_2, area__ha,
         gfw_forest_carbon_gross_emissions_all_gases_2001__Mg_CO2e:gfw_forest_carbon_gross_emissions_N2O_2024__Mg_CO2e) %>%
  rename(CELL5M = grid_code) %>%
  as_tibble() # drop the spatial component; only aggregation is needed from here

emissions_by_gas <- list(
  CO2e = emissions %>% select(key, Id, CELL5M, GID_0, GID_1, GID_2, area__ha,
                              gfw_forest_carbon_gross_emissions_all_gases_2001__Mg_CO2e:gfw_forest_carbon_gross_emissions_all_gases_2024__Mg_CO2e),
  CO2  = emissions %>% select(key, Id, CELL5M, GID_0, GID_1, GID_2, area__ha,
                              gfw_forest_carbon_gross_emissions_CO2_2001__Mg_CO2e:gfw_forest_carbon_gross_emissions_CO2_2024__Mg_CO2e),
  CH4  = emissions %>% select(key, Id, CELL5M, GID_0, GID_1, GID_2, area__ha,
                              gfw_forest_carbon_gross_emissions_CH4_2001__Mg_CO2e:gfw_forest_carbon_gross_emissions_CH4_2024__Mg_CO2e),
  N2O  = emissions %>% select(key, Id, CELL5M, GID_0, GID_1, GID_2, area__ha,
                              gfw_forest_carbon_gross_emissions_N2O_2001__Mg_CO2e:gfw_forest_carbon_gross_emissions_N2O_2024__Mg_CO2e)
)

gas_regex <- c(CO2e = "all_gases", CO2 = "CO2", CH4 = "CH4", N2O = "N2O")

crop_types <- unique(paf_product_expansion$crop_type)

for (gas_label in names(gas_regex)) {
  emissions_ratios <- emissions_by_gas[[gas_label]]
  
  for (crop in crop_types) {
    crop_data <- paf_product_expansion %>%
      filter(crop_type == crop) %>%
      inner_join(emissions_ratios, by = c("Id", "CELL5M"))
    
    allocated <- allocate_emissions(crop_data, gas_regex[[gas_label]])
    LD_result <- calculate_LD(as.data.frame(allocated))
    
    filename <- gsub("[^a-zA-Z0-9_]", "_", paste0("LD_", crop, "_", gas_label, ".csv"))
    write_csv(LD_result, file.path(results_dir, filename))
    message("Saved: ", filename)
  }
}


# ============================================================
# 3. Production
# ============================================================
# Production data for the different technologies, in metric tons.

grid_area <- read.dbf(file.path(data_dir, "geospatial_data", "intermediate_layers", "clip_gridEckarea.dbf")) %>%
  select(Id, Area_ha) %>%
  group_by(Id) %>%
  summarise(Grid_area_ha = sum(Area_ha, na.rm = TRUE))

# ---- Production for base_year (2020) ----
ta <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2020_v2",
                         "spam2020V2r0_global_production", "spam2020V2r0_global_P_TA.csv"))
ts <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2020_v2",
                         "spam2020V2r0_global_production", "spam2020V2r0_global_P_TS.csv")) # rainfed subsistence portion

prod20_thli <- ta[, -11] %>% full_join(ts[, -11]) # drop tech-type column (col 11); it blocks the merge

# Subtract the "_S" (subsistence) column from "_A" (all) for each 4-letter
# crop prefix, producing a "_THLI" (total-high/low/irrigated, i.e. non-subsistence) column
subtract_columns_AS <- function(data) {
  col_names <- names(data)
  unique_prefixes <- unique(substr(col_names, 1, 4))
  
  for (prefix in unique_prefixes) {
    col_A <- paste0(prefix, "_A")
    col_S <- paste0(prefix, "_S")
    
    if (all(c(col_A, col_S) %in% col_names) && is.numeric(data[[col_A]]) && is.numeric(data[[col_S]])) {
      as_col_name <- paste0(prefix, "_THLI")
      data <- data %>%
        mutate(!!as_col_name := coalesce(.data[[col_A]], 0) - coalesce(.data[[col_S]], 0))
    }
  }
  data
}

prod20_thli <- subtract_columns_AS(prod20_thli)
prod20_thli[is.na(prod20_thli)] <- 0

prod20_thli <- prod20_thli %>%
  select(c("grid_code", ADM0_NAME, ends_with("_THLI"))) %>%
  rename(CELL5M = grid_code)

prod_long <- prod20_thli %>%
  pivot_longer(
    cols = matches("_THLI"),
    names_to = "crop_type",
    values_to = "prod_mt",
    names_transform = list(crop_type = ~ gsub("_THLI", "", .x))
  )

# ---- Project production forward using FAOSTAT growth trends ----
FAO <- read_csv(file.path(data_dir, "FAO_Stat", "FAOSTAT_data_en_5-2-2025.csv")) # production in tonnes
mapping <- read_csv(file.path(data_dir, "FAO_Stat", "FAO_SPAM_mapping.csv")) %>%
  select(Item, MapSpam_Code) %>%
  mutate(MapSpam_Code = toupper(MapSpam_Code))

# Keep only individual (non-grouped) MapSPAM commodity categories
single_commodities <- c("BANA", "BARL", "BEAN", "CASS", "CHIC", "CNUT", "COCO", "ACOF", "RCOF",
                        "COTT", "COWP", "GROU", "LENT", "MAIZ", "PMIL", "SMIL", "OILP", "PIGE",
                        "PLNT", "POTA", "RAPE", "RICE", "SESA", "SORG", "SOYB", "SUGB", "SUGC",
                        "SUNF", "SWPO", "TEAS", "TOBA", "WHEA", "YAMS")

mapping <- mapping %>%
  filter(MapSpam_Code %in% single_commodities) %>%
  filter(Item != "Rice, paddy (rice milled equivalent)") # avoid double-counting rice

# There is still a many-to-many relationship here since ACOF and RCOF both
# map to FAO's single "green coffee" category
FAO <- FAO %>%
  right_join(mapping) %>%
  select(Area, Year, Value, MapSpam_Code)

# FAOSTAT doesn't have current_year data yet — estimate it per country/crop
# with a random forest (captures nonlinear, non-monotonic year-to-year
# fluctuations better than a simple linear trend)
predictions_current_year <- FAO %>%
  filter(!is.na(Value), is.finite(Value)) %>%
  group_by(Area, MapSpam_Code) %>%
  filter(n() >= 2) %>% # need at least 2 points to fit a trend
  do({
    df <- .
    rf_model <- randomForest(Value ~ Year, data = df)
    pred <- predict(rf_model, newdata = data.frame(Year = current_year))
    tibble(Year = current_year, Value = pred)
  }) %>%
  ungroup()

FAO <- bind_rows(FAO, predictions_current_year)

# Compute each year's production relative to base_year, per country/crop
faostat_factors <- FAO %>%
  filter(Year >= base_year) %>%
  group_by(Area, MapSpam_Code) %>%
  mutate(
    prod_base = if (any(Year == base_year)) Value[Year == base_year] else NA_real_,
    growth_factor = Value / prod_base
  ) %>%
  filter(!is.na(prod_base), Year > base_year) %>%
  select(Area, MapSpam_Code, Year, growth_factor) %>%
  rename(ADM0_NAME = Area, crop_type = MapSpam_Code, year = Year, factor = growth_factor)

# Average any repeated country/crop/year rows (shouldn't happen, but safe)
faostat_factors <- faostat_factors %>%
  group_by(ADM0_NAME, crop_type, year) %>%
  summarise(factor = mean(factor, na.rm = TRUE), .groups = "drop") %>%
  filter(if_all(everything(), ~ !is.na(.) & !is.nan(.) & is.finite(.)))

faostat_factors_wide <- faostat_factors %>%
  pivot_wider(names_from = year, values_from = factor, names_prefix = "factor_") %>%
  mutate(across(everything(), ~ replace_na(., 0)))

factor_cols <- paste0("factor_", years[years > base_year])

# Merge growth factors back onto the full (un-grouped) production table —
# categories that were excluded from the FAO mapping simply get no factor
mapspam_projected <- prod_long %>%
  left_join(faostat_factors_wide)

# Missing, zero, or infinite factors default to 1 — i.e. hold production
# flat at the base_year level rather than dropping the crop/country
mapspam_projected <- mapspam_projected %>%
  mutate(across(any_of(factor_cols), ~ replace(., is.na(.), 1))) %>%
  mutate(across(any_of(factor_cols), ~ replace(., is.infinite(.), 1))) %>%
  mutate(across(any_of(factor_cols), ~ replace(., . == 0, 1)))

# Project production for every year: base_year as-is, later years scaled by their factor
mapspam_projected[[paste0("prod_mt_", base_year)]] <- mapspam_projected$prod_mt
for (yr in years[years > base_year]) {
  mapspam_projected[[paste0("prod_mt_", yr)]] <- mapspam_projected$prod_mt * mapspam_projected[[paste0("factor_", yr)]]
}

production_file <- file.path(data_dir, "created_tables", "production_07_2025.csv")
write_csv(mapspam_projected, production_file)


# ============================================================
# 4. Calculate EF
# ============================================================
# EF = LD / adjusted production, at admin0/1/2, for all four gases.

production <- read_csv(production_file) %>%
  mutate(crop_type = case_when(
    crop_type == "COFF" ~ "ACOF", # Arabica coffee
    crop_type == "MILL" ~ "SMIL", # small millet
    crop_type %in% c("CITR", "ONIO", "TOMA", "RUBB") ~ "REST", # lump others into REST
    TRUE ~ crop_type
  ))

for (gas_label in names(gas_regex)) {
  for (admin_level in c("admin0", "admin1", "admin2")) {
    compute_ef_table(gas_label, admin_level, production, grid_area, years, results_dir, ef_dir)
    message("Completed EF for ", gas_label, " at ", admin_level)
  }
}