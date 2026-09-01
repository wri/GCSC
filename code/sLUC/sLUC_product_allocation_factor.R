library(foreign)
library(tidyverse)
library(data.table)

# ============================================================
# MapSPAM product-expansion allocation factors (PAF), 2000-2020
#
# Builds, for every 10km grid cell, the share of that cell's cropland
# expansion between MapSPAM snapshots (2000->2005, 2005->2010, 2010->2020)
# attributable to each crop (PAF0005 / PAF0510 / PAF1020). This is the
# `product_expansion_PAF_SUBS_*.csv` file the SLUC emission-factor pipeline
# reads as an input.
#
# Stages:
#   1-4. Format each MapSPAM year (2010, 2005, 2000, 2020): read the raw
#        physical-area tables, subtract subsistence from the all-technology
#        total, add a combined pasture category, and (via an external GIS
#        step) intersect the grid with GADM boundaries.
#   5. Merge 2005/2010/2020 into one long crop-area table, then back-cast
#      2000 values for crops that were reported as combined categories that
#      year (e.g. "COFF" split into ACOF/RCOF) using each crop's later-year
#      area trend.
#   6. Compute PAF0005/PAF0510/PAF1020 from the year-over-year differences
#      in crop area per grid cell.
#
# Run top to bottom — later stages depend on tables built earlier in the
# same run, and on the checkpoint file written partway through (see the
# note on `crop_area_file` below).
# ============================================================

# ---- User settings ----
# Root folder containing the project's subfolders (geospatial_data/,
# created_tables/). All paths below are built relative to this.
data_dir <- "path/to/your/project/folder"

created_tables_dir <- file.path(data_dir, "created_tables")
dir.create(created_tables_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# Shared functions
# ============================================================

# Sum a crop's high/low/irrigated-input columns (suffix "_H"/"_L"/"_I")
# into one "<crop>_THLI" column, per 4-letter crop prefix.
add_columns_THLI <- function(data) {
  col_names <- names(data)
  unique_prefixes <- unique(substr(col_names, 1, 4))
  
  for (prefix in unique_prefixes) {
    cols_with_prefix <- col_names[grep(paste0("^", prefix, "_(H|L|I)"), col_names)]
    cols_with_prefix_numeric <- Filter(function(x) is.numeric(data[[x]]), cols_with_prefix)
    if (length(cols_with_prefix_numeric) == 0) next
    
    thli_col_name <- paste0(prefix, "_THLI")
    data <- data %>%
      mutate(across(all_of(cols_with_prefix_numeric), as.numeric),
             !!thli_col_name := rowSums(select(., all_of(cols_with_prefix_numeric)), na.rm = TRUE))
  }
  data
}

# Sum every "_S" (subsistence) column into one "SUBS_THLI" column. Named
# with the "_THLI" suffix so subsistence is treated like any other crop by
# the rest of the pipeline (same convention used for pasture, "PAST_THLI").
add_subs_column <- function(data) {
  cols_s <- grep("_S$", names(data), value = TRUE)
  data %>% mutate(SUBS_THLI = rowSums(select(., all_of(cols_s)), na.rm = TRUE))
}

# Non-subsistence area = all-technology area (_A) minus subsistence area
# (_S), per crop. Subtracting rather than summing H+L+I directly avoids
# under-counting if one of the individual technology columns is missing.
subtract_columns_AS <- function(data) {
  col_names <- names(data)
  unique_prefixes <- unique(substr(col_names, 1, 4))
  
  for (prefix in unique_prefixes) {
    col_A <- paste0(prefix, "_A")
    col_S <- paste0(prefix, "_S")
    
    if (all(c(col_A, col_S) %in% col_names) && is.numeric(data[[col_A]]) && is.numeric(data[[col_S]])) {
      as_col_name <- paste0(prefix, "_THLI")
      data <- data %>%
        mutate(!!as_col_name := pmax(coalesce(.data[[col_A]], 0) - coalesce(.data[[col_S]], 0), 0))
      # pmax floors at 0 (A-S shouldn't go negative); coalesce treats NA as 0
    }
  }
  data
}

# Back-cast 2000 area for each crop from its 2005/2010/2020 trend (per
# country x crop), fit as area ~ Year + FIPS0 so country-level differences
# are absorbed into the trend rather than pooled across countries.
predict_area_2000 <- function(data) {
  dt <- as.data.table(data)
  
  need <- c("Id", "CELL5M", "FIPS0", "crop_type", "area_2005_ha", "area_2010_ha", "area_2020_ha")
  stopifnot(all(need %in% names(dt)))
  
  dt[, `:=`(
    FIPS0 = as.factor(FIPS0),
    crop_type = as.factor(crop_type),
    area_2005_ha = fifelse(is.na(area_2005_ha), 0, area_2005_ha),
    area_2010_ha = fifelse(is.na(area_2010_ha), 0, area_2010_ha),
    area_2020_ha = fifelse(is.na(area_2020_ha), 0, area_2020_ha)
  )]
  
  dt[, area_2000_ha := NA_real_]
  
  for (cr in levels(dt$crop_type)) {
    idx <- which(dt$crop_type == cr)
    sub <- dt[idx]
    
    long_c <- rbindlist(list(
      sub[, .(FIPS0, Year = 2005L, area = area_2005_ha)],
      sub[, .(FIPS0, Year = 2010L, area = area_2010_ha)],
      sub[, .(FIPS0, Year = 2020L, area = area_2020_ha)]
    ), use.names = TRUE)
    long_c <- long_c[!is.na(area)]
    
    if (nrow(long_c) < 2L || all(long_c$area == 0)) {
      dt[idx, area_2000_ha := 0]
      next
    }
    
    fit <- lm(area ~ Year + FIPS0, data = long_c)
    pred_c <- as.numeric(predict(fit, newdata = data.table(Year = 2000L, FIPS0 = sub$FIPS0)))
    dt[idx, area_2000_ha := pmax(0, round(pred_c, 2))]
  }
  
  # If a crop had no area in either 2005 or 2010, treat 2000 as zero too
  dt[area_2005_ha == 0 & area_2010_ha == 0, area_2000_ha := 0]
  dt[]
}

# For each crop that was reported as part of a combined MapSPAM 2000
# category, its 2000-back-cast area is split proportionally to how that
# combined category's members compare to each other in the 2005-2020 trend.
# E.g. proportion for ACOF = area_2000_ha[ACOF] / (area_2000_ha[ACOF] + area_2000_ha[RCOF]).
# Crops that were never combined get proportion = 1 (area_2000_ha / itself).
calculate_proportions <- function(data) {
  data <- data %>%
    group_by(Id, CELL5M, FIPS0) %>%
    mutate(proportion = case_when(
      crop_type == "ACOF" ~ area_2000_ha / (area_2000_ha + sum(area_2000_ha[crop_type == "RCOF"], na.rm = TRUE)),
      crop_type == "BANA" ~ area_2000_ha / (area_2000_ha + sum(area_2000_ha[crop_type == "PLTN"], na.rm = TRUE)),
      crop_type == "BARL" ~ area_2000_ha / area_2000_ha,
      crop_type == "BEAN" ~ area_2000_ha / area_2000_ha,
      crop_type == "CASS" ~ area_2000_ha / area_2000_ha,
      crop_type == "CHIC" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CHIC", "COWP", "LENT", "PIGE", "OPUL")], na.rm = TRUE),
      crop_type == "CNUT" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE")], na.rm = TRUE),
      crop_type == "COCO" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE")], na.rm = TRUE),
      crop_type == "COTT" ~ area_2000_ha / area_2000_ha,
      crop_type == "COWP" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CHIC", "COWP", "LENT", "PIGE", "OPUL")], na.rm = TRUE),
      crop_type == "GROU" ~ area_2000_ha / area_2000_ha,
      crop_type == "LENT" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CHIC", "COWP", "LENT", "PIGE", "OPUL")], na.rm = TRUE),
      crop_type == "MAIZ" ~ area_2000_ha / area_2000_ha,
      crop_type == "OCER" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "OFIB" ~ area_2000_ha / area_2000_ha,
      crop_type == "OILP" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE")], na.rm = TRUE),
      crop_type == "OOIL" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE")], na.rm = TRUE),
      crop_type == "OPUL" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CHIC", "COWP", "LENT", "PIGE", "OPUL")], na.rm = TRUE),
      crop_type == "ORTS" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "PIGE" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CHIC", "COWP", "LENT", "PIGE", "OPUL")], na.rm = TRUE),
      crop_type == "PLTN" ~ area_2000_ha / (area_2000_ha + sum(area_2000_ha[crop_type == "BANA"], na.rm = TRUE)),
      crop_type == "PMIL" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("PMIL", "SMIL")], na.rm = TRUE),
      crop_type == "POTA" ~ area_2000_ha / area_2000_ha,
      crop_type == "RAPE" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE")], na.rm = TRUE),
      crop_type == "RCOF" ~ area_2000_ha / (area_2000_ha + sum(area_2000_ha[crop_type == "ACOF"], na.rm = TRUE)),
      crop_type == "REST" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "RICE" ~ area_2000_ha / area_2000_ha,
      crop_type == "SESA" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE")], na.rm = TRUE),
      crop_type == "SMIL" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("PMIL", "SMIL")], na.rm = TRUE),
      crop_type == "SORG" ~ area_2000_ha / area_2000_ha,
      crop_type == "SOYB" ~ area_2000_ha / area_2000_ha,
      crop_type == "SUGB" ~ area_2000_ha / area_2000_ha,
      crop_type == "SUGC" ~ area_2000_ha / area_2000_ha,
      crop_type == "SUNF" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE")], na.rm = TRUE),
      crop_type == "SWPO" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("SWPO", "YAMS")], na.rm = TRUE),
      crop_type == "TEAS" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "TEMF" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "TOBA" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "TROF" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "VEGE" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST")], na.rm = TRUE),
      crop_type == "WHEA" ~ area_2000_ha / area_2000_ha,
      crop_type == "YAMS" ~ area_2000_ha / sum(area_2000_ha[crop_type %in% c("SWPO", "YAMS")], na.rm = TRUE),
      crop_type == "PAST" ~ area_2000_ha / area_2000_ha,
      crop_type == "SUBS" ~ area_2000_ha / area_2000_ha,
      TRUE ~ NA_real_
    )) %>%
    ungroup()
  data
}

# Splitting rules for the crop categories MapSPAM 2000 reports combined
# that later years report separately (e.g. "COFF" -> "ACOF"/"RCOF").
split_rules <- data.frame(
  original_crop_type = c("COFF", "BANP", "OPUL", "OOIL", "REST", "MILL", "SWPY"),
  new_crop_types = I(list(
    c("ACOF", "RCOF"),
    c("BANA", "PLTN"),
    c("CHIC", "COWP", "LENT", "PIGE", "OPUL"),
    c("CNUT", "COCO", "OILP", "OOIL", "SESA", "SUNF", "RAPE"),
    c("OCER", "ORTS", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", "REST"),
    c("SMIL", "PMIL"),
    c("SWPO", "YAMS")
  ))
)

expand_crop_types <- function(crop_type) {
  if (crop_type %in% split_rules$original_crop_type) {
    split_rules$new_crop_types[split_rules$original_crop_type == crop_type][[1]]
  } else {
    c(crop_type)
  }
}


# ============================================================
# 1. MapSPAM 2010
# ============================================================
# Read the MapSPAM physical-area tables for the different crop technologies.

ta <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2010", "spam2010V2r0_global_A_TA.dbf"))
ts <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2010", "spam2010V2r0_global_A_TS.dbf")) # subsistence

thli_2010 <- ta[, -8] %>% inner_join(ts[, -8]) # drop tech-type column (col 8); it blocks the merge
thli_2010 <- subtract_columns_AS(thli_2010) # e.g. ACOF_THLI = ACOF_A - ACOF_S
thli_2010 <- add_subs_column(thli_2010)
thli_2010 <- thli_2010 %>% select(c("ISO3", "PROD_LEVEL", "CELL5M", "X", "Y"), ends_with("_THLI"))

# write.dbf(thli_2010, file.path(created_tables_dir, "spam2010V2r0_global_A_THLI.DBF"))
# write_csv(thli_2010, file.path(created_tables_dir, "spam2010V2r0_global_A_THLI.CSV"))

# ID-only table for the external GIS spatial join described below
mapspam_id <- thli_2010 %>% select(c("ISO3", "PROD_LEVEL", "CELL5M", "X", "Y"))
# write.dbf(mapspam_id, file.path(created_tables_dir, "mapspam10_A_ids.DBF"))
# write.csv(mapspam_id, file.path(created_tables_dir, "mapspam10_A_ids.CSV"), row.names = FALSE)

# ---- Spatial analysis and table join (done in a GIS, not in R) ----
# Read the simplified ID table above, display X,Y data, and save as a
# shapefile. Spatially join it to the 10km grid (target = grid, join
# feature = mapspam points, one-to-one, "completely contains"), then
# intersect the result with GADM admin2 boundaries (grid first, then GADM).
# Export that table and read it back here:
spam10_gadm <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "map10Gadm4.dbf"))
# Not currently joined into the pipeline below — the crop/pasture merges
# join directly on CELL5M rather than through this grid-to-GID_2 table.

# ---- Add pastures ----
pasture_2010 <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "cultivated_area_annual_MapSpam_GPW_2010.csv")) %>%
  select(Id, CELL5M, `2010_dominant_class`) %>%
  rename(PAST_THLI = `2010_dominant_class`) # pasture area in hectares; subsistence/low-intensity pasture is not filtered out here

crops_2010 <- thli_2010
mapspam10 <- crops_2010 %>% left_join(pasture_2010, by = "CELL5M") # keeps crop cells with or without pasture; use an outer join instead if pasture-only cells are needed later


# ============================================================
# 2. MapSPAM 2005
# ============================================================

coordinates_2005 <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2005", "cell5m_allockey_xy.csv")) # 2005 crop tables lack X/Y, so join them in separately

ta <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2005", "spam2005V3r2_global_A_TA.DBF"))
ts <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2005", "spam2005V3r2_global_A_TS.DBF")) # subsistence

thli_2005 <- ta[, -6] %>% inner_join(ts[, -6])
thli_2005 <- subtract_columns_AS(thli_2005)
thli_2005 <- add_subs_column(thli_2005)
thli_2005 <- thli_2005 %>% select(c("ISO3", "PROD_LEVEL", "CELL5M"), ends_with("_THLI"))
thli_2005 <- left_join(thli_2005, coordinates_2005[, -1], by = c("CELL5M" = "hc_seq5m"))
colnames(thli_2005) <- toupper(colnames(thli_2005)) # match ArcGIS column labeling

# write.dbf(thli_2005, file.path(created_tables_dir, "spam2005V3r2_global_A_THLI.DBF"))
# write_csv(thli_2005, file.path(created_tables_dir, "spam2005V3r2_global_A_THLI.CSV"))

mapspam2005_id <- thli_2005 %>% select(c("ISO3", "PROD_LEVEL", "CELL5M", "X", "Y"))
# write.dbf(mapspam2005_id, file.path(created_tables_dir, "mapspam2005_A_ids.DBF"))
# write.csv(mapspam2005_id, file.path(created_tables_dir, "mapspam2005_A_ids.CSV"), row.names = FALSE)

# ---- Spatial analysis and table join (done in a GIS, not in R) ----
# Same procedure as the 2010 section above.
spam05_gadm <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "map05Gadm4.dbf"))
# Not currently joined into the pipeline below.

# ---- Add pastures ----
pasture_2005 <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "cultivated_area_annual_MapSpam_GPW_2005.csv")) %>%
  select(Id, CELL5M, `2005_dominant_class`) %>%
  rename(PAST_THLI = `2005_dominant_class`)

crops_2005 <- thli_2005
mapspam05 <- crops_2005 %>% left_join(pasture_2005, by = "CELL5M")


# ============================================================
# 3. MapSPAM 2000
# ============================================================

thli_2000 <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2000", "spam_p.DBF"))
thli_2000 <- add_columns_THLI(thli_2000) # 2000 has no "_A" (all-technology) column, so sum H+L+I instead of subtracting A-S
thli_2000 <- add_subs_column(thli_2000)
thli_2000 <- thli_2000 %>% select(c("STAT_CODE", "PROD_LEVEL", "HC_SEQ5M", "X", "Y"), ends_with("_THLI"))

# write.dbf(thli_2000, file.path(created_tables_dir, "spam2000V3r107_global_A_THLI.DBF"))
# write_csv(thli_2000, file.path(created_tables_dir, "spam2000V3r107_global_A_THLI.CSV"))

mapspam2000_id <- thli_2000 %>% select(c("STAT_CODE", "PROD_LEVEL", "HC_SEQ5M", "X", "Y"))
# write.dbf(mapspam2000_id, file.path(created_tables_dir, "mapspam2000_ids.DBF"))
# write.csv(mapspam2000_id, file.path(created_tables_dir, "mapspam2000_ids.CSV"), row.names = FALSE)

# ---- Spatial analysis and table join (done in a GIS, not in R) ----
# Same procedure as the 2010 section above.
spam00_gadm <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "map00Gadm4.dbf"))
# Not currently joined into the pipeline below.

# ---- Add pastures ----
pasture_2000 <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "cultivated_area_annual_MapSpam_GPW_2000.csv")) %>%
  select(Id, CELL5M, `2000_dominant_class`) %>%
  rename(PAST_THLI = `2000_dominant_class`)

crops_2000 <- thli_2000 %>% rename("CELL5M" = "HC_SEQ5M")
mapspam00 <- crops_2000 %>% left_join(pasture_2000, by = "CELL5M")


# ============================================================
# 4. MapSPAM 2020
# ============================================================

ta <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2020_v2",
                         "spam2020V2r0_global_physical_area", "spam2020V2r0_global_A_TA.csv"))
ts <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "mapspam_2020_v2",
                         "spam2020V2r0_global_physical_area", "spam2020V2r0_global_A_TS.csv")) # rainfed subsistence

thli_2020 <- ta[, -11] %>% full_join(ts[, -11])
thli_2020 <- subtract_columns_AS(thli_2020)
thli_2020 <- add_subs_column(thli_2020)
thli_2020 <- thli_2020 %>%
  select(c("FIPS0", "grid_code", "x", "y"), ends_with("_THLI")) %>%
  rename("CELL5M" = "grid_code")

# write_csv(thli_2020, file.path(created_tables_dir, "spam2020V2r0_global_THLI.CSV"))

mapspam20_id <- thli_2020 %>% select(c("CELL5M", "x", "y"))
# write.csv(mapspam20_id, file.path(created_tables_dir, "mapspam20_A_ids.CSV"), row.names = FALSE)

# ---- Spatial analysis and table join (done in a GIS, not in R) ----
# Same procedure as the 2010 section above.
spam20_gadm <- read.dbf(file.path(data_dir, "geospatial_data", "input_data", "spam20v2_gadm2.dbf")) %>%
  rename("CELL5M" = "grid_code")
# Not currently joined into the pipeline below.

# ---- Add pastures ----
pasture_2020 <- read_csv(file.path(data_dir, "geospatial_data", "input_data", "cultivated_area_annual_MapSpam_GPW_2020v2.csv")) %>%
  select(Id, grid_code, `2020_dominant_class`) %>%
  rename(PAST_THLI = `2020_dominant_class`, CELL5M = grid_code)

crops_2020 <- thli_2020
mapspam20 <- crops_2020 %>% left_join(pasture_2020, by = "CELL5M")


# ============================================================
# 5. Merge years and back-cast 2000
# ============================================================

colnames(mapspam00) <- gsub("_THLI", "", colnames(mapspam00))
colnames(mapspam05) <- gsub("_THLI", "", colnames(mapspam05))
colnames(mapspam10) <- gsub("_THLI", "", colnames(mapspam10))
colnames(mapspam20) <- gsub("_THLI", "", colnames(mapspam20))

crop_list_2000 <- mapspam00 %>% select(WHEA:PAST) %>% colnames()
crop_list_2005 <- mapspam05 %>% select(WHEA:SUBS, PAST) %>% colnames()
crop_list_2010 <- mapspam10 %>% select(WHEA:SUBS, PAST) %>% colnames()
crop_list_2020 <- mapspam20 %>% select(BANA:SUBS, PAST) %>% colnames()
full_crop_list <- unique(c(crop_list_2000, crop_list_2005, crop_list_2010, crop_list_2020))

# ---- Wide to long, per year ----
mapspam05_long <- mapspam05 %>%
  select(Id, CELL5M, WHEA:SUBS, PAST) %>%
  pivot_longer(cols = c("WHEA":"PAST"), names_to = "crop_type", values_to = "area_2005_ha")

mapspam10_long <- mapspam10 %>%
  select(Id, CELL5M, WHEA:SUBS, PAST) %>%
  pivot_longer(cols = c("WHEA":"PAST"), names_to = "crop_type", values_to = "area_2010_ha")

mapspam20_long <- mapspam20 %>%
  select(Id, CELL5M, FIPS0, BANA:SUBS, PAST) %>%
  pivot_longer(cols = c("BANA":"PAST"), names_to = "crop_type", values_to = "area_2020_ha") %>%
  mutate(crop_type = case_when(
    crop_type == "COFF" ~ "ACOF", # Arabica coffee, to match other years
    crop_type == "MILL" ~ "SMIL", # small millet, to match other years
    crop_type == "CITR" ~ "TROF", # citrus/onion/tomato/rubber aren't present in
    crop_type == "ONIO" ~ "VEGE", # earlier years, so fold them into the closest
    crop_type == "TOMA" ~ "VEGE", # existing category
    crop_type == "RUBB" ~ "REST",
    TRUE ~ crop_type
  )) %>%
  group_by(Id, CELL5M, FIPS0, crop_type) %>% # re-sum cells whose crops were just merged above
  summarize(area_2020_ha = sum(area_2020_ha, na.rm = TRUE), .groups = "drop")

mapspam00_long <- mapspam00 %>%
  select(Id, CELL5M, WHEA:SUBS, PAST) %>%
  pivot_longer(cols = c("WHEA":"PAST"), names_to = "crop_type", values_to = "area_2000_ha") %>%
  mutate(crop_type = if_else(crop_type == "OTHE", "REST", crop_type))

# 2000 is merged in later (needs the back-cast adjustments below)
crop_area_spam <- mapspam05_long %>%
  full_join(mapspam10_long) %>%
  full_join(mapspam20_long)

# ---- Back-cast 2000 gaps ----
# MapSPAM 2000 reports several crops as combined categories (e.g. "COFF").
# Predict what 2000 area each combined category would have had (from its
# 2005-2020 trend), then split that predicted total across its component
# crops using each component's own predicted-2000 proportion.
predicted_values <- predict_area_2000(crop_area_spam)
area_proportions <- calculate_proportions(predicted_values)
# write_csv(area_proportions, file.path(created_tables_dir, "PE_backcast_proportions_v2.CSV"))

prop_simpl <- area_proportions %>% select(Id, CELL5M, FIPS0, crop_type, proportion)

split_mapspam00_long <- mapspam00_long %>%
  rowwise() %>%
  mutate(crop_type = list(expand_crop_types(crop_type))) %>%
  unnest(crop_type) %>%
  ungroup() %>%
  left_join(prop_simpl) %>% # prop_simpl is the back-cast prediction, so it can cover more cells than mapspam00_long's original footprint
  mutate(area_2000 = area_2000_ha * proportion) %>%
  select(Id, CELL5M, FIPS0, crop_type, area_2000) %>%
  rename("area_2000_ha" = "area_2000")

crop_area_spam <- crop_area_spam %>% full_join(split_mapspam00_long)

# Checkpoint: the rest of the pipeline re-reads this file rather than using
# the in-memory object, so it must be written before the read below runs.
crop_area_file <- file.path(created_tables_dir, "crop_area_spamv2.CSV")
write_csv(crop_area_spam, crop_area_file)


# ============================================================
# 6. Product-expansion allocation factors (PAF)
# ============================================================

crop_area_spam <- read_csv(crop_area_file)

crop_area_spam <- crop_area_spam %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(
    diff_ha_00_05 = area_2005_ha - area_2000_ha,
    diff_ha_05_10 = area_2010_ha - area_2005_ha,
    diff_ha_10_20 = area_2020_ha - area_2010_ha
  )

# The PAF denominator is the total expansion (only positive differences)
# across all crops in a grid cell for that time window
denominator_0005 <- crop_area_spam %>% filter(diff_ha_00_05 > 0) %>%
  group_by(CELL5M) %>% summarise(denominator0005 = sum(diff_ha_00_05, na.rm = TRUE))
denominator_0510 <- crop_area_spam %>% filter(diff_ha_05_10 > 0) %>%
  group_by(CELL5M) %>% summarise(denominator0510 = sum(diff_ha_05_10, na.rm = TRUE))
denominator_1020 <- crop_area_spam %>% filter(diff_ha_10_20 > 0) %>%
  group_by(CELL5M) %>% summarise(denominator1020 = sum(diff_ha_10_20, na.rm = TRUE))

# Left join: cells with no positive expansion in a window get no denominator
# (and therefore PAF 0, via the ifelse below) rather than being dropped
crop_area_spam <- crop_area_spam %>%
  left_join(denominator_0005) %>%
  left_join(denominator_0510) %>%
  left_join(denominator_1020)

crop_area_spam$PAF0005 <- ifelse(crop_area_spam$diff_ha_00_05 > 0, crop_area_spam$diff_ha_00_05 / crop_area_spam$denominator0005, 0)
crop_area_spam$PAF0510 <- ifelse(crop_area_spam$diff_ha_05_10 > 0, crop_area_spam$diff_ha_05_10 / crop_area_spam$denominator0510, 0)
crop_area_spam$PAF1020 <- ifelse(crop_area_spam$diff_ha_10_20 > 0, crop_area_spam$diff_ha_10_20 / crop_area_spam$denominator1020, 0)

# Sanity check: PAFs per cell should sum to 1 (where there was any expansion)
PAFs_1 <- crop_area_spam %>%
  group_by(CELL5M) %>%
  summarise(PAF0005 = sum(PAF0005, na.rm = TRUE),
            PAF0510 = sum(PAF0510, na.rm = TRUE),
            PAF1020 = sum(PAF1020, na.rm = TRUE))

# Drop subsistence and pasture — pasture allocation isn't fully developed
# yet (cells with pasture but no cropland aren't included), and subsistence
# isn't reported under the corporate emissions standard this feeds into
crop_area_spam <- crop_area_spam %>%
  filter(crop_type != "SUBS") %>%
  filter(crop_type != "PAST")

write_csv(crop_area_spam, file.path(created_tables_dir, "product_expansion_PAF_SUBS_072325.csv"))