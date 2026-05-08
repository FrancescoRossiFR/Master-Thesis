############################################################
# 03_build_analysis_panel.R
#
# Purpose:
# build the final analysis panel used for superstar-city
# classification and regressions.
#
# Inputs:
# - data/processed/panel_apartments.csv
# - data/processed/panel_single_family.csv
# - data/processed/income_panel_interpolated.csv
#
# Outputs:
# - data/processed/analysis_panel_apartments.csv
# - data/processed/analysis_panel_single_family.csv
#
############################################################

############################################################
# Libraries

required_packages <- c(
  "tidyverse"
)

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

############################################################
# Load Processed Datasets

panel_apartments <- read_csv(
  "data/processed/panel_apartments.csv",
  show_col_types = FALSE
)

panel_single_family <- read_csv(
  "data/processed/panel_single_family.csv",
  show_col_types = FALSE
)

income_panel <- read_csv(
  "data/processed/income_panel_interpolated.csv",
  show_col_types = FALSE
)

############################################################
# Standardize City Naming

income_panel <- income_panel %>%
  rename(city = cities_clean)

############################################################
# Merge Apartment Analysis Panel

analysis_panel_apartments <- panel_apartments %>%
  
  left_join(
    income_panel,
    by = c("city", "year")
  ) %>%
  
  arrange(city, year)

############################################################
# Merge Single-Family Analysis Panel

analysis_panel_single_family <- panel_single_family %>%
  
  left_join(
    income_panel,
    by = c("city", "year")
  ) %>%
  
  arrange(city, year)

############################################################
# Validation Checks

stopifnot(
  nrow(analysis_panel_apartments) > 0
)

stopifnot(
  nrow(analysis_panel_single_family) > 0
)

############################################################
# Export Final Analysis Panels

write_csv(
  analysis_panel_apartments,
  "data/processed/analysis_panel_apartments.csv"
)

write_csv(
  analysis_panel_single_family,
  "data/processed/analysis_panel_single_family.csv"
)