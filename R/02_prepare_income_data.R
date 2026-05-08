############################################################
# 02_prepare_income_data.R
#
# Purpose:
# Clean and interpolate Eurostat urban income data for 
# German cities, including national median benchmarking.
#
# Inputs:
# - data/raw/urb_clivcon__custom_20168663_linear.csv
#
# Outputs:
# - data/processed/income_panel_interpolated.csv
#
############################################################

############################################################
# Libraries

library(tidyverse)
library(janitor)
library(zoo)

############################################################
# 1. Constants & Configuration

cities_by_name <- c(
  "Berlin", "Hamburg", "München", "Köln", "Frankfurt", "Stuttgart", 
  "Düsseldorf", "Dortmund", "Duisburg", "Bonn", "Münster", "Wiesbaden", 
  "Lübeck", "Potsdam", "Erfurt", "Karlsruhe", "Dresden", "Chemnitz", "Germany"
)

manual_germany_data <- tibble(
  cities_clean = "Germany",
  type = "median_income",
  year = 2012:2019,
  income_manual = c(23514, 23934, 24363, 24800, 25611, 26448, 27313, 28206)
)

############################################################
# 2. Load and Initial Cleaning

income_raw <- read_csv(
  "data/raw/urb_clivcon__custom_20168663_linear.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>% 
  clean_names()

income_processed <- income_raw %>%
  mutate(
    cities_clean = str_remove(cities, " \\(.*\\)") %>% str_trim(),
    year = as.integer(time_period),
    income = as.numeric(obs_value),
    type = case_when(
      str_detect(indic_ur, "Median") ~ "median_income",
      str_detect(indic_ur, "Average") ~ "mean_income",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(cities_clean %in% cities_by_name, !is.na(type)) %>%
  filter(year >= 2000 & year <= 2019) # focus on the window 2000-2019 to have a clean interpolation range given missing data before and after such interval

############################################################
# 3. Manual Corrections & Interpolation

# Step A: Apply manual Germany overrides
income_processed <- income_processed %>%
  left_join(manual_germany_data, by = c("cities_clean", "type", "year")) %>%
  mutate(income = coalesce(income_manual, income)) %>%
  select(-income_manual)

# Step B: Interpolation & Extrapolation
income_panel <- income_processed %>%
  group_by(cities_clean, type) %>%
  complete(year = 2000:2019) %>%
  arrange(year) %>%
  mutate(
    # Fill internal gaps up to 5 years
    income = zoo::na.approx(income, x = year, na.rm = FALSE, maxgap = 5),
    # Extrapolate to edges (fixes 2000/2001 issue)
    income = zoo::na.fill(income, fill = "extend")
  ) %>%
  ungroup()

############################################################
# 4. Benchmarking & Feature Engineering

# Create National Reference
df_germany_ref <- income_panel %>%
  filter(cities_clean == "Germany", type == "median_income") %>%
  # Collapse any potential duplicates in reference data
  group_by(year) %>%
  summarize(germany_median = max(income, na.rm = TRUE), .groups = "drop")

income_final <- income_panel %>%
  # COLLAPSE DUPLICATES: Pivot and then summarize to ensure 1 row per city-year
  pivot_wider(names_from = type, values_from = income) %>%
  group_by(cities_clean, year) %>%
  summarize(
    mean_income = max(mean_income, na.rm = TRUE),
    median_income = max(median_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Fix -Inf values from max(NA)
  mutate(across(c(mean_income, median_income), ~ifelse(is.infinite(.), NA, .))) %>%
  # Join national median
  left_join(df_germany_ref, by = "year") %>%
  filter(cities_clean != "Germany") %>%
  group_by(cities_clean) %>%
  arrange(year) %>%
  mutate(
    distance_internal = mean_income / median_income,
    dist_increased_5y = distance_internal > lag(distance_internal, 4),
    dist_to_germany = median_income / germany_median
  ) %>%
  ungroup()

############################################################
# 5. Export

write_csv(
  income_final, 
  "data/processed/income_panel_interpolated.csv"
)