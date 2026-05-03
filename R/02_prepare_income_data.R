############################################################
# 02_prepare_income_data.R
#
# Purpose:
# Clean and interpolate Eurostat urban income data
# for selected German cities.
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

required_packages <- c(
  "tidyverse",
  "janitor",
  "zoo"
)

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

############################################################
# Constants

# Cities included in the analysis
cities_by_name <- c(
  "Berlin",
  "Hamburg",
  "München",
  "Köln",
  "Frankfurt",
  "Stuttgart",
  "Düsseldorf",
  "Dortmund",
  "Duisburg",
  "Bonn",
  "Münster",
  "Wiesbaden",
  "Lübeck",
  "Potsdam",
  "Erfurt",
  "Karlsruhe",
  "Dresden",
  "Chemnitz"
)

############################################################
# Helper Functions

# ----------------------------------------------------------
# Interpolate short income gaps
# ----------------------------------------------------------

interpolate_income <- function(df, max_gap = 3) {
  
  df %>%
    
    group_by(cities_clean) %>%
    
    # Create complete yearly sequence
    complete(
      year = seq(
        min(year),
        max(year),
        by = 1
      )
    ) %>%
    
    arrange(cities_clean, year) %>%
    
    mutate(
      
      # Missing observations
      is_na = is.na(income),
      
      # Consecutive gap tracking
      gap_id = cumsum(!is_na),
      
      gap_length = ave(
        is_na,
        gap_id,
        FUN = sum
      ),
      
      # Linear interpolation
      income_interp = zoo::na.approx(
        income,
        x = year,
        na.rm = FALSE
      ),
      
      # Keep interpolation only for short gaps
      income = ifelse(
        is_na & gap_length <= max_gap,
        income_interp,
        income
      ),
      
      interpolated =
        is_na & gap_length <= max_gap
    ) %>%
    
    select(
      cities_clean,
      year,
      income,
      interpolated
    ) %>%
    
    ungroup()
}

############################################################
# Load Raw Eurostat Data

income_raw <- read_csv(
  
  "data/raw/urb_clivcon__custom_20168663_linear.csv",
  
  locale = locale(encoding = "UTF-8"),
  
  show_col_types = FALSE
)

############################################################
# Clean Variable Names

income_raw <- income_raw %>%
  clean_names()

############################################################
# Standardize City Names

income_clean <- income_raw %>%
  
  mutate(
    
    cities = iconv(
      cities,
      from = "",
      to = "UTF-8"
    ),
    
    cities_clean =
      str_remove(cities, " \\(.*\\)") %>%
      str_trim()
  )

############################################################
# Restrict to Selected Cities

income_cities <- income_clean %>%
  
  filter(
    cities_clean %in% cities_by_name
  )

############################################################
# Restrict to Target Income Indicator

# Eurostat indicator:
# "Median disposable annual household income"

income_indicator <- income_cities %>%
  
  filter(
    str_detect(
      indic_ur,
      "Median disposable annual household income"
    )
  )

############################################################
# Build Long Income Panel

income_panel <- income_indicator %>%
  
  mutate(
    
    year = as.integer(time_period),
    
    income = as.numeric(obs_value)
  ) %>%
  
  select(
    cities_clean,
    year,
    income
  ) %>%
  
  arrange(
    cities_clean,
    year
  )

############################################################
# Interpolate Short Gaps

income_panel_interpolated <- interpolate_income(
  income_panel,
  max_gap = 3
)

############################################################
# Validation Checks

# Ensure dataset is not empty
stopifnot(
  nrow(income_panel_interpolated) > 0
)

# Ensure no negative incomes
stopifnot(
  all(
    income_panel_interpolated$income >= 0,
    na.rm = TRUE
  )
)

# Display remaining missing values
remaining_missing <- sum(
  is.na(income_panel_interpolated$income)
)

message(
  "Remaining missing income observations: ",
  remaining_missing
)

############################################################
# Export Processed Dataset

write_csv(
  
  income_panel_interpolated,
  
  "data/processed/income_panel_interpolated.csv"
)
