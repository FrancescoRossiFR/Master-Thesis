############################################################
# 01_prepare_housing_data.R
#
# Purpose:
# Construct German housing market panel datasets
# using GREIX price indices, construction statistics,
# and WWII destruction data.
#
# Inputs:
# - data/raw/GREIX_real.xlsx
# - data/raw/construction_kreis.csv
# - data/raw/stock95.csv
# - data/raw/destruction_instrument.txt
#
# Outputs:
# - data/processed/panel_apartments.csv
# - data/processed/panel_single_family.csv
#
############################################################

############################################################
# Libraries

required_packages <- c(
  "tidyverse",
  "readxl"
)

invisible(lapply(required_packages, library, character.only = TRUE))

############################################################
# Constants

# Cities included in the analysis
cities_by_name <- c(
  "Berlin", "Hamburg", "München", "Köln",
  "Frankfurt", "Stuttgart", "Düsseldorf",
  "Dortmund", "Duisburg", "Bonn",
  "Münster", "Wiesbaden", "Lübeck",
  "Potsdam", "Erfurt", "Karlsruhe"
)

# Manual geographic ID corrections
city_code_map <- c(
  "14713" = "Leipzig",
  "14511" = "Chemnitz",
  "14612" = "Dresden",
  "06412" = "Frankfurt",
  "08212" = "Karlsruhe"
)

city_regex <- str_c(cities_by_name, collapse = "|")

############################################################
# City name harmonization table

city_translation <- tibble(
  city_en = c(
    "Berlin", "Hamburg", "Munich", "Cologne",
    "Frankfurt", "Stuttgart", "Duesseldorf",
    "Leipzig", "Dortmund", "Dresden",
    "Duisburg", "Bonn", "Muenster",
    "Wiesbaden", "Chemnitz", "Luebeck",
    "Potsdam", "Erfurt", "Karlsruhe"
  ),
  
  city_de = c(
    "Berlin", "Hamburg", "München", "Köln",
    "Frankfurt", "Stuttgart", "Düsseldorf",
    "Leipzig", "Dortmund", "Dresden",
    "Duisburg", "Bonn", "Münster",
    "Wiesbaden", "Chemnitz", "Lübeck",
    "Potsdam", "Erfurt", "Karlsruhe"
  )
)

############################################################
# Helper Functions

# ----------------------------------------------------------
# Build GREIX price panel by property type
# ----------------------------------------------------------

build_price_panel <- function(greix_data, pattern) {
  
  greix_data %>%
    
    filter(
      str_detect(
        series,
        regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "year",
      values_to = "value"
    ) %>%
    
    mutate(
      year = as.integer(year),
      
      city = str_trim(
        str_remove(series, "\\s*\\(.*\\)")
      )
    ) %>%
    
    select(year, city, value) %>%
    
    left_join(
      city_translation,
      by = c("city" = "city_en")
    ) %>%
    
    mutate(city = city_de) %>%
    
    select(year, city, value)
}

# ----------------------------------------------------------
# Build cumulative housing stock panel
# ----------------------------------------------------------

build_housing_stock_panel <- function(construction_data,
                                      stock_data) {
  
  construction_data %>%
    
    left_join(stock_data, by = "geographic_id") %>%
    
    group_by(city) %>%
    
    arrange(year) %>%
    
    mutate(
      total_buildings =
        stock_build_95 +
        lag(cumsum(build_flow), default = 0),
      
      total_dwellings =
        stock_dwell_95 +
        lag(cumsum(dwell_flow), default = 0)
    ) %>%
    
    ungroup() %>%
    
    select(
      year,
      city,
      total_buildings,
      total_dwellings,
      build_flow,
      dwell_flow
    )
}

# ----------------------------------------------------------
# Merge housing stock with price data and controls
# ----------------------------------------------------------

merge_panel_data <- function(price_data,
                             housing_stock,
                             destruction_data) {
  
  housing_stock %>%
    
    mutate(year = as.integer(year)) %>%
    
    left_join(
      
      price_data %>%
        mutate(
          year = as.integer(year),
          city = str_remove(city, ",.*")
        ),
      
      by = c("year", "city")
    ) %>%
    
    left_join(
      destruction_data,
      by = "city"
    )
}

############################################################
# Load GREIX Index Data

greix_raw <- read_excel(
  "data/raw/GREIX_real.xlsx"
)

greix_long <- greix_raw %>%
  
  column_to_rownames("year") %>%
  
  t() %>%
  
  as.data.frame() %>%
  
  rownames_to_column("series") %>%
  
  as_tibble()

############################################################
# Build Price Panels

panel_apartment_prices <- build_price_panel(
  greix_long,
  pattern = "apartment"
)

panel_single_family_prices <- build_price_panel(
  greix_long,
  pattern = "single[- ]family"
)

############################################################
# Load Construction Completion Data

construction_data <- read_delim(
  
  "data/raw/construction_kreis.csv",
  
  delim = ";",
  
  skip = 10,
  
  col_names = c(
    "year",
    "geographic_id",
    "region_name",
    "build_flow",
    "build_flow_1_dwelling",
    "build_flow_2_dwellings",
    "build_flow_3plus",
    "dwell_flow",
    "dwell_flow_1",
    "dwell_flow_2",
    "dwell_flow_3plus",
    "living_area_1000sqm_flow"
  ),
  
  locale = locale(encoding = "ISO-8859-1"),
  
  show_col_types = FALSE
) %>%
  
  mutate(
    
    region_name = str_trim(region_name),
    
    city = coalesce(
      city_code_map[geographic_id],
      str_extract(region_name, city_regex)
    ),
    
    is_kreisfrei =
      str_detect(region_name, "kreisfreie Stadt")
  ) %>%
  
  filter(!is.na(city)) %>%
  
  group_by(year, city) %>%
  
  summarise(
    
    geographic_id =
      geographic_id[which.max(is_kreisfrei)],
    
    build_flow =
      sum(as.numeric(build_flow), na.rm = TRUE),
    
    dwell_flow =
      sum(as.numeric(dwell_flow), na.rm = TRUE),
    
    .groups = "drop"
  )

############################################################
# Load 1995 Housing Stock Data

housing_stock_1995 <- read_delim(
  
  "data/raw/stock95.csv",
  
  delim = ";",
  
  skip = 10,
  
  col_names = c(
    "year",
    "geographic_id",
    "region_name",
    "buildings_total",
    "buildings_1_dwelling",
    "buildings_2_dwellings",
    "living_area_1000sqm",
    "dwellings_total",
    "dwellings_1",
    "dwellings_2",
    "dwellings_3",
    "dwellings_4",
    "dwellings_5",
    "dwellings_6",
    "dwellings_7",
    "dwellings_7plus"
  ),
  
  locale = locale(encoding = "ISO-8859-1"),
  
  show_col_types = FALSE
) %>%
  
  mutate(
    
    year = 1995,
    
    across(
      c(buildings_total, dwellings_total),
      ~ as.numeric(str_replace(.x, ",", "."))
    ),
    
    city = coalesce(
      city_code_map[geographic_id],
      str_remove(
        str_remove(region_name, ",.*"),
        " am Main"
      )
    )
  ) %>%
  
  filter(
    !str_detect(region_name, "Frankfurt") |
      geographic_id == "06412"
  ) %>%
  
  select(
    geographic_id,
    stock_build_95 = buildings_total,
    stock_dwell_95 = dwellings_total
  )

############################################################
# Build Housing Stock Time Series

housing_stock_panel <- build_housing_stock_panel(
  construction_data,
  housing_stock_1995
)

############################################################
# Load WWII Destruction Instrument

destruction_data <- read_delim(
  
  "data/raw/destruction_instrument.txt",
  
  delim = ",",
  
  show_col_types = FALSE
) %>%
  
  mutate(
    
    city = str_trim(city),
    
    city = str_remove(city, ",.*")
  ) %>%
  
  select(
    city,
    destroyed_share_1939,
    east_germany
  ) %>%
  
  rename(
    destruction_share = destroyed_share_1939
  )

############################################################
# Build Final Apartment Panel

panel_apartments <- merge_panel_data(
  
  price_data = panel_apartment_prices,
  
  housing_stock = housing_stock_panel,
  
  destruction_data = destruction_data
) %>%
  
  # Leipzig excluded due to incomplete coverage
  filter(city != "Leipzig")

############################################################
# Build Final Single-Family Panel

panel_single_family <- merge_panel_data(
  
  price_data = panel_single_family_prices,
  
  housing_stock = housing_stock_panel,
  
  destruction_data = destruction_data
) %>%
  
  # Exclusions due to incomplete series coverage
  filter(
    !city %in% c("München")
  )

############################################################
# Validation Checks

stopifnot(
  nrow(panel_apartments) > 0
)

stopifnot(
  nrow(panel_single_family) > 0
)

stopifnot(
  all(
    panel_apartments$total_dwellings >= 0,
    na.rm = TRUE
  )
)

############################################################
# Export Processed Datasets

write_csv(
  panel_apartments,
  "data/processed/panel_apartments.csv"
)

write_csv(
  panel_single_family,
  "data/processed/panel_single_family.csv"
)
