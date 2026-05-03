############################################################
# 04_superstar_classification.R
#
# Purpose:
# Construct superstar-city classifications using
# housing-price growth and housing-supply elasticity.
#
# Inputs:
# - data/processed/analysis_panel_apartments.csv
#
# Outputs:
# - data/processed/superstar_panel.csv
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
# Helper Function

prep_superstar_data <- function(data,
                                version = "global",
                                sample_type = "incl") {
  
  # Exclude COVID years if requested
  if (sample_type == "excl") {
    data <- data %>%
      filter(year < 2020)
  }
  
  ##########################################################
  # Compute Growth Measures
  
  df <- data %>%
    
    group_by(city) %>%
    
    arrange(year) %>%
    
    mutate(
      grow5_index_ann =
        ((value / lag(value, 4))^(1 / 5)) - 1,
      
      grow5_hu_ann =
        ((total_dwellings /
            lag(total_dwellings, 4))^(1 / 5)) - 1,
      
      p_q5 = grow5_index_ann / grow5_hu_ann,
      
      growth5sum =
        grow5_index_ann + grow5_hu_ann
    ) %>%
    
    ungroup()
  
  ##########################################################
  # Define Classification Thresholds
  
  if (version == "global") {
    
    inelasticity_cutoff <- quantile(
      df$p_q5,
      0.50,
      na.rm = TRUE
    )
    
    year_cutoffs <- df %>%
      
      group_by(year) %>%
      
      summarise(
        demand_cutoff =
          median(growth5sum, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      
      mutate(
        demand_cutoff_lag = lag(demand_cutoff, 4)
      )
    
    df <- df %>%
      
      left_join(year_cutoffs, by = "year") %>%
      
      mutate(
        inelasticity_cutoff = inelasticity_cutoff,
        inelasticity_cutoff_lag = inelasticity_cutoff
      )
  } else {
  
  year_cutoffs <- df %>%
    
    group_by(year) %>%
    
    summarise(
      inelasticity_cutoff =
        quantile(p_q5, 0.50, na.rm = TRUE),
      
      demand_cutoff =
        median(growth5sum, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      inelasticity_cutoff_lag =
        lag(inelasticity_cutoff, 4),
      
      demand_cutoff_lag =
        lag(demand_cutoff, 4)
    )
  
  df <- df %>%
    left_join(year_cutoffs, by = "year")
}

##########################################################
# Superstar Classification

df %>%
  
  group_by(city) %>%
  
  mutate(
    p_q5_lag = lag(p_q5, 4),
    
    growth5sum_lag = lag(growth5sum, 4),
    
    superstar = ifelse(
      p_q5 >= inelasticity_cutoff &
        growth5sum >= demand_cutoff &
        p_q5_lag >= inelasticity_cutoff_lag &
        growth5sum_lag >= demand_cutoff_lag,
      1,
      0
    )
  ) %>%
  
  ungroup() %>%
  
  mutate(
    growth_window = case_when(
      year == 1999 ~ "1995-1999",
      year == 2004 ~ "2000-2004",
      year == 2009 ~ "2005-2009",
      year == 2014 ~ "2010-2014",
      year == 2019 ~ "2015-2019",
      year == 2024 ~ "2020-2024",
      TRUE ~ NA_character_
    )
  )
}

############################################################
# Load Analysis Panel

analysis_panel <- read_csv(
  "data/processed/analysis_panel_apartments.csv",
  show_col_types = FALSE
)

############################################################
# Build Superstar Panels

superstar_panel_global <- prep_superstar_data(
  analysis_panel,
  version = "global",
  sample_type = "incl"
)

superstar_panel_period <- prep_superstar_data(
  analysis_panel,
  version = "period",
  sample_type = "incl"
)

############################################################
# Export Processed Superstar Panels

write_csv(
  superstar_panel_global,
  "data/processed/superstar_panel_global.csv"
)

write_csv(
  superstar_panel_period,
  "data/processed/superstar_panel_period.csv"
)