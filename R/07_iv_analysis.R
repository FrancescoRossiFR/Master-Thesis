############################################################
# 06_iv_analysis.R
# 
# Purpose: IV Estimation of Supply Inelasticity using 
# WWII Destruction Shares as an instrument.
# Formatted for LaTeX tabular output.
############################################################

library(tidyverse)
library(fixest)

# ----------------------------- 1. Load Data -----------------------------
static_data_raw <- read_csv("data/processed/superstar_panel_global.csv", show_col_types = FALSE)

# ----------------------------- 2. Prepare Data -----------------------------
iv_data <- static_data_raw %>%
  filter(
    !is.na(growth_window),
    growth_window != "1995-1999"
  ) %>%
  mutate(
    log_price = log(value),
    year_f = as.factor(year)
  ) %>%
  filter(!is.na(p_q5), !is.na(destruction_share))

# ----------------------------- 3. IV Model -----------------------------
# We estimate the IV model once. fixest allows us to extract 
# both stages from this single object.
iv_model <- feols(log_price ~ east_germany | year_f | p_q5 ~ destruction_share, 
                  data = iv_data, 
                  cluster = ~city)

# Extract the stages
# stage = 1: First stage (p_q5 ~ destruction_share)
# stage = 2: Second stage (log_price ~ instrumented p_q5)
iv_results <- summary(iv_model, stage = 1:2)

# ----------------------------- 4. Export Table -----------------------------

# Define a dictionary for professional variable names
var_dict <- c(
  "destruction_share" = "WWII Destruction",
  "p_q5"              = "Housing Inelasticity",
  "east_germany"      = "east",
  "log_price"         = "log_price",
  "fit_p_q5"          = "Housing Inelasticity" # Label for instrumented var in 2nd stage
)

# Customizing the etable to match your template
etable(iv_results,
       title    = "IV Regression Results for Housing Inelasticity and Prices",
       label    = "tab:iv_results",
       headers  = list("IV stages" = c("First", "Second")),
       dict     = var_dict,
       # Choose specific fit statistics: n (obs), ivf1 (First stage F), ivwald2 (Hausman)
       fitstat  = ~ n + ivf1 + ivwald2,
       # Format to match your style
       style.tex = style.tex("aer", model.format = "(1)"),
       notes    = c("Clustered (city) standard-errors in parentheses.", "***: 0.01, **: 0.05, *: 0.1"),
       file     = "output/tables/appendix_iv_results.tex",
       replace  = TRUE)