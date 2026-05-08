############################################################
# 05_regressions_sf.R
############################################################

############################################################
# Libraries

required_packages <- c(
  "tidyverse",
  "modelsummary",
  "sandwich",
  "lmtest"
)

invisible(lapply(required_packages, library, character.only = TRUE))

############################################################
# Load Data (SINGLE FAMILY)

superstar_static <- read_csv(
  "data/processed/superstar_panel_global_sf.csv",
  show_col_types = FALSE
)

superstar_lagged <- read_csv(
  "data/processed/superstar_panel_period_sf.csv",
  show_col_types = FALSE
)

############################################################
# Clustered SE helper

cluster_se <- function(model, cluster) {
  vcovCL(model, cluster = cluster)
}

############################################################
# Regression Function

run_superstar_models <- function(data, sample = "incl") {
  
  reg_data <- data %>%
    filter(
      !is.na(growth_window),
      growth_window != "1995-1999"
    )
  
  # Exclude COVID Period
  if (sample == "excl") {
    reg_data <- reg_data %>%
      filter(year < 2020)
  }
  
  # Income transformations
  reg_data <- reg_data %>%
    group_by(year) %>%
    mutate(
      national_median_income = median(median_income, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      log_mean_income = log(mean_income),
      log_income_relative = log(median_income / national_median_income),
      log_income_dispersion = log(mean_income / median_income)
    )
  
  # Models
  list(
    hv_pooled = lm(log(value) ~ superstar_ever + as.factor(growth_window), data = reg_data),
    hv_timevarying = lm(log(value) ~ superstar + as.factor(growth_window) + city, data = reg_data),
    income_mean_pooled = lm(log_mean_income ~ superstar_ever + as.factor(growth_window), data = reg_data),
    income_mean_timevarying = lm(log_mean_income ~ superstar + as.factor(growth_window) + city, data = reg_data),
    income_rel_pooled = lm(log_income_relative ~ superstar_ever + as.factor(growth_window), data = reg_data),
    income_rel_timevarying = lm(log_income_relative ~ superstar + as.factor(growth_window) + city, data = reg_data),
    income_disp_pooled = lm(log_income_dispersion ~ superstar_ever + as.factor(growth_window), data = reg_data),
    income_disp_timevarying = lm(log_income_dispersion ~ superstar + as.factor(growth_window) + city, data = reg_data)
  )
}

############################################################
# Estimate Models

models_static_incl <- run_superstar_models(superstar_static, "incl")
models_static_excl <- run_superstar_models(superstar_static, "excl")
models_lagged_incl <- run_superstar_models(superstar_lagged, "incl")
models_lagged_excl <- run_superstar_models(superstar_lagged, "excl")

############################################################
# Common settings

coef_map <- c(
  "superstar_ever" = "Superstar (Ever)",
  "superstar" = "Superstar",
  "(Intercept)" = "Constant"
)

stars <- c('+'=.1, '*'=.05, '**'=.01, '***'=.001)

############################################################
# TABLE 1: HOUSE PRICES (SF)

modelsummary(
  list(
    "Static Pooled" = models_static_incl$hv_pooled,
    "Static Time"   = models_static_incl$hv_timevarying,
    "Lagged Pooled" = models_lagged_incl$hv_pooled,
    "Lagged Time"   = models_lagged_incl$hv_timevarying
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov = list(
    cluster_se(models_static_incl$hv_pooled, ~city),
    cluster_se(models_static_incl$hv_timevarying, ~city),
    cluster_se(models_lagged_incl$hv_pooled, ~city),
    cluster_se(models_lagged_incl$hv_timevarying, ~city)
  ),
  coef_map = coef_map,
  stars = stars,
  gof_map = c("nobs", "adj.r.squared"),
  add_rows = tribble(
    ~term, ~M1, ~M2, ~M3, ~M4,
    "Identification", "Static", "Static", "Lagged", "Lagged",
    "Fixed effects", "Period", "Period + City", "Period", "Period + City"
  ),
  output = "output/tables/house_prices_sf.tex"
)

############################################################
# INCOME TABLES (SF)

# Relative Income
modelsummary(
  list(
    "Static Pooled" = models_static_incl$income_rel_pooled,
    "Static Time"   = models_static_incl$income_rel_timevarying,
    "Lagged Pooled" = models_lagged_incl$income_rel_pooled,
    "Lagged Time"   = models_lagged_incl$income_rel_timevarying
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov = list(
    cluster_se(models_static_incl$income_rel_pooled, ~city),
    cluster_se(models_static_incl$income_rel_timevarying, ~city),
    cluster_se(models_lagged_incl$income_rel_pooled, ~city),
    cluster_se(models_lagged_incl$income_rel_timevarying, ~city)
  ),
  coef_map = coef_map,
  stars = stars,
  gof_map = c("nobs", "rmse"),
  output = "output/tables/income_relative_sf.tex"
)

# Income Dispersion
modelsummary(
  list(
    "Static Pooled" = models_static_incl$income_disp_pooled,
    "Static Time"   = models_static_incl$income_disp_timevarying,
    "Lagged Pooled" = models_lagged_incl$income_disp_pooled,
    "Lagged Time"   = models_lagged_incl$income_disp_timevarying
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov = list(
    cluster_se(models_static_incl$income_disp_pooled, ~city),
    cluster_se(models_static_incl$income_disp_timevarying, ~city),
    cluster_se(models_lagged_incl$income_disp_pooled, ~city),
    cluster_se(models_lagged_incl$income_disp_timevarying, ~city)
  ),
  coef_map = coef_map,
  stars = stars,
  gof_map = c("nobs", "rmse"),
  output = "output/tables/income_dispersion_sf.tex"
)

# Mean Income
modelsummary(
  list(
    "Static Pooled" = models_static_incl$income_mean_pooled,
    "Static Time"   = models_static_incl$income_mean_timevarying,
    "Lagged Pooled" = models_lagged_incl$income_mean_pooled,
    "Lagged Time"   = models_lagged_incl$income_mean_timevarying
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov = list(
    cluster_se(models_static_incl$income_mean_pooled, ~city),
    cluster_se(models_static_incl$income_mean_timevarying, ~city),
    cluster_se(models_lagged_incl$income_mean_pooled, ~city),
    cluster_se(models_lagged_incl$income_mean_timevarying, ~city)
  ),
  coef_map = coef_map,
  stars = stars,
  gof_map = c("nobs", "rmse"),
  output = "output/tables/income_mean_sf.tex"
)