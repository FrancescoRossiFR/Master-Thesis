############################################################
# 05_regressions.R
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
# Load Data

superstar_static <- read_csv(
  "data/processed/superstar_panel_global.csv",
  show_col_types = FALSE
)

superstar_lagged <- read_csv(
  "data/processed/superstar_panel_period.csv",
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
  
  ##########################################################
  # EXCLUDE COVID PERIOD (adjust if needed)
  
  if (sample == "excl") {
    reg_data <- reg_data %>%
      filter(year < 2020)   # <-- change if your cutoff differs
  }
  
  ##########################################################
  # Income transformations
  
  reg_data <- reg_data %>%
    
    group_by(year) %>%
    
    mutate(
      national_median_income =
        median(median_income, na.rm = TRUE)
    ) %>%
    
    ungroup() %>%
    
    mutate(
      log_mean_income = log(mean_income),
      
      log_income_relative =
        log(median_income / national_median_income),
      
      log_income_dispersion =
        log(mean_income / median_income)
    )
  
    reg_data <- reg_data %>%
      mutate(
        city = droplevels(as.factor(city)),
        growth_window = droplevels(as.factor(growth_window))
    )
  ##########################################################
  # Models
  
  list(
    
    # House values
    hv_pooled = lm(
      log(value) ~ superstar_ever + as.factor(growth_window),
      data = reg_data
    ),
    
    hv_timevarying = lm(
      log(value) ~ superstar + as.factor(growth_window) + city,
      data = reg_data
    ),
    
    # Mean income
    income_mean_pooled = lm(
      log_mean_income ~ superstar_ever + as.factor(growth_window),
      data = reg_data
    ),
    
    income_mean_timevarying = lm(
      log_mean_income ~ superstar + as.factor(growth_window) + city,
      data = reg_data
    ),
    
    # Income relative
    income_rel_pooled = lm(
      log_income_relative ~ superstar_ever + as.factor(growth_window),
      data = reg_data
    ),
    
    income_rel_timevarying = lm(
      log_income_relative ~ superstar + as.factor(growth_window) + city,
      data = reg_data
    ),
    
    # Income dispersion
    income_disp_pooled = lm(
      log_income_dispersion ~ superstar_ever + as.factor(growth_window),
      data = reg_data
    ),
    
    income_disp_timevarying = lm(
      log_income_dispersion ~ superstar + as.factor(growth_window) + city,
      data = reg_data
    )
  )
}

############################################################
# Estimate ALL MODELS (8 total)

models_static_incl <- run_superstar_models(superstar_static, "incl")
models_static_excl <- run_superstar_models(superstar_static, "excl")

models_lagged_incl <- run_superstar_models(superstar_lagged, "incl")
models_lagged_excl <- run_superstar_models(superstar_lagged, "excl")

############################################################
# Common settings

coef_map <- c(
  "superstar_ever" = "Ever-Superstar City",
  "superstar" = "Superstar",
  "(Intercept)" = "Constant"
)

stars <- c('+'=.1, '*'=.05, '**'=.01, '***'=.001)

############################################################
# TABLE 1: HOUSE PRICES (FULL 8 MODELS)

modelsummary(
  
  list(
    "Static Pooled (Incl)" = models_static_incl$hv_pooled,
    "Static Time (Incl)"   = models_static_incl$hv_timevarying,
    "Lagged Pooled (Incl)" = models_lagged_incl$hv_pooled,
    "Lagged Time (Incl)"   = models_lagged_incl$hv_timevarying,
    
    "Static Pooled (Excl)" = models_static_excl$hv_pooled,
    "Static Time (Excl)"   = models_static_excl$hv_timevarying,
    "Lagged Pooled (Excl)" = models_lagged_excl$hv_pooled,
    "Lagged Time (Excl)"   = models_lagged_excl$hv_timevarying
  ),
  
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  
  vcov = list(
    cluster_se(models_static_incl$hv_pooled, ~city),
    cluster_se(models_static_incl$hv_timevarying, ~city),
    cluster_se(models_lagged_incl$hv_pooled, ~city),
    cluster_se(models_lagged_incl$hv_timevarying, ~city),
    
    cluster_se(models_static_excl$hv_pooled, ~city),
    cluster_se(models_static_excl$hv_timevarying, ~city),
    cluster_se(models_lagged_excl$hv_pooled, ~city),
    cluster_se(models_lagged_excl$hv_timevarying, ~city)
  ),
  
  coef_map = coef_map,
  stars = stars,
  
  gof_map = c("nobs", "adj.r.squared"),
  
  add_rows = tribble(
    ~term, ~M1, ~M2, ~M3, ~M4, ~M5, ~M6, ~M7, ~M8,
    "COVID", "Incl", "Incl", "Incl", "Incl", "Excl", "Excl", "Excl", "Excl",
    "Classification", "Static", "Static", "Lagged", "Lagged", "Static", "Static", "Lagged", "Lagged",
    "Fixed effects", "Period", "Period + City", "Period", "Period + City", "Period", "Period + City", "Period", "Period + City"
  ),
  
  output = "output/tables/house_prices_full.tex"
)

############################################################
# TABLE 2: INCOME RELATIVE

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
  
  add_rows = tribble(
    ~term, ~M1, ~M2, ~M3, ~M4,
    "Classification", "Static", "Static", "Lagged", "Lagged",
    "Fixed effects", "Period", "Period + City", "Period", "Period + City"
  ),
  
  output = "output/tables/income_relative.tex"
)

############################################################
# TABLE 3: INCOME DISPERSION

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
  
  add_rows = tribble(
    ~term, ~M1, ~M2, ~M3, ~M4,
    "Classification", "Static", "Static", "Lagged", "Lagged",
    "Fixed effects", "Period", "Period + City", "Period", "Period + City"
  ),
  
  output = "output/tables/income_dispersion.tex"
)

############################################################
# TABLE 4: MEAN INCOME

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
  
  add_rows = tribble(
    ~term, ~M1, ~M2, ~M3, ~M4,
    "Classification", "Static", "Static", "Lagged", "Lagged",
    "Fixed effects", "Period", "Period + City", "Period", "Period + City"
  ),
  
  output = "output/tables/income_mean.tex"
)