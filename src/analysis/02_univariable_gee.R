# Fit a series of univariable logistic generalized estimating equations (GEE) and collect results  

# GEE parameters: 
  # Family: Binomial
  # Link: Logit
  # Working correlation structure: exchangeable or ar1, chosen by minimizing QIC
  # Standard error: robust sandwich estimator
# Collected results:
  # Model specification
  # Quasi fit statistics 
  # Estimated coefficients and standard errors
  # Odds ratios (OR) and 95% confidence intervals
  # Single- and multi-parameter inference for categorical covariates with >2 levels
  # Predictor metadata for forest plots

#### set up ----

options(scipen = 999)

library(tidyverse)
library(geepack)
source(here::here("src/analysis/utils.R"))

alpha_level <- 0.05
conf_level <- 0.95
id_variable <- "record_id"


#### import data ----

source(here::here("src/analysis/01_format_data_for_analysis.R"))


#### fit GEE ----

daily_survey_univariable_gee <- unlist(covariate_columns, use.names = FALSE) %>% 
  purrr::map(
    logistic_gee, 
    outcome = "complete_daily_survey", 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = daily_data
  ) %>% 
  purrr::reduce(bind_rows)

phone_data_yield_univariable_gee <- unlist(covariate_columns, use.names = FALSE) %>% 
  purrr::map(
    logistic_gee, 
    outcome = "sufficient_phone_data_yield", 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = daily_data
  ) %>% 
  purrr::reduce(bind_rows)

# We know phone data yield is substantially lower among Android users, so we also examine other covariate effects when controlling for phone type
phone_data_yield_univariable_gee_adjusted_for_phone_type <- unlist(covariate_columns[covariate_columns != "phone_type"], use.names = FALSE) %>% 
  lapply(function(x) c("phone_type", x)) %>% 
  purrr::map(
    logistic_gee, 
    outcome = "sufficient_phone_data_yield", 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = daily_data
  ) %>% 
  purrr::reduce(bind_rows)

fitbit_data_yield_univariable_gee <- unlist(covariate_columns, use.names = FALSE) %>% 
  purrr::map(
    logistic_gee, 
    outcome = "sufficient_fitbit_data_yield", 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = daily_data
  ) %>% 
  purrr::reduce(bind_rows)


# collect results
univariable_gee_results <- bind_rows(daily_survey_univariable_gee, phone_data_yield_univariable_gee, phone_data_yield_univariable_gee_adjusted_for_phone_type, fitbit_data_yield_univariable_gee)
write_csv(univariable_gee_results, here::here(glue::glue("output/results/rosa_univariable_gee_results_with_further_collapsed_race_education_{gsub('-', '', Sys.Date())}.csv")))
