# Fit a series multivariable logistic generalized estimating equations (GEE) and collect results  

# Purposeful subset models evaluated for each outcome
  # (1) multivariable model with purposeful subset of predictors
  # (2) multivariable model with purposeful subset of predictors, also including phone type for phone data yield outcome only

# GEE parameters: 
  # Family: Binomial
  # Link: Logit
  # Working correlation structure: exchangeable
  # Standard error: robust sandwich estimator

# Collected results:
  # Model specification
  # Quasi fit statistics 
  # Estimated coefficients and standard errors
  # Odds ratios (OR) and 95% confidence intervals
  # Single-, and multi-parameter inference for categorical covariates with >2 levels

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


#### purposeful subset multivariable models ----

daily_survey_purposeful_subset_multivariable_gee <- list(purposeful_covariate_sets[[2]]) %>%
  purrr::map(
    logistic_gee, 
    outcome = "complete_daily_survey", 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = daily_data
  ) %>% 
  purrr::reduce(bind_rows)

phone_data_yield_purposeful_subset_multivariable_gee <- purposeful_covariate_sets %>% 
  purrr::map(
    logistic_gee, 
    outcome = "sufficient_phone_data_yield", 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = daily_data
  ) %>% 
  purrr::reduce(bind_rows)

fitbit_data_yield_purposeful_subset_multivariable_gee <- list(purposeful_covariate_sets[[2]]) %>% 
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
purposeful_subset_multivariable_gee_results <- bind_rows(daily_survey_purposeful_subset_multivariable_gee, phone_data_yield_purposeful_subset_multivariable_gee, fitbit_data_yield_purposeful_subset_multivariable_gee)
write_csv(purposeful_subset_multivariable_gee_results, here::here(glue::glue("output/results/rosa_purposeful_subset_multivariable_gee_results_with_further_collapsed_race_education_{gsub('-', '', Sys.Date())}.csv")))

