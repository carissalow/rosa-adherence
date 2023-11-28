library(tidyverse)

#### helper function ----

#' Apply data transformations for modeling: mean-center continuous variables, set reference levels for categorical variables
#' 
#' @param data A data frame
#' @param mean_age Numeric. The mean age of the sample, required to mean-center age. Can override by passing 0
#' @return A data frame containing mean-centered continuous variables and factorized categorical variables with specified reference level
apply_data_transformations <- function(data, mean_age = NULL) {
  
  if (is.null(mean_age)) {
    stop("You must specify a value for `mean_age` in order to mean-center age")
  }
  
  data <- data %>%
    mutate(
      record_id = as.factor(record_id),
      across(contains("date"), as.Date),
      # mean-center age for interpretability
      age_years = age_years - mean_age,
      # rescale baseline PROMIS preference scores from [0, 1] to [0, 10] for interpretability
      across(starts_with("baseline_promis"), function(x) x*10),
      # set reference categories for categorical variables
      study_completion_status = factor(study_completion_status, levels = c("Completed study", "Withdrawn")),
      gender = factor(gender, levels = c("Female", "Male")),
      race_collapsed = factor(race_collapsed, levels = c("White/Caucasian", "Black/African American", "Other")),
      education_collapsed = factor(education_collapsed, levels = c("Bachelor's degree", "High school or less", "Some college", "Graduate degree")),
      phone_type = factor(phone_type, levels = c("iPhone", "Android")),
      forhp_rural_zipcode = factor(forhp_rural_zipcode, levels = c("No", "Yes")),
      insurance_plan_type_collapsed = factor(insurance_plan_type_collapsed, levels = c("Private", "Mixed", "Public")),
      cancer_type_collapsed = factor(cancer_type_collapsed, levels = c("GI", "Pancreas", "Breast", "Urogenital", "Other")),
      cancer_stage_four = factor(cancer_stage_four, levels = c("No", "Yes"))
    )
  
  if ("weekend_day" %in% names(data)) {
    data <- data %>%
      mutate(
        # set reference categories for categorical variables
        weekend_day = factor(weekend_day, levels = c("No", "Yes"))
      )
  }
  
  return(data)
}

#' Apply further collapsing of race and/or education categories due to sparsity 
#' 
#' @param data A data frame
#' @param race Logical; race categories will be further collapsed or left as-is if TRUE and left as-is otherwise
#' @param education Logical; education categories will be further collapsed or left as-is if TRUE and left as-is otherwise
#' @return A data frame containing further-collapsed and factorized categorical variables with specified reference level
further_collapse_sparse_categories <- function(data, race = TRUE, education = TRUE) {
  if (race) {
    data <- data %>%
      mutate(
        race_collapsed = case_when(
          race_collapsed %in% c("Black/African American", "Other") ~ "Not White/Caucasian",
          TRUE ~ race_collapsed
        ),
        race_collapsed = factor(race_collapsed, levels = c("White/Caucasian", "Not White/Caucasian"))
      )
  }
  if (education) {
    data <- data %>%
      mutate(
        education_collapsed = case_when(
          education_collapsed %in% c("High school or less", "Some college") ~ "Less than college degree",
          education_collapsed %in% c("Bachelor's degree", "Graduate degree") ~ "College degree or higher",
          TRUE ~ NA_character_
        ),
        education_collapsed = factor(education_collapsed, levels = c("College degree or higher", "Less than college degree"))
      )
  }
  return(data)
}



#### import data ----
    
daily_data <- read_csv(here::here("data/processed/rosa_within_subjects_daily_data_for_analysis.csv"))

    
#### specify outcomes and covariates of interest for analysis ----
    
outcome_columns <- c("complete_daily_survey", "sufficient_phone_data_yield", "sufficient_fitbit_data_yield")
    
covariate_columns <- list(
  # time-invariant
  study_covariate_columns = c("study_completion_status"),
  demographic_covariate_columns = c("age_years", "gender", "race_collapsed", "education_collapsed"),
  ses_covariate_columns = c("forhp_rural_zipcode", "insurance_plan_type_collapsed"),
  tech_literacy_covariate_columns = c("phone_type"), 
  baseline_qol_covariate_columns = grep("baseline_promis", colnames(daily_data), value = TRUE),
  clinical_covariate_columns = c("cancer_stage_four", "cancer_type_collapsed", "months_since_diagnosis_at_enrollment"),
  # time-varying
  timevarying_covariate_columns = c("study_day", "weekend_day", "days_since_last_known_chemotherapy_treatment")
)

covariates_excluded_from_purposeful_subset <- c(
  "insurance_plan_type_collapsed", "cancer_type_collapsed", "months_since_diagnosis_at_enrollment", "study_completion_status",
  covariate_columns$baseline_qol_covariate_columns[!grepl("cognition|depression", covariate_columns$baseline_qol_covariate_columns)]
)
  

#### define sets of covariates for multivariable models ----

# full covariate sets
full_covariates <- unlist(covariate_columns, use.names = FALSE)
full_covariates_with_propr_subscales <- full_covariates[!full_covariates == "baseline_promis_preference_propr"]
full_covariates_with_propr_overall <- full_covariates[!(grepl("baseline_promis", full_covariates) & !full_covariates == "baseline_promis_preference_propr")]

full_covariate_sets <- list(full_covariates, full_covariates_with_propr_subscales, full_covariates_with_propr_overall)

# purposeful subsets
purposeful_covariates <- full_covariates[!full_covariates %in% covariates_excluded_from_purposeful_subset]
purposeful_covariates_without_phone_type <- purposeful_covariates[purposeful_covariates != "phone_type"]

purposeful_covariate_sets <- list(purposeful_covariates, purposeful_covariates_without_phone_type)
 

#### format data for analysis ----
    
mean_age_of_sample <- daily_data %>%
  dplyr::select(record_id, age_years) %>%
  distinct() %>%
  summarize(mean(age_years, na.rm = TRUE)) %>%
  pull()
    
daily_data <- daily_data %>%
  dplyr::select(record_id, date, any_of(outcome_columns), any_of(unlist(covariate_columns, use.names = FALSE))) %>%
  apply_data_transformations(mean_age = mean_age_of_sample) %>%
  further_collapse_sparse_categories(race = TRUE, education = TRUE)

