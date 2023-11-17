# 10. Assemble datasets for analysis

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### between-subjects data ----

between_subjects_files <- list(
  # covariates
  demographics = "data/processed/rosa_demographic_covariates.csv",
  baseline_promis_propr = "data/processed/rosa_baseline_promis_propr_scores.csv",
  clinical_covariates = "data/processed/rosa_clinical_covariates.csv",
  # aggregated outcomes
  daily_survey_adherence = "data/processed/rosa_daily_survey_adherence_aggregated.csv",
  device_adherence = "data/processed/rosa_device_adherence_aggregated.csv",
  weekly_survey_adherence = "data/processed/rosa_weekly_survey_adherence_aggregated.csv"
)

between_subjects_data <- between_subjects_files %>%
  lapply(here::here) %>%
  purrr::map(read_csv) %>%
  purrr::reduce(full_join, by = "record_id") %>%
  select(record_id, study_completion_status, everything())

write_csv(between_subjects_data, here::here("data/processed/rosa_between_subjects_data_for_analysis.csv"))


#### within-subjects data ----

within_subjects_files <- list(
  # day-level covariates
  study_dates = "data/processed/rosa_study_dates_and_indexes.csv",
  timevarying_covariates = "data/processed/rosa_timevarying_covariates.csv",
  # day-level outcomes
  daily_survey_adherence = "data/processed/rosa_daily_survey_adherence.csv",
  device_adherence = "data/processed/rosa_device_adherence.csv",
  # week-level outcomes
  weekly_survey_adherence = "data/processed/rosa_weekly_survey_adherence.csv"
)

within_subjects_data_daily <- within_subjects_files[!grepl("weekly", names(within_subjects_files))] %>%
  lapply(here::here) %>%
  purrr::map(read_csv) %>%
  purrr::reduce(full_join, by = c("record_id", "date")) %>%
  select(-c(days_in_study, ends_with(".x"), ends_with(".y"))) %>%
  left_join(between_subjects_data, by = "record_id") %>%
  select(-c(contains("days_with"), contains("weeks")))

within_subjects_data_weekly <- within_subjects_files[grepl("weekly", names(within_subjects_files))] %>%
  lapply(here::here) %>%
  purrr::map(read_csv) %>%
  purrr::reduce(full_join, by = "record_id") %>%
  select(-weeks_in_study) %>%
  left_join(between_subjects_data, by = "record_id") %>%
  select(-c(days_in_study, contains("days_with"), contains("weeks_with")))
  
write_csv(within_subjects_data_daily, here::here("data/processed/rosa_within_subjects_daily_data_for_analysis.csv"))  
write_csv(within_subjects_data_weekly, here::here("data/processed/rosa_within_subjects_weekly_data_for_analysis.csv"))  
