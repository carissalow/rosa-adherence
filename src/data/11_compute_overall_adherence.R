# 11. Compute a metric representing overall adherence to survey, phone, and Fitbit data collection

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### import data ----

within_subjects_data_daily <- read_csv(here::here("data/processed/rosa_within_subjects_daily_data_for_analysis.csv"))  

#### compute overall adherence ----

overall_adherence <- within_subjects_data_daily %>%
  select(record_id, date, study_day, complete_daily_survey, sufficient_phone_data_yield, sufficient_fitbit_data_yield) %>%
  mutate(
    overall_adherence_type = case_when(
      complete_daily_survey == 0 & sufficient_phone_data_yield == 0 & sufficient_fitbit_data_yield == 0 ~ "None",
      complete_daily_survey == 1 & sufficient_phone_data_yield == 0 & sufficient_fitbit_data_yield == 0 ~ "Survey only",
      complete_daily_survey == 0 & sufficient_phone_data_yield == 1 & sufficient_fitbit_data_yield == 0 ~ "Phone only",
      complete_daily_survey == 0 & sufficient_phone_data_yield == 0 & sufficient_fitbit_data_yield == 1 ~ "Fitbit only",
      complete_daily_survey == 1 & sufficient_phone_data_yield == 1 & sufficient_fitbit_data_yield == 0 ~ "Survey & phone",
      complete_daily_survey == 1 & sufficient_phone_data_yield == 0 & sufficient_fitbit_data_yield == 1 ~ "Survey & Fitbit",
      complete_daily_survey == 0 & sufficient_phone_data_yield == 1 & sufficient_fitbit_data_yield == 1 ~ "Phone & Fitbit",
      complete_daily_survey == 1 & sufficient_phone_data_yield == 1 & sufficient_fitbit_data_yield == 1 ~ "Survey & phone & Fitbit",
    ),
    overall_adherence_type = factor(
      overall_adherence_type, 
      levels = c("None", "Survey only", "Phone only", "Fitbit only", "Survey & phone", "Survey & Fitbit", "Phone & Fitbit", "Survey & phone & Fitbit")
    ),
    overall_adherence_num_streams = rowSums(.[, c("complete_daily_survey", "sufficient_phone_data_yield", "sufficient_fitbit_data_yield")])
  )

write_csv(overall_adherence, here::here("data/processed/rosa_overall_adherence.csv"))