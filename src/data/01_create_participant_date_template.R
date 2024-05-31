# 1. Convert dates to study day index

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### import data ----

excluded_patterns <- "Repeat"

study_dates <- read_file_with_clean_names(file = "data/raw/ROSA-StartAndEndDates_DATA_LABELS_2023-06-12_1550.csv", excluded_column_patterns = excluded_patterns)
excluded_dates <- read_file_with_clean_names(file = "data/raw/ROSA-ExcludedDates_2023-06-12_1701.csv")

#### convert dates to indexes ----

study_dates_and_indexes <- study_dates %>%
  full_join(excluded_dates, by = "record_id") %>%
  mutate(across(ends_with("_date") & !lubridate::is.Date, as.Date, "%m/%d/%y")) %>%
  group_by(record_id)  %>%
  convert_date_to_study_day(
    start_date_column = "study_start_date",
    end_date_column = "study_end_date",
    account_for_excluded_periods = TRUE,
    excluded_period_start_date_column = "excluded_start_date",
    excluded_period_end_date_column = "excluded_end_date",
    excluded_period_offset = 1
  ) %>%
  # to get enrollment duration in days, we add 1 because we begin study day indexing at 0
  mutate(days_in_study = max(study_day) + 1) %>%
  ungroup() %>%
  select(record_id, study_completion_status, days_in_study, date, study_day)

write_csv(study_dates_and_indexes, file = here::here("data/processed/rosa_study_dates_and_indexes.csv"))
