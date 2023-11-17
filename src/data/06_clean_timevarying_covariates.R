# 6. Clean and derive time-varying covariates 

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### import data ----

study_dates <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv"))
participant_ids <- study_dates %>% pull(record_id) %>% unique()

excluded_columns <- c("Repeat Instrument", "Repeat Instance")

chemo_dates <- read_file_with_clean_names(here::here("data/raw/ROSA-ChemoDates_DATA_LABELS_2023-06-21_1040.csv"), excluded_columns)

#### clean chemo treatment data ----

chemo_treatment_dates <- chemo_dates %>%
  select(record_id, starts_with("chemotherapy") & contains("date")) %>%
  pivot_longer(
    cols = -c(record_id),
    names_to = "chemotherapy_treatment_number",
    names_prefix = "chemotherapy_treatment_number_|_date",
    values_to = "chemotherapy_treatment_date",
    values_drop_na = TRUE
  )

#### derive time-varying covariates ----

timevarying_covariates <- study_dates %>%
  mutate(
    weekend_day = case_when(
      lubridate::wday(date, label = TRUE, abbr = TRUE) %in% c("Sat", "Sun") ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  left_join(
    chemo_treatment_dates, 
    by = c("record_id", "date" = "chemotherapy_treatment_date")
  ) %>%
  group_by(record_id) %>%
  fill(chemotherapy_treatment_number, .direction = "down") %>%
  ungroup() %>%
  mutate(
    .by = c(record_id, chemotherapy_treatment_number),
    days_since_last_known_chemotherapy_treatment = case_when(
      !is.na(chemotherapy_treatment_number) ~ as.numeric(date - min(date), unit = "days"),
      TRUE ~ NA
    )
  ) %>%
  select(record_id, date, weekend_day, contains("days_since"))

write_csv(timevarying_covariates, here::here("data/processed/rosa_timevarying_covariates.csv"))
