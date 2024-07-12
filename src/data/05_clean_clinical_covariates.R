# 5. Combine and clean clinical covariate data from REDCap

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### import data ----

participant_ids <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv")) %>% pull(record_id) %>% unique()

clinical_files <- list(
  cancer_types = "data/raw/ROSA_Cancer_Categories.csv",
  cancer_diagnoses = "data/raw/ROSA-CancerDiagnoses_DATA_LABELS_2023-06-22_0921.csv"
)

excluded_columns <- c("Repeat Instrument", "Repeat Instance", "cancer_type (from EMR)")
join_columns <- c("record_id")

clinical <- clinical_files %>%
  purrr::map(read_file_with_clean_names, excluded_columns) %>%
  purrr::reduce(full_join, join_columns) %>%
  subset_participants("record_id", participant_ids)


#### clean data ----

clinical_clean <- clinical %>%
  mutate(
    days_since_diagnosis_at_enrollment = as.numeric(study_start_date - cancer_diagnosis_date, units = "days"),
    months_since_diagnosis_at_enrollment = lubridate::interval(cancer_diagnosis_date, study_start_date) %/% months(1),
    cancer_type = case_when(
      cancer_type == "GI" ~ "GI tract",
      TRUE ~ cancer_type
    ),
    cancer_stage_four = case_when(
      cancer_stage == 4 ~ "Yes",
      cancer_stage < 4 ~ "No",
      TRUE ~ NA_character_
    )
  ) %>%
  # if there are fewer than 10 participants with a given type of cancer, we combine those types into a single "other" category
  mutate(
    .by = cancer_type,
    cancer_type_collapsed = case_when(
      n() < 10 ~ "Other",
      TRUE ~ cancer_type
    )
  ) %>%
  select(-contains("date"))

write_csv(clinical_clean, here::here("data/processed/rosa_clinical_covariates.csv"))
