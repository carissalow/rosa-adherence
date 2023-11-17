# 2. Format baseline PROMIS 29+2 Profile v2.1 responses for Health Measures Scoring Service upload

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### import data ----

excluded_patterns <- "^redcap|^baseline_promis|^fse|^ss|^ccc|^sfsat|^semsx|^notes"

baseline_promis <- read_file_with_clean_names(file = "data/raw/ROSA-BaselinePROMIS_DATA_2023-06-08_1456.csv", excluded_column_patterns = excluded_patterns)
participant_ids <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv")) %>% pull(record_id) %>% unique()

#### format data according to Health Measures scoring service template -----

baseline_promis <- baseline_promis %>%
  format_promis_profile_responses(timepoint = "bl", timepoint_index = 1) %>%
  subset_participants(participant_id_column = "PIN", participants = participant_ids)

write_delim(baseline_promis, file = here::here("data/interim/rosa_baseline_promis_responses_formatted.csv"), delim = ",", na = "")

#### alternatively, save data in batches of size 10 to upload if Health Measures Scoring Service will not deliver scores for the full file ----

participant_id_batches <- split(participant_ids, ceiling(seq_along(participant_ids)/10))

purrr::map(
  participant_id_batches, 
  function(batch) {
    baseline_promis %>%
      subset_participants(participant_id_column = "PIN", participants = batch) %>%
      write_delim(file = here::here(sprintf("data/interim/rosa_baseline_promis_responses_formatted_%s_%s.csv", min(batch), max(batch))), delim = ",", na = "")
  }
)
