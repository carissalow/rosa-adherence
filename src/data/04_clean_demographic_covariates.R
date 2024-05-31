# 4. Combine and clean demographics data from REDCap

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### import data ----

participant_ids <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv")) %>% pull(record_id) %>% unique()

demographics_files <- list(
  demographics = "data/raw/ROSA-Demographics_DATA_LABELS_2023-06-21_0911.csv",
  device_os = "data/raw/ROSA-OperatingSystems_DATA_LABELS_2023-06-08_1443.csv",
  home_wifi = "data/raw/ROSA-HomeWifi_DATA_LABELS_2023-06-08_1443.csv",
  home_zipcode = "data/raw/ROSA-ZipCode_DATA_2023-06-12_1059.csv",
  insurance_status = "data/raw/ROSA_Insurance_Updated.xlsx"
)

excluded_columns <- c("Repeat Instrument", "Repeat Instance", "Complete?", "dob")
join_columns <- c("record_id")

demographics <- demographics_files %>%
  purrr::map(read_file_with_clean_names, excluded_columns) %>%
  purrr::reduce(full_join, join_columns) %>%
  subset_participants("record_id", participant_ids)

#### clean data ----

demographics_clean <- demographics %>%
  select(
    record_id,
    # demographics
    age_years = age,
    gender = what_is_your_gender,
    hispanic = do_you_consider_yourself_to_be_of_latin_or_hispanic_origin,
    race = what_is_your_race_ethnicity,
    education = what_is_the_highest_level_of_education_that_you_completed,
    # tech literacy
    wifi_access_at_home = do_you_have_wireless_internet_access_at_home_wi_fi,
    phone_type = phone_model,
    # socioeconomic status
    address_zip,
    insurance_plan_type = type
  ) %>%
  # collapse categories with low cell counts
  mutate(
    race_collapsed = case_when(
      !is.na(race) & !(race %in% c("White/Caucasian", "Black/African American")) ~ "Other",
      TRUE ~ race
    ),
    education_collapsed = case_when(
      education %in% c("Less than a high school diploma", "High school diploma or equivalent") ~ "High school or less",
      education %in% c("Associate of arts or other 2-year degree", "Some college, no degree") ~ "Some college",
      TRUE ~ education
    ),
    wifi_access_at_home_collapsed = case_when(
      wifi_access_at_home %in% c("No", "Not sure") ~ "No or not sure",
      TRUE ~ wifi_access_at_home
    ) 
  ) %>%
  # create indicator for rural zip code based on Federal Office of Rural Health Policy eligibility
  get_forhp_rural_zipcode_status(zipcode_column = "address_zip", drop_zipcode_column = TRUE) %>%
  # treat no insurance plan coverage listed in EMR ("None" category) as missing
  # collapse mix of public, private, and other primary and secondary insurance plan coverage into single "mixed" category
  mutate(
    insurance_plan_type = case_when(
      insurance_plan_type == "None" ~ NA_character_,
      TRUE ~ insurance_plan_type,
    ),
    insurance_plan_type_collapsed = case_when(
      insurance_plan_type %in% c("Public/Private", "Private/Public", "Private/Other") ~ "Mixed",
      TRUE ~ insurance_plan_type
    )
  ) 

write_csv(demographics_clean, file = here::here("data/processed/rosa_demographic_covariates.csv"))
