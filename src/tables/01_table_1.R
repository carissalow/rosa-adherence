# Table 1: descriptive statistics of participant characteristics
# Two versions:
# 1. With stats in overall sample only
# 2. With stats separated by study completion status and comparison

#### set-up ----

library(tidyverse)
library(gtsummary)

source(here::here("src/tables/utils.R"))

#### prep data ----

daily_data <- read_csv(here::here("data/processed/rosa_within_subjects_daily_data_for_analysis.csv"))

covariate_columns <- list(
  study_covariate_columns = c("study_completion_status"),
  demographic_covariate_columns = c("age_years", "gender", "hispanic", "race",  "education"),
  ses_covariate_columns = c("forhp_rural_zipcode"),
  tech_literacy_covariate_columns = c("phone_type"), 
  baseline_qol_covariate_columns = grep("baseline_promis", colnames(daily_data), value = TRUE),
  clinical_covariate_columns = c("insurance_plan_type_collapsed", "cancer_type", "cancer_stage", "months_since_diagnosis_at_enrollment")
)

participant_characteristics_data <- daily_data %>%
  select(c("record_id", unlist(covariate_columns, use.names = FALSE))) %>%
  distinct() %>%
  mutate(
    gender = as.factor(gender),
    race = factor(race, levels = c("White/Caucasian", "Black/African American", "Asian", "Other", "More than one race/ethnicity")),
    ethnicity = case_when(hispanic == "Yes" ~ "Hispanic", hispanic == "No" ~ "Non-Hispanic", TRUE ~ NA_character_),
    ethnicity = factor(ethnicity, levels = c("Non-Hispanic", "Hispanic")),
    education = factor(education, levels = c("Less than a high school diploma", "High school diploma or equivalent", "Some college, no degree", "Associate of arts or other 2-year degree", "Bachelor's degree", "Graduate degree")),
    forhp_rural_zipcode = as.factor(forhp_rural_zipcode),
    insurance_plan_type_collapsed = factor(insurance_plan_type_collapsed, levels = c("Private", "Public", "Mixed")),
    phone_type = factor(phone_type, levels = c("iPhone", "Android")),
    cancer_type = as.factor(cancer_type), 
    cancer_stage = as.factor(cancer_stage)
  ) %>%
  mutate(
    across(
      .cols = where(is.factor), 
      .fns = function(x) forcats::fct_explicit_na(x, na_level = "Unknown")
    )
  ) %>%
  select(-hispanic) %>%
  relocate(ethnicity, .after = "race")


#### create table 1 ----

table_1 <- participant_characteristics_data %>%
  # create an empty variable to be used as a header row for PROMIS scores
  mutate(baseline_promis_preference_empty_row = NA) %>%
  relocate(baseline_promis_preference_empty_row, .after = "phone_type") %>%
  tbl_summary(
    include = -c("record_id", "study_completion_status"),
    label = list(
      age_years = "Age (years), mean (SD) [range]",
      gender = "Sex/gender, n (%)",
      race = "Race, n (%)",
      ethnicity = "Ethnicity, n (%)",
      education = "Education, n (%)",
      forhp_rural_zipcode = "Rural zip code, n (%)",
      phone_type = "Phone type, n (%)",
      baseline_promis_preference_empty_row = "Baseline PROMIS preference score, mean (SD) [range]",
      baseline_promis_preference_propr = "PROPr",
      baseline_promis_preference_cognition = "Cognition",
      baseline_promis_preference_depression = "Depression",
      baseline_promis_preference_fatigue = "Fatigue",
      baseline_promis_preference_pain = "Pain",
      baseline_promis_preference_physical = "Physical",
      baseline_promis_preference_sleep = "Sleep",
      baseline_promis_preference_social = "Social",
      insurance_plan_type_collapsed = "Insurance plan type, n (%)",
      cancer_type = "Cancer type, n (%)",
      cancer_stage = "Cancer stage, n (%)",
      months_since_diagnosis_at_enrollment = "Time since diagnosis (months), mean (SD) [range]"
    ),
    type = list(
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(2, 2, 2, 2),
      c(age_years, months_since_diagnosis_at_enrollment) ~ c(2, 2, 0, 0),
      all_categorical() ~ c(0, 1)
    ),
    missing = "no"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ NA
  ) %>%
  modify_column_indent(
    columns = label,
    rows = label %in% c("PROPr", "Cognition", "Depression", "Fatigue", "Pain", "Physical", "Sleep", "Social")
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Baseline PROMIS preference score, mean (SD) [range]",
    footnote = "Data missing for 3/162 participants (1.8%)"
  ) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        row_type = ifelse(label %in% c("PROPr", "Cognition", "Depression", "Fatigue", "Pain", "Physical", "Sleep", "Social"), "level", row_type),
        stat_0 = ifelse(label == "Baseline PROMIS preference score, mean (SD) [range]", "", stat_0)
      )
  ) %>%
  bold_labels() %>% 
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) 

gt::gtsave(table_1, here::here("output/tables/table_1.docx"))


#### create table 1 with comparison of completed and withdrawn groups ----

table_1_w_comparison <- participant_characteristics_data %>%
  mutate(study_completion_status = gsub(" study$", "", study_completion_status)) %>%
  # create an empty variable to be used as a header row for PROMIS scores
  mutate(baseline_promis_preference_empty_row = NA) %>%
  relocate(baseline_promis_preference_empty_row, .after = "phone_type") %>%
  tbl_summary(
    include = -c("record_id"),
    by = study_completion_status,
    label = list(
      study_completion_status = "Study completion status",
      age_years = "Age (years), mean (SD)",
      gender = "Sex/gender, n (%)",
      race = "Race, n (%)",
      ethnicity = "Ethnicity, n (%)",
      education = "Education, n (%)",
      forhp_rural_zipcode = "Rural zip code, n (%)",
      phone_type = "Phone type, n (%)",
      baseline_promis_preference_empty_row = "Baseline PROMIS preference score, mean (SD)",
      baseline_promis_preference_propr = "PROPr",
      baseline_promis_preference_cognition = "Cognition",
      baseline_promis_preference_depression = "Depression",
      baseline_promis_preference_fatigue = "Fatigue",
      baseline_promis_preference_pain = "Pain",
      baseline_promis_preference_physical = "Physical",
      baseline_promis_preference_sleep = "Sleep",
      baseline_promis_preference_social = "Social",
      insurance_plan_type_collapsed = "Insurance plan type, n (%)",
      cancer_type = "Cancer type, n (%)",
      cancer_stage = "Cancer stage, n (%)",
      months_since_diagnosis_at_enrollment = "Time since diagnosis (months), mean (SD)"
    ),
    type = list(
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(2, 2),
      all_categorical() ~ c(0, 1)
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p() %>%
  modify_fmt_fun(
    update = p.value ~ format_p_values
  ) %>%
  modify_header(
    p.value = "***P* value**"
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Study completion status**"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ NA
  ) %>%
  modify_column_indent(
    columns = label,
    rows = label %in% c("PROPr", "Cognition", "Depression", "Fatigue", "Pain", "Physical", "Sleep", "Social")
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Baseline PROMIS preference score, mean (SD)",
    footnote = "Data missing for 3/162 participants (1.8%)"
  ) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        row_type = ifelse(label %in% c("PROPr", "Cognition", "Depression", "Fatigue", "Pain", "Physical", "Sleep", "Social"), "level", row_type),
        stat_0 = ifelse(label == "Baseline PROMIS preference score, mean (SD)", "", stat_0),
        stat_1 = ifelse(label == "Baseline PROMIS preference score, mean (SD)", "", stat_1),
        stat_2 = ifelse(label == "Baseline PROMIS preference score, mean (SD)", "", stat_2)
      )
  ) %>%
  bold_labels() %>% 
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) 

gt::gtsave(table_1_w_comparison, here::here("output/tables/table_1_with_comparison.docx"))
