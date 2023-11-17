# Table 2: descriptive statistics of time-varying/day-level characteristics

#### set-up ----

library(tidyverse)
library(gtsummary)

#### prep data ----

daily_data <- read_csv(here::here("data/processed/rosa_within_subjects_daily_data_for_analysis.csv"))

day_characteristics_data <- daily_data %>%
  select(record_id, study_day, weekend_day, days_since_last_known_chemotherapy_treatment) %>%
  mutate(
    weekend_day = as.factor(weekend_day),
    weekend_day = forcats::fct_explicit_na(weekend_day, na_level = "Unknown")
  ) 


#### create table 2 ----

table_2 <- day_characteristics_data %>%
  tbl_summary(
    include = -c("record_id"),
    label = list(
      study_day = "Study day, mean (SD) [range]",
      weekend_day = "Weekend, n (%)",
      days_since_last_known_chemotherapy_treatment = "Time since last chemotherapy (days), mean (SD) [range]"
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
      c(study_day, days_since_last_known_chemotherapy_treatment) ~ c(2, 2, 0, 0),
      all_categorical() ~ c(0, 2)
    ),
    missing = "no"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ NA
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Time since last chemotherapy (days), mean (SD) [range]",
    footnote = "Data missing for 1257/13954 days (9.01%)"
  ) %>%
  bold_labels() %>% 
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) 

gt::gtsave(table_2, here::here("output/tables/table_2.docx"))
