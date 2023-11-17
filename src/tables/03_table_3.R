# Table 3: Descriptive statistics of overall adherence
# Two versions:
# 1. With stats in overall sample only
# 2. With stats separated by study completion status and comparison

#### set up ----

library(tidyverse)
library(gtsummary)

source(here::here("src/tables/utils.R"))

#### prep data ----

daily_data <- read_csv(here::here("data/processed/rosa_within_subjects_daily_data_for_analysis.csv"))

participant_adherence_data <- daily_data %>%
  select(
    record_id, study_completion_status, study_day, 
    complete_daily_survey, sufficient_phone_data_yield, sufficient_fitbit_data_yield
  ) %>%
  group_by(record_id, study_completion_status) %>%
  summarize(
    across(
      .cols = c(complete_daily_survey, sufficient_phone_data_yield, sufficient_fitbit_data_yield),
      .fns = list(
        n_days = function(x) sum(x),
        prop_days = function(x) mean(x), # mean of a binary variable is the proportion of "1"s
        percent_days = function(x) mean(x)*100
      ),
      .names = "{.col}_{.fn}"
    )
  ) 


#### create table 3 ----

table_3 <- participant_adherence_data %>%
  tbl_summary(
    include = -c("record_id", "study_completion_status", contains("n_days"), contains("prop_days")),
    label = list(
      complete_daily_survey_percent_days = "Daily survey adherence, mean (SD) [range]",
      sufficient_phone_data_yield_percent_days = "Smartphone adherence, mean (SD) [range]",
      sufficient_fitbit_data_yield_percent_days = "Fitbit adherence, mean (SD) [range]"
    ),
    type = list(
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]"
    ),
    digits = list(
      all_continuous() ~ c(2, 2, 0, 0)
    ),
    missing = "ifany"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ NA
  ) %>%
  modify_header(
    label = "**Outcome**"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label %in% c("Daily survey adherence, mean (SD) [range]", "Smartphone adherence, mean (SD) [range]", "Fitbit adherence, mean (SD) [range]"),
    footnote = "Percent of enrolled days per participant"
  ) %>%
  bold_labels() %>% 
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) 

gt::gtsave(table_3, here::here("output/tables/table_3.docx"))


#### create table 3 with comparison of completed and withdrawn groups ----

table_3_w_comparison <- participant_adherence_data %>%
  tbl_summary(
    include = -c("record_id", contains("n_days"), contains("prop_days")),
    by = study_completion_status,
    label = list(
      complete_daily_survey_percent_days = "Daily survey adherence, mean (SD)",
      sufficient_phone_data_yield_percent_days = "Smartphone adherence, mean (SD)",
      sufficient_fitbit_data_yield_percent_days = "Fitbit adherence, mean (SD)"
    ),
    type = list(
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = list(
      all_continuous() ~ c(2, 2)
    ),
    missing = "ifany"
  ) %>%
  add_overall() %>%
  add_p() %>%
  modify_fmt_fun(
    update = p.value ~ format_p_values
  ) %>%
  modify_footnote(
    all_stat_cols() ~ NA
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Study completion status**"
  ) %>%
  modify_header(
    label = "**Outcome**",
    p.value = "***P* value**"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label %in% c("Daily survey adherence, mean (SD)", "Smartphone adherence, mean (SD)", "Fitbit adherence, mean (SD)"),
    footnote = "Percent of enrolled days per participant"
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) 

gt::gtsave(table_3_w_comparison, here::here("output/tables/table_3_with_comparison.docx"))
