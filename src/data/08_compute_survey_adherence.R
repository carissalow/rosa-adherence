# 8. Compute adherence for daily and weekly surveys

# Adherence is operationalized as the proportion of days (or weeks) that the participant was enrolled in the study
# on which they submitted a sufficiently complete (>= 50% survey progress) survey response

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

completeness_threshold <- 50

#### import data ----

study_dates <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv"))
daily_survey_dates <- read_csv(here::here("data/interim/rosa_daily_survey_dates.csv"))
weekly_survey_dates <- read_csv(here::here("data/interim/rosa_weekly_survey_dates.csv"))

#### compute daily survey adherence ----

daily_survey_adherence <- study_dates %>%
  left_join(daily_survey_dates, by = c("record_id", "date" = "survey_start_date")) %>%
  mutate(complete_daily_survey = ifelse(!is.na(survey_progress) & survey_progress >= completeness_threshold, 1, 0)) %>%
  select(-study_day)

daily_survey_adherence_aggregated <- daily_survey_adherence %>%
  group_by(record_id, study_completion_status, days_in_study) %>%
  summarize(days_with_complete_daily_survey = sum(complete_daily_survey)) %>%
  mutate(prop_days_with_complete_daily_survey = days_with_complete_daily_survey/days_in_study) %>%
  ungroup() 

write_csv(daily_survey_adherence, here::here("data/processed/rosa_daily_survey_adherence.csv"))
write_csv(daily_survey_adherence_aggregated, here::here("data/processed/rosa_daily_survey_adherence_aggregated.csv"))

#### compute weekly survey adherence ----

study_weeks <- study_dates %>%
  mutate(
    # we define a week as Monday 00:00:00 through Sunday 23:59:59
    week_start_date = lubridate::floor_date(date, unit = "week", week_start = 1),
    week_end_date = week_start_date + 6,
    week_label = paste(weekdays(week_start_date), week_start_date, "00:00:00", "to", weekdays(week_end_date), week_end_date, "23:59:59")
  ) %>%
  group_by(record_id) %>%
  mutate(
    study_week = dense_rank(week_start_date) - 1,
    study_start_date = min(date),
    study_end_date = max(date),
    weeks_in_study = n_distinct(week_label)
  ) %>%
  ungroup() %>%
  select(record_id, weeks_in_study, study_week, week_start_date, week_end_date, week_label) %>%
  distinct() 

weekly_survey_adherence <- study_weeks %>%
  left_join(weekly_survey_dates, by = c("record_id", "week_start_date", "week_end_date")) %>%
  mutate(complete_weekly_survey = ifelse(!is.na(survey_progress) & survey_progress >= completeness_threshold, 1, 0))

weekly_survey_adherence_aggregated <- weekly_survey_adherence %>%
  group_by(record_id, weeks_in_study) %>%
  summarize(weeks_with_complete_weekly_survey = sum(complete_weekly_survey)) %>%
  mutate(prop_weeks_with_complete_weekly_survey = weeks_with_complete_weekly_survey/weeks_in_study) %>%
  ungroup()

write_csv(weekly_survey_adherence, here::here("data/processed/rosa_weekly_survey_adherence.csv"))
write_csv(weekly_survey_adherence_aggregated, here::here("data/processed/rosa_weekly_survey_adherence_aggregated.csv"))
