# 7. Pull and clean daily and weekly symptom surveys 

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

credentials <- yaml::read_yaml("credentials.yaml")[[1]]

con <- RMariaDB::dbConnect(
  RMariaDB::MariaDB(), 
  dbname = credentials$database, 
  host = credentials$host, 
  user = credentials$user, 
  password = credentials$password
)

completeness_threshold <- 50

#### pull data ----

participant_ids <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv")) %>% pull(record_id) %>% unique()

survey_query <- "
  select 
    # correct typo
    case when id = 0015 then 1015 else id end as record_id, 
    # we define the day to which the survey corresponds as the survey start date
    cast(StartDate as date) as survey_start_date,
    cast(StartDate as datetime) as survey_start_date_time,
    # coerce string to integer
    cast(Progress as unsigned) as survey_progress
  from %s_survey_responses
  order by record_id;
"

daily_surveys <- RMariaDB::dbGetQuery(con, sprintf(survey_query, "daily"))
weekly_surveys <- RMariaDB::dbGetQuery(con, sprintf(survey_query, "weekly"))

#### clean daily survey responses ---- 

daily_survey_dates_clean <- daily_surveys %>%
  distinct() %>%
  subset_participants("record_id", participant_ids) %>%
  group_by(record_id, survey_start_date) %>%
  filter(
    # if there are multiple surveys on a given day, one or more is not sufficiently complete, 
    # and one or more is sufficiently complete, retain the first sufficiently complete survey
    if (n() > 1 & (min(survey_progress) < completeness_threshold & max(survey_progress) >= completeness_threshold)) {
      survey_start_date_time == min(survey_start_date_time[survey_progress >= completeness_threshold]) 
    # if there are multiple surveys on a given day and they are all associated with the same date-time,
    # retain the most complete survey
    } else if (n() > 1 & n_distinct(survey_start_date_time) == 1) {
      survey_progress == max(survey_progress)
    # o.w. if there are multiple surveys on a given day and they are all either sufficiently incomplete or complete, 
    # retain the first one
    } else {
      survey_start_date_time == min(survey_start_date_time)
    }
  ) %>%
  ungroup() %>%
  select(record_id, survey_start_date, survey_progress)

write_csv(daily_survey_dates_clean, here::here("data/interim/rosa_daily_survey_dates.csv"))

#### clean weekly survey responses ----

weekly_survey_dates_clean <- weekly_surveys %>%
  distinct() %>%
  subset_participants("record_id", participant_ids) %>%
  mutate(
    # we define a week as Monday 00:00:00 through Sunday 23:59:59
    week_start_date = lubridate::floor_date(survey_start_date, unit = "week", week_start = 1),
    week_end_date = week_start_date + 6,
    week_label = paste(weekdays(week_start_date), week_start_date, "00:00:00", "to", weekdays(week_end_date), week_end_date, "23:59:59")
  ) %>%
  group_by(record_id, week_label) %>%
  filter(
    # if there are multiple surveys in a given week, one or more is not sufficiently complete, 
    # and one or more is sufficiently complete, retain the first sufficiently complete survey
    if (n() > 1 & (min(survey_progress) < completeness_threshold & max(survey_progress) >= completeness_threshold)) {
      survey_start_date_time == min(survey_start_date_time[survey_progress >= completeness_threshold]) 
    # if there are multiple surveys in a given week and they are all associated with the same date-time, 
    # retain the most complete survey
    } else if (n() > 1 & n_distinct(survey_start_date_time) == 1) {
      survey_progress == max(survey_progress)
    # o.w. if there are multiple surveys in a given week and they are all either sufficiently incomplete or complete, 
    # retain the first one
    } else {
      survey_start_date_time == min(survey_start_date_time)
    }
  ) %>%
  ungroup() %>%
  select(record_id, week_start_date, week_end_date, survey_start_date, survey_progress) 
  
write_csv(weekly_survey_dates_clean, here::here("data/interim/rosa_weekly_survey_dates.csv"))
