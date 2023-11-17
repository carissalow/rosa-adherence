# 0. Create participant file for RAPIDS (https://www.rapids.science/1.9/setup/configuration/#participant-files)

#### set up ----

library(RMariaDB)
library(tidyverse)
source(here::here("src/data/utils.R"))

credentials <- yaml::read_yaml("credentials.yaml")[[1]]

con <- dbConnect(
  MariaDB(), 
  dbname = credentials$database, 
  host = credentials$host, 
  user = credentials$user, 
  password = credentials$password
)

#### import data ----

# participants who completed the study on or before 6/7/2023
study_dates <- read_file_with_clean_names(file = "data/raw/ROSA-StartAndEndDates_DATA_LABELS_2023-06-12_1550.csv", excluded_column_patterns = "Repeat")

aware_device_query <- "
  select 
    group_concat(device_id separator ';') as device_id,
    group_concat(case when manufacturer = 'Apple' then 'ios' else 'android' end separator ';') as platform,
    # correct typo
    case when label = 0015 then 1015 else label end as label
  from aware_device
  group by label
  order by label;
"

fitbit_device_query <- "
  select
    group_concat(distinct fitbit_id separator ';') as fitbit_id,
    # correct typo and 'new' IDs created when tokens were reassigned
    case 
      when label = 0015 then 1015
      when label = 1056 then null
      when label = 10561 then 1056
      when label = 1089 then null
      when label = 10891 then 1089
      else label
    end as label
  from fitbit_data_from_api_v2
  group by label
  order by label;
"

aware_devices <- dbGetQuery(con, aware_device_query) 
fitbit_devices <- dbGetQuery(con, fitbit_device_query)

#### create participants file ---- 

participants_file <- study_dates %>%
  mutate(
    label = as.character(record_id),
    pid = paste0("p", label),
    empatica_id = "",
    start_date = paste0(as.character(as.Date(study_start_date, format = "%m/%d/%y")), " 00:00:00"),
    end_date = paste0(as.character(as.Date(study_end_date, format = "%m/%d/%y")), " 23:59:59")
  ) %>%
  left_join(aware_devices, by = "label") %>%
  left_join(fitbit_devices, by = "label") %>%
  select(device_id, fitbit_id, empatica_id, pid, platform, label, start_date, end_date)

write_delim(participants_file, file = here::here("data/processed/participants.csv"), delim = ",", na = "")
