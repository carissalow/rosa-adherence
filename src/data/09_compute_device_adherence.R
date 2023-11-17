# 9. Compute adherence for phone and Fitbit data collection

# Adherence is operationalized as the proportion of days that the participant was enrolled in the study
# on which sufficient sensor data were collected (data yield ratio valid yielded hours >= (1/3); i.e., at least 8 of 24 hours)

# Daily phone and Fitbit data yield were obtained with RAPIDS v1.9.1 under the following settings:
# Time segments: Periodic, daily, 00:00:00 to 23:59:59
# Timezone: Multiple, use default timezone (America/New_York) if missing, allow multiple timezones per Fitbit, infer Fitbit timezone from phone
# Phone data yield sensors: 
  # activity recognition, applications crashes, applications foreground, applications notifications, battery, 
  # Bluetooth, calls, keyboard, light, locations, messages, screen, WiFi connected, WiFi visible 
# Phone data yield minute ratio for valid yielded hours: 0.5
# Phone data yield features: ratio valid yielded minutes, ratio valid yielded hours
# Fitbit data yield sensors: heart rate intraday
# Fitbit data yield minute ratio for valid yielded hours: 0.5
# Fitbit data yield features: ratio valid yielded minutes, ratio valid yielded hours

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

completeness_threshold <- (1/3)

#### import data ----

study_dates <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv"))
sensor_data_yield <- read_csv(here::here("data/processed/all_sensor_features.csv"))

#### compute device adherence ----

device_adherence <- study_dates %>%
  mutate(pid = paste0("p", record_id)) %>%
  left_join(sensor_data_yield, by = c("pid", "date" = "local_segment_start_datetime")) %>%
  mutate(
    sufficient_phone_data_yield = ifelse(!is.na(phone_data_yield_rapids_ratiovalidyieldedhours) & phone_data_yield_rapids_ratiovalidyieldedhours >= completeness_threshold, 1, 0),
    sufficient_fitbit_data_yield = ifelse(!is.na(fitbit_data_yield_rapids_ratiovalidyieldedhours) & fitbit_data_yield_rapids_ratiovalidyieldedhours >= completeness_threshold, 1, 0),
  ) %>%
  select(-c(pid, study_completion_status, study_day, starts_with("local_segment")))

device_adherence_aggregated <- device_adherence %>% 
  group_by(record_id, days_in_study) %>%
  summarize(
    days_with_sufficient_phone_data_yield = sum(sufficient_phone_data_yield),
    days_with_sufficient_fitbit_data_yield = sum(sufficient_fitbit_data_yield)
  ) %>%
  mutate(
    prop_days_with_sufficient_phone_data_yield = days_with_sufficient_phone_data_yield/days_in_study,
    prop_days_with_sufficient_fitbit_data_yield = days_with_sufficient_fitbit_data_yield/days_in_study
  ) %>%
  ungroup() %>%
  select(-days_in_study)

write_csv(device_adherence, here::here("data/processed/rosa_device_adherence.csv"))
write_csv(device_adherence_aggregated, here::here("data/processed/rosa_device_adherence_aggregated.csv"))
