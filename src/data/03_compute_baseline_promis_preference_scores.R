# 3. Generate baseline PROMIS preference-based (PROPr) scores from theta values (DeWitt et al., 2018, Med Decis Making)

# PROMIS Profile t-scores and theta values were obtained via Health Measures Scoring Service using the following inputs:
  # Type of short form instrument: Existing short form or profile
  # Measurement system: PROMIS
  # Respondents: Adult
  # Domain: Depression/Sadness (note this selection is arbitrary)
  # Keyword: Profile
  # Short form: PROMIS-29+2 Profile v2.1 (PROPr)
  # Calibration sample: PROMIS Profile v2.1 (default)*

# *Scores were also obtained using the available Cancer calibration sample, but with this calibration sample, theta values are 
# not generated for the Cognitive Function, Ability to Participate in Social Roles/Activities, or Sleep Disturbance domains,
# all of which are required to generate the preference-based scores; thus these data were not suitable for further use

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

#### import data ----

participant_ids <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv")) %>% pull(record_id) %>% unique()
promis_theta_scores <- read_healthmeasures_promis_output("data/interim", "^.*default_calibration_sample.*[0-9].csv$")
  
#### calculate preference-based scores ----

promis_preference_scores <- compute_promis_preference_scores(promis_theta_scores)

promis_preference_scores <- promis_preference_scores %>%
  select(-Assmnt) %>%
  rename_all(function(x) ifelse(x != "PIN", paste0("baseline_promis_preference_", tolower(x)), x)) %>%
  rename(record_id = PIN) %>%
  full_join(data.frame("record_id" = participant_ids), by = "record_id") %>%
  arrange(record_id)

write_csv(promis_preference_scores, here::here("data/processed/rosa_baseline_promis_propr_scores.csv"))
