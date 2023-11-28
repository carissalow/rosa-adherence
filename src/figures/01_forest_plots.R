# Figures 1-3: forest plots summarizing results of multivariable models

#### set up ----

library(tidyverse)
library(ggtext)

source(here::here("src/figures/utils.R"))


#### prepare data ----

multivariable_gee_results_file <- "rosa_purposeful_subset_multivariable_gee_results_with_further_collapsed_race_education_20230828.csv"
multivariable_gee_results <- read_csv(here::here("output/results/", multivariable_gee_results_file))

multivariable_gee_results <- multivariable_gee_results %>%
  filter(!grepl("study_completion_status", model_formula)) %>%
  filter(!grepl("weekly", outcome))


#### figure 1: daily survey adherence ---- 

figure_1 <- multivariable_gee_results %>% 
  filter(outcome == "complete_daily_survey") %>%
  forest_plot(middle_panel_label = "**Association with daily survey adherence**", banded_method = "grouped")

ggsave(figure_1, filename = here::here("output/figures/figure_1.png"), dpi = 500, width = 10, height = 7, units = "in")


#### figure 2: smartphone adherence ----

figure_2 <- multivariable_gee_results %>% 
  filter(outcome == "sufficient_phone_data_yield") %>%
  filter(grepl("phone_type", model_formula)) %>%
  forest_plot(middle_panel_label = "**Association with smartphone adherence**", banded_method = "grouped")

ggsave(figure_2, filename = here::here("output/figures/figure_2.png"), dpi = 500, width = 10, height = 7.5, units = "in")


#### figure 3: fitbit adherence ---- 

figure_3 <- multivariable_gee_results %>% 
  filter(outcome == "sufficient_fitbit_data_yield") %>%
  forest_plot(middle_panel_label = "**Association with Fitbit adherence**", banded_method = "grouped")

ggsave(figure_3, filename = here::here("output/figures/figure_3.png"), dpi = 500, width = 10, height = 7, units = "in")
