# Table 4: Results of univariable models

#### set up ----

library(tidyverse)
library(gt)

source(here::here("src/tables/utils.R"))

#### prep data ----

univariable_model_results_file <- "rosa_univariable_gee_results_with_further_collapsed_race_education_20230829.csv"
univariable_model_results <- read_csv(here::here("output/results/", univariable_model_results_file))

outcomes <- c("complete_daily_survey", "sufficient_phone_data_yield", "sufficient_fitbit_data_yield")

covariates <- list(
  demographic_covariate_columns = c("age_years", "gender", "race_collapsed",  "education_collapsed"),
  ses_covariate_columns = c("forhp_rural_zipcode"),
  tech_literacy_covariate_columns = c("phone_type"), 
  baseline_qol_covariate_columns = grep("baseline_promis", unique(univariable_model_results$predictor), value = TRUE),
  clinical_covariate_columns = c("insurance_plan_type_collapsed", "cancer_type_collapsed", "cancer_stage_four", "months_since_diagnosis_at_enrollment"),
  timevarying_covariate_columns = c("study_day", "weekend_day", "days_since_last_known_chemotherapy_treatment")
)


#### format data for tabling ----

univariable_model_results <- univariable_model_results %>%
  filter(
    outcome %in% outcomes & predictor %in% unlist(covariates, use.names = FALSE),
    # for phone data yield outcome, retain only the models that adjusted for phone type
    case_when(
      outcome == "sufficient_phone_data_yield" ~ grepl("phone_type", model_formula),
      TRUE ~ predictor %in% unique(univariable_model_results$predictor)
    ),
    # for phone data yield models that adjusted for phone type, drop rows corresponding to estimates for the phone type predictor
    case_when(
      outcome == "sufficient_phone_data_yield" & grepl("phone_type +", model_formula) ~ predictor != "phone_type",
      TRUE ~ predictor %in% unique(univariable_model_results$predictor)
    )
  ) %>%
  arrange(outcome, match(predictor, unlist(covariates, use.names = FALSE))) %>%
  # format odds ratios, CIs, and p-values per JMIR guidelines
  mutate(
    or_and_ci = case_when(
      is.na(or) ~ "*Reference*",
      TRUE ~ as.character(glue::glue("{format_odds_ratios(or)} ({format_odds_ratios(or_ci_low)}-{format_odds_ratios(or_ci_high)})"))
    ),
    p = format_p_values(wald_p_value),
    global_p = format_p_values(anova_p_value)
  ) %>%
  select(outcome, predictor, term_label, level, level_number, n, n_obs, n_per_level, or_and_ci, p, global_p, wald_p_value, anova_p_value) %>%
  # data must be in wide format for the table with separate columns for all possible combinations of outcome and statistic
  pivot_wider(
    id_cols = c(predictor, term_label, level, level_number, n, n_obs, n_per_level),
    names_from = outcome,
    names_glue = "{outcome}_{.value}",
    values_from = c(or_and_ci, p, global_p, wald_p_value, anova_p_value)
  ) %>%
  # make term labels more descriptive and presentable 
  mutate(
    term_label = str_to_sentence(term_label),
    term_label = gsub("Baseline_promis_preference_", "Baseline PROMIS, ", term_label),
    term_label = case_when(
      term_label == "Age_years" ~ "Age (years, centered at mean)",
      term_label == "Race_collapsed" ~ "Race (collapsed)",
      term_label == "Education_collapsed" ~ "Education (collapsed)",
      term_label == "Forhp_rural_zipcode" ~ "Rural zip code",
      term_label == "Insurance_plan_type_collapsed" ~ "Insurance plan type",
      term_label == "Phone_type" ~ "Phone type",
      term_label == "Baseline PROMIS, propr" ~ "Baseline PROMIS, PROPr",
      term_label == "Cancer_stage_four" ~ "Cancer stage 4",
      term_label == "Cancer_type_collapsed" ~ "Cancer type (collapsed)",
      term_label == "Months_since_diagnosis_at_enrollment" ~ "Time since cancer diagnosis (months)",
      term_label == "Study_day" ~ "Study day",
      term_label == "Weekend_day" ~ "Weekend",
      term_label == "Days_since_last_known_chemotherapy_treatment" ~ "Time since last chemotherapy (days)",
      TRUE ~ term_label
    )
  )

#### correct for multiple comparisons (q-values) ----

global_q_values <- univariable_model_results %>%
  select(predictor, ends_with("anova_p_value")) %>%
  distinct() %>%
  mutate(
    across(
      .cols = ends_with("anova_p_value"),
      .fns = function(x) format_p_values(p.adjust(x, method = "fdr")),
      .names = "{gsub('_anova_p_value', '', .col)}_global_q"
    )
  ) %>%
  select(-ends_with("anova_p_value"))


#### create table 4 ----

table_4 <- univariable_model_results %>%
  bind_rows(
    univariable_model_results %>%
      filter(!is.na(level)) %>%
      select(predictor, term_label, n_obs, ends_with("global_p")) %>%
      distinct() %>%
      mutate(level = term_label, level_number = 0) %>%
      rename(n_per_level = n_obs) %>%
      rename_all(function(x) gsub("global_", "", x))
  ) %>%
  arrange(match(predictor, unlist(covariates, use.names = FALSE)), term_label, level_number) %>%
  mutate(
    level = ifelse(is.na(level), term_label, level),
    level_number = ifelse(is.na(level_number), 0, level_number),
    across(
      .cols = ends_with("_p"),
      .fns = function(x) ifelse(level_number == 1,  NA_character_, x)
    )
  ) %>%
  select(level, level_number, n_per_level, contains("complete_daily_survey"), contains("sufficient_phone"), contains("sufficient_fitbit")) %>%
  select(-contains("global_p"), -contains("wald_p"), -contains("anova_p")) %>%
  gt(
    rowname_col = "level"
  ) %>%
  fmt_integer(columns = n_per_level) %>%
  fmt_markdown(columns = contains("or_and_ci")) %>%
  sub_missing(missing_text = "") %>%
  tab_stubhead(md("**Covariate**")) %>%
  tab_stub_indent(
    rows = level_number != 0,
    indent = 3
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_stub(
      rows = level_number == 0
    )
  ) %>%
  tab_spanner(
    label = md("**Daily survey adherence**"),
    columns = paste0("complete_daily_survey_", c("or_and_ci", "p"))
  ) %>%
  tab_spanner(
    label = md("**Smartphone adherence**"),
    columns = paste0("sufficient_phone_data_yield_", c("or_and_ci", "p"))
  ) %>%
  tab_spanner(
    label = md("**Fitbit adherence**"),
    columns = paste0("sufficient_fitbit_data_yield_", c("or_and_ci", "p"))
  ) %>%
  cols_hide(
    level_number
  ) %>%
  cols_label(
    n_per_level = md("**N**"),
    complete_daily_survey_or_and_ci = md("**OR (95% CI)**"),
    complete_daily_survey_p = md("***P* value**"),
    sufficient_phone_data_yield_or_and_ci = md("**OR (95% CI)**"),
    sufficient_phone_data_yield_p = md("***P* value**"),
    sufficient_fitbit_data_yield_or_and_ci = md("**OR (95% CI)**"),
    sufficient_fitbit_data_yield_p = md("***P* value**"),
  ) %>%
  tab_footnote(
    # locations = Covariate
    "For smartphone adherence outcome, adjusted for phone type"
  ) %>%
  tab_footnote(
    # locations = OR (95% CI)
    "OR: odds ratio, CI: confidence interval"
  ) %>%
  tab_footnote(
    # locations = P value
    "Unadjusted Wald test for single- or multi-parameter inference"
  )

gtsave(table_4, here::here("output/tables/table_4.docx"))


#### create alternative version of table 4 with q-values ----

table_4_w_q_values <- univariable_model_results %>%
  bind_rows(
    univariable_model_results %>%
      filter(!is.na(level)) %>%
      select(predictor, term_label, n_obs, ends_with("global_p")) %>%
      distinct() %>%
      mutate(level = term_label, level_number = 0) %>%
      rename(n_per_level = n_obs) %>%
      rename_all(function(x) gsub("global_", "", x))
  ) %>%
  left_join(global_q_values, by = "predictor") %>%
  mutate(
    across(
      .cols = ends_with("global_q"),
      .fns = function(x) ifelse(!is.na(level_number) & level_number != 0, NA_character_, x),
      .names = "{.col}"
    )
  ) %>%
  arrange(match(predictor, unlist(covariates, use.names = FALSE)), term_label, level_number) %>%
  mutate(
    level = ifelse(is.na(level), term_label, level),
    level_number = ifelse(is.na(level_number), 0, level_number),
    across(
      .cols = ends_with("_p"),
      .fns = function(x) ifelse(level_number == 1,  NA_character_, x)
    )
  ) %>%
  select(level, level_number, n_per_level, contains("complete_daily_survey"), contains("sufficient_phone"), contains("sufficient_fitbit")) %>%
  select(-contains("global_p"), -contains("wald_p"), -contains("anova_p")) %>%
  gt(
    rowname_col = "level"
  ) %>%
  fmt_integer(columns = n_per_level) %>%
  fmt_markdown(columns = contains("or_and_ci")) %>%
  sub_missing(missing_text = "") %>%
  tab_stubhead(md("**Covariate**")) %>%
  tab_stub_indent(
    rows = level_number != 0,
    indent = 3
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_stub(
      rows = level_number == 0
    )
  ) %>%
  tab_spanner(
    label = md("**Daily survey adherence**"),
    columns = paste0("complete_daily_survey_", c("or_and_ci", "p", "global_q"))
  ) %>%
  tab_spanner(
    label = md("**Smartphone adherence**"),
    columns = paste0("sufficient_phone_data_yield_", c("or_and_ci", "p", "global_q"))
  ) %>%
  tab_spanner(
    label = md("**Fitbit adherence**"),
    columns = paste0("sufficient_fitbit_data_yield_", c("or_and_ci", "p", "global_q"))
  ) %>%
  cols_hide(
    level_number
  ) %>%
  cols_label(
    n_per_level = md("**N**"),
    complete_daily_survey_or_and_ci = md("**OR (95% CI)**"),
    complete_daily_survey_p = md("***P* value**"),
    complete_daily_survey_global_q = md("***Q* value**"),
    sufficient_phone_data_yield_or_and_ci = md("**OR (95% CI)**"),
    sufficient_phone_data_yield_p = md("***P* value**"),
    sufficient_phone_data_yield_global_q = md("***Q* value**"),
    sufficient_fitbit_data_yield_or_and_ci = md("**OR (95% CI)**"),
    sufficient_fitbit_data_yield_p = md("***P* value**"),
    sufficient_fitbit_data_yield_global_q = md("***Q* value**")
  ) %>%
  tab_footnote(
    # locations = Covariate
    "For smartphone adherence outcome, adjusted for phone type"
  ) %>%
  tab_footnote(
    # locations = OR (95% CI)
    "OR: odds ratio, CI: confidence interval"
  ) %>%
  tab_footnote(
    # locations = P value
    "Unadjusted Wald test for single- or multi-parameter inference"
  ) %>%
  tab_footnote(
    # locations = P value
    "Global Wald test p-value, corrected for multiple comparisons"
  )

gtsave(table_4_w_q_values, here::here("output/tables/table_4_with_q_values.docx"))
