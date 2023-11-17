require(tidyverse)
require(readxl)
require(clock)


#' Read a CSV or Excel and clean the column names
#'
#' @param file A CSV, XLS, or XSLX file
#' @param excluded_column_names A vector of strings containing column names to be excluded from output
#' @param exlcuded_column_patterns A regular expression  
#' @return A data frame
read_file_with_clean_names <- function(file, excluded_column_names = c(), excluded_column_patterns = NULL) {
  file_extension <- gsub(".*[\\..*$]", "", file)
  
  if (file_extension == "csv") {
    read_file_function <- readr::read_csv
  } else if (file_extension %in% c("xls", "xlsx")) {
    read_file_function <- readxl::read_excel
  } else {
    stop("File must be one of the following: csv, xls, xlsx")
  }
  
  data <- read_file_function(here::here(file))
  
  if (!is.null(excluded_column_patterns)) {
    columns_to_exclude <- c(excluded_column_names, grep(excluded_column_patterns, colnames(data), value = TRUE))
  } else {
    columns_to_exclude <- excluded_column_names
  }
  
  item_numbering_regex <- "^[1-9|a-z]\\d*[\\.|\\)] "
  parentheticals_regex <- " \\(e.g.,.*\\)"
  
  data <- data %>%
    # drop specified columns (exact names and/or pattern matches)
    select(-any_of(columns_to_exclude)) %>%
    # remove item numbering and parentheticals from column names
    rename_all(function(x) gsub(item_numbering_regex, "", x)) %>%
    rename_all(function(x) gsub(parentheticals_regex, "", x)) %>%
    janitor::clean_names()
  
  return(data)
}


#' Convert a wide-format dataset with one row per participant ID and columns for study start and end dates to long-format data set with rows for each participant ID and date in the study
#'
#' @param data A data frame
#' @param start_date_column A string with the name of the column containing the study start date for each participant
#' @param end_date_column A string with the name of the column containing the study end date for each participant
#' @param account_for_excluded_periods A logical flag indicating whether or not you need to account for periods between start and end date that should be dropped (e.g., because the participant paused participation)
#' @param excluded_period_start_date_column A string with the name of the column containing the excluded period start date for each participant
#' @param excluded_period_end_date_column A string with the name of the column containing the excluded period end date for each participant
#' @param excluded_period_offset An integer indicating the number of days by which the excluded period dates should be offset; default is 0
#' @return A data frame in long format with additional columns `date` and `study_day`
convert_date_to_study_day <- function(data, start_date_column, end_date_column, account_for_excluded_periods = FALSE, excluded_period_start_date_column = NULL, excluded_period_end_date_column = NULL, excluded_period_offset = 0) {
  if (!account_for_excluded_periods) {
    data <- data %>%
      mutate(date = list(clock::date_seq(from = get(start_date_column), to = get(end_date_column), by = 1))) 
  } else {
    data <- data %>%
      mutate(
        date = ifelse(
          !is.na(excluded_start_date) & !is.na(excluded_end_date),
          list(
            c(
              clock::date_seq(from = get(start_date_column), to = get(excluded_period_start_date_column) - excluded_period_offset, by = 1), 
              clock::date_seq(from = get(excluded_period_end_date_column) + excluded_period_offset, to = get(end_date_column), by = 1))
            ),
          list(clock::date_seq(from = get(start_date_column), to = get(end_date_column), by = 1))
        )
      )
  }
  data <- data %>%
    unnest(date) %>%
    mutate(study_day = dense_rank(date) - 1) 
  return(data)
}


#' Restrict data frame to subset of participants of interest
#'
#' @param data A data frame
#' @param participant_id_column A string containing the column name corresponding to the participant identifier
#' @param participants A vector of participant IDs 
#' @param include A logical flag indicating whether or not the IDs in the `participants` vector should be included or excluded from the output
#' @return A data frame
subset_participants <- function(data, participant_id_column, participants, include = TRUE) {
  if (include) {
    data <- filter(data, get(participant_id_column) %in% participants)
  }
  else {
    data <- filter(data, !(get(participant_id_column) %in% participants))
  }
  return(data)
}


#' Format PROMIS Profile 29+2 v2.1 responses for HealthMeasures.net Scoring Service 
#' 
#' @param data A data frame
#' @param timepoint A string correspoding to the label for the timepoint of administration 
#' @param timepoint_index An integer value corresponding to the index of the timepoint 
#' @return A data frame
format_promis_profile_responses <- function(data, timepoint = "", timepoint_index = 1) {
  data <- data %>%
    mutate(Assmnt = timepoint_index) %>%
    rename(PIN = record_id) %>%
    rename_all(toupper) %>%
    rename_all(function(x) gsub(sprintf("_%s$", toupper(timepoint)), "", x)) %>%
    rename_all(function(x) ifelse(grepl("SLEEP|GLOBAL|ASSMNT", x), stringr::str_to_sentence(x), x)) %>%
    rename_all(function(x) ifelse(grepl("PC.*R", x), gsub("R$", "r", x), x)) %>%
    rename_all(function(x) gsub("CAPS$", "CaPS", x)) %>%
    select(PIN, Assmnt, everything())
  return(data)
}


#' Read in PROMIS score output from HealthMeasures.net 
#' 
#' @param file_path A string specifying the relative path to the data
#' @param file_name_regex A string specifying the pattern file names should match to be included
#' @return A data frame
read_healthmeasures_promis_output <- function(file_path, file_name_regex) {
  file_names <- paste0(file_path, "/", list.files(here::here(file_path), pattern = file_name_regex))
  if (length(file_names) == 0) { 
    stop(glue::glue("There were no files matching the specified pattern {file_name_regex} found in {here::here(file_path)}"))
  } else if (length(file_names) == 1) {
    data <- read_csv(here::here(file_names), skip = 4)
  } else {
    data <- here::here(file_names) %>%
      purrr::map(., read_csv, skip = 4) %>%
      purrr::reduce(bind_rows)
  }
  return(data)
}


#### calculate variables ----

#' Format output of theta scores and compute PROMIS preference scores (DeWitt et al., 2018)
#' 
#' @param data A data frame containing output from HealthMeasures Scoring Service 
#' @return A data frame containing PROMIS preference scores for each of 7 domains and one summary score  
compute_promis_preference_scores <- function(data) {
  
  # function to generate perefence scores from theta values 
  source("https://raw.githubusercontent.com/janelhanmer/PROPr/master/Generic%20MAUT%20code%202017_09_02.R")
  
  domain_names_to_abbreviations <- tribble(
    ~name, ~abbreviation,
    "Cognitive Function", "cog",
    "Depression/Sadness", "dep", 
    "Fatigue", "fat", 
    "Pain Interference", "pain", 
    "Physical Function", "phys", 
    "Sleep Disturbance", "slp", 
    "Ability to Participate in Social Roles/Activities", "sr" 
  )
  
  data <- data %>%
    select(PIN, Assmnt, Inst, Theta) %>%
    # remove prefix corresponding to file name in each domain
    mutate(Inst = gsub("^.* - ", "", Inst)) %>%
    left_join(
      domain_names_to_abbreviations, 
      by = c("Inst" = "name")
    ) %>%
    select(-Inst) %>%
    # discard any domains that might be present but do not contribute to PROPr scoring (e.g., Anxiety)
    filter(abbreviation %in% domain_names_to_abbreviations$abbreviation) %>%
    pivot_wider(
      values_from = Theta,
      names_from = abbreviation,
      names_prefix = "theta_"
    ) 
  
  expected_columns <- paste0("theta_", domain_names_to_abbreviations$abbreviation)
  
  # all domains must be present in order to generate scores
  if (!all(expected_columns %in% colnames(data))) {
    stop("PROPr scores cannot be computed because theta values are missing for one or more required domains")
  } else {
    # scores cannot be computed for rows with one or more missing theta values so we must drop them
    data <- data %>%
      filter_at(vars(all_of(expected_columns)), all_vars(!is.na(.))) 
  }
  
  # there must be at least one row of data with no missing theta values to generate scores
  if (nrow(data) == 0) {
    stop("PROPr scores cannot be computed because there are no rows without missing theta values")
  } else {
    data <- data %>%
      group_by(PIN, Assmnt) %>%
      mutate(
        # propr.maut.function.201709() expects that thetas are in this specific order
        as.data.frame(propr.maut.function.201709(c_across(all_of(expected_columns)))), .keep = "none"
      ) %>%
      ungroup()
  }
  
  return(data)
}


#' Determine rural/not rural zip code status based on FORHP definition
#'
#' @param data A data frame
#' @param zipcode_column Character string for the name of the column containing participant zip codes 
#' @param drop_zipcode_column Logical. Should the column containing the actual zip code should be dropped from results? Default is `FALSE`
#' @param use_external_forhp_file Logical. Should FORHP eligible zip codes be read from an external file or downloaded directly from hrsa.gov? Default is `FALSE`
#' @param external_forhp_file_path Character string containing the path to the external file, if using
#' @return A data frame with an additional column for rural zip code status, `forhp_rural_zipcode`, and without the initial zip code column if requested
get_forhp_rural_zipcode_status <- function(data, zipcode_column, drop_zipcode_column = FALSE, use_external_forhp_file = FALSE, external_forhp_file_path = NULL) {
  if (use_external_forhp_file) {
    if(is.null(external_forhp_file_path)) {
      stop("You must either specify the path to your external file or set `use_external_forhp_file` to FALSE")
    }
    forhp_file <- here::here(forhp_file_path)
  } else {
    forhp_file <- tempfile(fileext = ".xlsx")
    download.file("https://www.hrsa.gov/sites/default/files/hrsa/rural-health/about/forhp-eligible-zips.xlsx", destfile = forhp_file, mode = "wb")
  } 
  forhp_eligible_zipcodes <- read_excel(forhp_file) %>% pull(ZIP_CODE) %>% as.character()
  data <- data %>%
    mutate(
      # rural zip defined as presence of zip in FORHP list and as not rural otherwise
      forhp_rural_zipcode = case_when(
        !is.na(as.character(get(zipcode_column))) & as.character(get(zipcode_column)) %in% forhp_eligible_zipcodes ~ "Yes",
        !is.na(as.character(get(zipcode_column))) & !(as.character(get(zipcode_column)) %in% forhp_eligible_zipcodes) ~ "No",
        TRUE ~ NA_character_
      )
    )
  if (drop_zipcode_column) {
    data <- data %>%
      select(-all_of(zipcode_column))
  }
  return(data)
}