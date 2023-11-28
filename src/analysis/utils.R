require(tidyverse)
require(geepack)

#' Construct a model formula of type y ~ X from given inputs
#' 
#' @param outcome String containing the name of the outcome variable
#' @param predictors String, character vector, or list of one or more predictor variable names. If none are specified, an intercept-only model formula (i.e., y ~ 1) will be returned
#' @param include_interaction Logical. Should an interaction term be included in the model? 
#' @param interaction_variable String containing the name of the variable to create an interaction term with. Required if `include_interaction` is set to `TRUE`
#' @return A character string representing a model formula that can be passed to `as.formula()`
create_gee_formula <- function(outcome = "", predictors = "1", include_interaction = FALSE, interaction_variable = NULL) {
  if (include_interaction & is.null(interaction_variable)) {
    stop("You must specify a value for `interaction_variable` in order to include an interaction term in the model formula")
  } 
  
  if (is.list(predictors)) {
    predictors <- unlist(predictors, use.names = FALSE)
  }
  
  predictors <- unique(unlist(lapply(predictors, function(x) ifelse(x == "", "1", x)))) 
  
  if (include_interaction) {
    predictors <- paste0(predictors, "*", interaction_variable)
  }
  
  if (length(predictors) > 1) {
    predictor_string <- paste(predictors, collapse = " + ")
  } else {
    predictor_string <- predictors
  }
  
  gee_formula <- glue::glue("{outcome} ~ {predictor_string}")
  return(gee_formula)
}


#' Fit a logistic GEE (binomial family, logit link) with working correlation structure chosen by minimizing QIC and extract results
#' 
#' @param outcome Same as `create_gee_formula`
#' @param predictors Same as `create_gee_formula`
#' @param include_interaction Same as `create_gee_formula`
#' @param interaction_variableS Same as `create_gee_formula`
#' @param id_variable String containing the name of the variable uniquely identifying participants or clusters; data are assumed to have been sorted by this variable  
#' @param data Data frame containing, at a minimum, the outcome, predictor(s), and id variables
#' @param alpha_level Numeric specifying the alpha level for determining statistical significance. Default is 0.05
#' @param conf_level Numeric specifying the confidence level to use for confidence interval calculation. Default is 0.95
#' @return A data frame with model specification details, coefficient estimates, odds ratios and confidence intervals, and statistical significance (coefficient and global predictor levels)
logistic_gee <- function(outcome = "", predictors = "", include_interaction = FALSE, interaction_variable = NULL, id_variable = "", alpha_level = 0.05, conf_level = 0.95, data) {
  
  # fit the model and select working correlation structure
  # (1) create the model formula
  model_formula <- create_gee_formula(outcome, predictors, include_interaction, interaction_variable)

  # (2) subset model-wise complete data data 
  fit_data <- data %>%
    rename(id = {{id_variable}}) %>%
    dplyr::select(id, all_of(all.vars(as.formula(as.character(model_formula))))) %>% 
    na.omit()

  # (3) fit logistic GEE robust standard errors and exchangeable working correlation structure
  fit_exchangeable <- geeglm(
    formula = as.formula(model_formula),
    family = binomial(link = "logit"),
    id = id,
    data = fit_data,
    corstr = "exchangeable",
    std.err = "san.se"
  )
  
  # (4) refit above GEE with first-order autoregressive correlation structure
  fit_ar1 <- update(fit_exchangeable, corstr = "ar1")
  
  # (5) select which correlation structure is best based on quasi information criterion (QIC)
  if (QIC(fit_exchangeable)[["QIC"]] <= QIC(fit_ar1)[["QIC"]]) {
    fit <- fit_exchangeable
  } else {
    fit <- fit_ar1
  }
  
  # (6) if any of the exponentiated coefficients are Inf, we default to an independence correlation structure
  if (any(is.infinite(exp(fit$coefficients)))) {
    fit <- update(fit, corstr = "independence")
  }
  
  # extract results
  # (1) model specification for reproducibility
  model_specification <- data.frame(
    model_formula = as.character(model_formula),
    outcome = outcome,
    predictors = paste0(c(unlist(predictors, use.names = FALSE), interaction_variable), collapse = ", "),
    interaction_terms = ifelse(is.null(interaction_variable), "N/A", paste0(paste0(unlist(predictors, use.names = FALSE), "*", interaction_variable), collapse = ", ")),
    n = length(unique(fit$id)),
    n_obs = nobs(fit),
    corr_structure = fit$corstr,
    se_type = fit$std.err
  )
  
  # (2) quasi fit statistics (models with smaller values are preferred)
  fit_qic <- QIC(fit)
  model_fit <- data.frame(
    QIC = fit_qic[["QIC"]], 
    QICu = fit_qic[["QICu"]], 
    QICC = fit_qic[["QICC"]], # corrected QIC
    CIC = fit_qic[["CIC"]], # correlation information criterion; more robust alternative to QIC 
    quasi_likelihood = fit_qic[["Quasi Lik"]], # quasi-likelihood
    num_params = fit_qic[["params"]] # number of mean effect parameters
  )
  
  # (3) estimated odds ratios and [conf_level]*100% confidence intervals
  model_coefs <- fit %>%
    broom::tidy(conf.int = TRUE, conf.level = conf_level, exponentiate = TRUE) %>%
    select(term, or = estimate, or_ci_low = conf.low, or_ci_high = conf.high) %>%
    mutate(conf_level = conf_level) %>%
    left_join(
      fit %>%
        broom::tidy() %>%
        select(term, beta_hat = estimate, std_error = std.error, wald_statistic = statistic, wald_p_value = p.value),
      by = "term"
    ) %>%
    mutate(predictor = ifelse(!(term == "(Intercept)"), gsub("[[:upper:]].*", "", term), term)) %>%
    select(predictor, everything())
  
  # (4) predictor global significance 
  model_global_sig <- data.frame()
  for (predictor in predictors) {
    if (length(predictors) > 1) {
      predictors_for_null_model <- predictors[predictors != predictor]
      null_model_formula <- create_gee_formula(outcome, predictors_for_null_model, include_interaction, interaction_variable)
    } else {
      null_model_formula <- create_gee_formula(outcome, "1", include_interaction, interaction_variable)
    }
    null_model_fit <- update(fit, formula = as.formula(null_model_formula))
    anova_test <- anova(fit, null_model_fit)
    predictor_global_sig <- data.frame(
      predictor_tested = predictor,
      anova_statistic = gsub("Analysis of |'| Table\n", "", attributes(anova_test)$heading[1]),
      anova_chi_sq = anova_test$X2,
      anova_df = anova_test$Df,
      anova_p_value = anova_test$`P(>|Chi|)`
    )
    model_global_sig <- rbind(model_global_sig, predictor_global_sig)
  }
  
  # (5) predictor metadata for plotting 
  model_term_info <- data.frame(
    predictor = names(attr(fit$terms, "dataClasses"))[-1],
    term_label = attr(fit$terms, "term.labels"),
    class = attr(fit$terms, "dataClasses")[-1], 
    stringsAsFactors = FALSE, 
    row.names = NULL
  ) %>%
  group_by(term_number = row_number()) %>% 
  do({
    if (.$class == "factor") {
      tab <- table(eval(parse(text = .$term_label), fit_data, parent.frame()))
      data.frame(
        .,
        level = names(tab),
        level_number = 1:length(tab),
        n_per_level = as.integer(tab),
        stringsAsFactors = FALSE, 
        row.names = NULL
      )
    } else {
      data.frame(
        ., 
        level = NA,
        level_number = NA,
        n_per_level = sum(!is.na(eval(parse(text = .$term_label), fit_data, parent.frame()))),
        stringsAsFactors = FALSE
      )
    }
  }) %>%
  ungroup() %>%
  mutate(
    term = paste0(term_label, replace(level, is.na(level), ""))
  )
  
  # collect results
  model_results <- bind_cols(model_specification, model_fit, model_coefs) %>%
    full_join(model_term_info, by = c("predictor", "term")) %>%
    fill(., c(colnames(model_specification), colnames(model_fit)), .direction = "down") %>%
    arrange(!is.na(term_number), term_number, level_number) %>%
    left_join(model_global_sig, by = c("predictor" = "predictor_tested")) 
  
  return(model_results)
}
