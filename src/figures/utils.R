require(tidyverse)

#' format p-values per JMIR guidelines
#' @param val Numeric p-value
#' @return P-value as formatted character string 
format_p_values <- function(val) {
  p <- case_when(
    val < 0.001 ~ "<.001",
    val > 0.001 & val < 0.01 ~ as.character(round(val, 3)),
    val > 0.001 & val < 0.05 & round(val, 2) == 0.05 ~ as.character(round(val, 3)),
    val > 0.001 & round(val, 2) == 1.0 ~ ">.99",
    TRUE ~ as.character(round(val, 2))
  )
  p <- gsub("NA", "", p) 
  p <- gsub("^0", "", p)
  p <- ifelse(!is.na(nchar(p)) & nchar(p) == 2, paste0(p, "0"), p)
  
  return(p)
}

#' Format odds ratios per JMIR guidelines
#' #' @param val Numeric odds ratio or confidence limit
#' @return odds ratio as formatted character string 
format_odds_ratios <- function(val) {
  or <- as.character(round(val, 2))
  or <- ifelse(!is.na(nchar(or)) & nchar(or) == 1, paste0(or, ".00"), or)
  or <- ifelse(!is.na(nchar(or)) & nchar(or) == 3, paste0(or, "0"), or)
  return(or)
}

#' Format variable names for presentation in plots or tables
#' @param names String of variable name 
#' @return Formatted name 
format_variable_names <- function(names) {
  names <- names %>%
    str_to_sentence() %>%
    gsub("_", " ", .) %>%
    gsub(" collapsed", "", .) %>%
    gsub(" years", " (years, centered at mean)", .) %>%
    gsub("promis", "PROMIS", .) %>%
    gsub("preference", ", ", .) %>%
    gsub("PROMIS , ", "PROMIS,", .) %>%
    gsub("propr$", "PROPr", .) %>%
    gsub("cognition$", "Cognition", .) %>%
    gsub("depression$", "Depression", .) %>%
    gsub("^Forhp r", "R", .) %>%
    gsub("zipcode", "zip code", .) %>%
    gsub(" four", " 4", .) %>%
    gsub("Weekend day", "Weekend", .) %>%
    gsub("Days since last known chemotherapy treatment", "Time since last chemotherapy (days)", .)
  return(names)
}

#' Custom theme for forest plot
#' @return Theme that can be applied to a plot created with ggplot
forest_theme <- function() {
  theme_light() +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_blank(),
      panel.margin = unit(rep(2, 4), "mm")
    )
}

#' Create a summary forest plot for a series of univariable GEEs or single multivariable GEE 
#' Attribution: code adapted from Nick Kennedy (https://stackoverflow.com/a/31373382)
#' @param model_results Data frame returned by `logistic_gee()` function
#' @param widths Numeric vector with 8 elements that specify the relative widths of the plot panes
#' @param middle_panel_label Text displayed 
#' @param color Color of point estimate and error bars
#' @param shape Shape of point estimate
#' @param alpha Transparency of point estimate
#' @param banded Should alternating grey and white bars be used for the plot background?
#' @param banded_method If "grouped", the bands will encompass all levels of a covariate and alternate across covariates; otherwise, the bands will alternate irrespective of covariate groups 
#' @return Forest plot
forest_plot <- function(
    model_results, 
    widths = c(0.15, 0.2, 0.01, 0.01, 0.28, 0.025, 0.2), 
    middle_panel_label = "",
    color = "black", shape = 20, alpha = 0.8, 
    banded = TRUE, banded_method = c("alternating", "grouped")
) {
  
  # format GEE results for plotting
  forest_terms <- model_results %>%
    filter(predictor != "(Intercept)") %>%
    select(
      predictor, class, term, term_label, term_number, level, level_number, n_per_level,
      or, or_ci_low, or_ci_high, wald_p_value, anova_p_value
    ) %>%
    mutate(
      predictor = format_variable_names(predictor),
      n_per_level = scales::comma(n_per_level),
      or = ifelse(is.na(level_number) | level_number > 1, or, 1),
      conf_int = ifelse(is.na(level_number) | level_number > 1, as.character(glue::glue("{format_odds_ratios(or)} ({format_odds_ratios(or_ci_low)}-{format_odds_ratios(or_ci_high)})")), "Reference"),
      p_value = ifelse(is.na(level_number) | level_number > 1, format_p_values(wald_p_value), "")
    ) %>%
    mutate(y = n():1)
  
  # get plot panel geometries
  rel_x <- cumsum(c(0, widths / sum(widths)))
  panes_x <- numeric(length(rel_x))
  forest_panes <- 5:6
  before_after_forest <- c(forest_panes[1] - 1, length(panes_x) - forest_panes[2])
  panes_x[forest_panes] <- with(forest_terms, c(min(or_ci_low, na.rm = TRUE), max(or_ci_high, na.rm = TRUE)))
  panes_x[-forest_panes] <- panes_x[rep(forest_panes, before_after_forest)] + diff(panes_x[forest_panes]) / diff(rel_x[forest_panes]) * (rel_x[-(forest_panes)] - rel_x[rep(forest_panes, before_after_forest)])
  
  # define x coordinates for each element of the plot
  forest_terms <- forest_terms %>%
    mutate(
      variable_x = panes_x[1],
      level_x = panes_x[2],
      n_x = panes_x[3],
      conf_int_x = panes_x[forest_panes[2] + 1],
      p_x = panes_x[forest_panes[2] + 2]
    )
  
  # create coordinates for lines separating plot panes and headings
  forest_lines <- data.frame(
    x = c(rep(c(1, mean(panes_x[forest_panes + 1]), mean(panes_x[forest_panes - 1])), each = 2), panes_x[1], panes_x[length(panes_x)]),
    y = c(rep(c(0.5, max(forest_terms$y) + 0.5), 1), rep(c(0.5, max(forest_terms$y) + 1.5), 2), rep(max(forest_terms$y) + 0.5, 2)), linetype = rep(c("dashed", "solid"), c(2, 6)),
    group = rep(1:4, each = 2)
  )
  
  # create panel heading coordinates and labels
  forest_headings <- data.frame(
    x = c(panes_x[1], panes_x[3], mean(panes_x[forest_panes]), panes_x[forest_panes[2] + 1], panes_x[forest_panes[2] + 2]),
    y = nrow(forest_terms) + 1,
    label = c("**Covariate**", "**N**", middle_panel_label, "**Odds Ratio (95% CI)**", "***P* value**"),
    hjust = c(0, 1, 0.5, 0, 1)
  )
  
  # create coordinates for banding rectangles 
  if (banded & banded_method == "grouped") {
    # alternating rows by predictor
    forest_rectangles <- forest_terms %>%
      select(predictor, y) %>%
      mutate(
        min_y = min(y), 
        max_y = max(y), 
        num_rows = n(), 
        .by = predictor
      ) %>%
      mutate(rank = dense_rank(max_y)) %>%
      filter(!(rank %% 2 == 0)) %>%
      mutate(
        xmin = panes_x[1],
        xmax = panes_x[forest_panes[2] + 2],
        ymin = min_y - 0.5,
        ymax = max_y + 0.5,
      ) %>%
      select(y, xmin, xmax, ymin, ymax) 
  } else {
    # alternating rows by term
    forest_rectangles <- data.frame(
      xmin = panes_x[1], 
      xmax = panes_x[forest_panes[2] + 2], 
      y = seq(max(forest_terms$y), 1, -2)
    ) %>%
      mutate(
        ymin = y - 0.5, 
        ymax = y + 0.5
      )
  }
  
  # x-axis breaks for center panel
  forest_range <- panes_x[forest_panes]
  forest_breaks <- c(
    if (forest_range[1] < 0.8 & forest_range[2] < 5) seq(max(0.2, ceiling(forest_range[1] / 0.2) * 0.2), 0.8, 0.2),
    if (forest_range[1] < 0.8 & forest_range[2] >= 5 & forest_range[2] < 40) 0.5,
    1,
    if (forest_range[2] < 2) 1.5,
    if (forest_range[2] > 2) seq(2, min(10, floor(forest_range[2] / 2) * 2), 2),
    if (forest_range[2] > 20) seq(20, min(100, floor(forest_range[2] / 20) * 20), 20),
    floor(forest_range[2])
  )
  
  # create the plot
  plot <- ggplot(forest_terms, aes(y = y))
  
  # add alternating white and grey bands to plot rows if requested
  if (banded) {
    plot <- plot + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), forest_rectangles, fill = "#EFEFEF")
  }
  
  plot <- plot +
    # center panel: error bars
    geom_errorbarh(
      aes(xmin = or_ci_low, xmax = or_ci_high, y = y),
      height = 0.15, 
      color = color
    ) +
    # center panel: point estimates
    geom_point(
      aes(x = or, y = y), 
      size = 5, 
      shape = shape, 
      color = color,
      alpha = alpha
    ) +
    # delineate panels
    geom_line(
      aes(x = x, y = y, linetype = linetype, group = group),
      forest_lines
    ) +
    scale_linetype_identity() +
    # axis scaling
    scale_x_continuous(
      breaks = forest_breaks,
      labels = sprintf("%g", forest_breaks),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = c(0, 0)
    ) +
    # add headings to panels
    geom_richtext(
      aes(x = x, label = label, hjust = hjust),
      forest_headings,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    # left panel: variable names
    geom_text(
      aes(x = variable_x, label = predictor),
      subset(forest_terms, is.na(level_number) | level_number == 1),
      fontface = "bold",
      hjust = 0
    ) +
    # left panel: level names for categorical covariates 
    geom_text(
      aes(x = level_x, label = level), 
      hjust = 0, 
      na.rm = TRUE
    ) +
    # left panel: number of observations or number of observations per level of categorical covariate
    geom_text(
      aes(x = n_x, label = n_per_level), 
      hjust = 1
    ) +
    # right panel: OR and 95% CI (plotted in center panel)
    geom_text(
      aes(x = conf_int_x, label = conf_int), 
      hjust = 0
    ) +
    # right panel: p-value for single-parameter inference
    geom_text(
      aes(x = p_x, label = p_value), 
      hjust = 1
    ) +
    # apply custom theme elements
    forest_theme()
  
  return(plot)
}
