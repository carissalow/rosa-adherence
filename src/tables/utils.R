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

#' format odds ratios (ORs) per JMIR guidelines
#' @param val Numeric odds ratio  
#' @return Odds ratio as formatted character string 
format_odds_ratios <- function(val) {
  or <- as.character(round(val, 2))
  or <- ifelse(!is.na(nchar(or)) & nchar(or) == 1, paste0(or, ".00"), or)
  or <- ifelse(!is.na(nchar(or)) & nchar(or) == 3, paste0(or, "0"), or)
  return(or)
}
