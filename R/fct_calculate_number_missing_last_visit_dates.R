#' Calculate the Number of Missing Last Visit Dates
#'
#' @param dat data frame with adverse event data
#' @param sel_lvdt character with variable name of last visit date in data
#' @param sel_subjidn character with variable name of subject id
#'
#' @return numeric value with the number missing last visit dates

calculate_number_missing_last_visit_dates <- function(dat, sel_lvdt, sel_subjidn){

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if sel_lvdt is null or character
  if (!is.null(sel_lvdt)) {
    if (!is.character(sel_lvdt)) {
      stop("Parameter 'sel_lvdt' must be a character!")
    }
  }

  if (sel_lvdt %in% colnames(dat)) {

    number_missing_lvdt <- dat %>%
      dplyr::filter((is.na(!!rlang::sym(sel_lvdt)))) %>%
      dplyr::pull(!!rlang::sym(sel_subjidn)) %>%
      unique() %>%
      length()

  } else {
    number_missing_lvdt <- 0
  }
  return(number_missing_lvdt)
}
