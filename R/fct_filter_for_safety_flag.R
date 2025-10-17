#' Filter for Safety Flag and Create Required Variables
#'
#' @param dat frame with adverse event data set
#' @param SAFFN Safety Flag as numeric (required)
#'
#' @return data frame with filtered adverse event data
#'

filter_for_safety_flag <- function(dat, SAFFN) {

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if parameter SAFFN is null or character
  if (!is.null(SAFFN)) {
    if (!is.character(SAFFN)) {
      stop("Parameter 'dat' must be a character!")
    }
  }

  #filter data for safety flag
  filtered_data <- dat %>%
    dplyr::filter(
      !!rlang::sym(SAFFN) == 1 |
      !!rlang::sym(SAFFN) == "Y" |
      !!rlang::sym(SAFFN) == "Yes" |
      !!rlang::sym(SAFFN) == "YES" |
      !!rlang::sym(SAFFN) == "yes" |
      !!rlang::sym(SAFFN) == "y"
    )

  return(filtered_data)
}
