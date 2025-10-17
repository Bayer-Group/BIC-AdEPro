#' Calculate the Number of Cases when Adverse Event Start Day is after the Adverse Event End Day
#'
#' @param dat data frame with adverse event data
#' @param sel_aestdy character with adverse event start day variable name
#' @param sel_aeendy character with adverse event end day variable name
#'
#' @return numeric value with the number of adverse event end day smaller start day

calculate_number_ae_end_is_earlier_than_start <- function(dat, sel_aestdy, sel_aeendy) {
 #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if sel_aestdy is null or character
  if (!is.null(sel_aestdy)) {
    if (!is.character(sel_aestdy)) {
      stop("Parameter 'sel_aestdy' must be a character!")
    }
  }

  #check if sel_aeendy is null or character
  if (!is.null(sel_aeendy)) {
    if (!is.character(sel_aeendy)) {
      stop("Parameter 'sel_aeendy' must be a character!")
    }
  }

  if (sel_aestdy %in% colnames(dat) & sel_aeendy %in% colnames(dat)) {
    number_days_removed <- dat %>%
    dplyr::filter(
      as.numeric(!!rlang::sym(shiny::req(sel_aestdy))) > as.numeric(!!rlang::sym(sel_aeendy))) %>%
      nrow()
    } else {
      number_days_removed <- 0
    }
  return(number_days_removed)
}
