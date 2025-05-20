#' Calculate the Number of Adverse Events Start Day Missing
#'
#' @param dat data frame with adverse event data
#' @param sel_aestdy character with adverse event start day variable name
#'
#' @return numeric value with the number missing adverse event start days
#'

calculate_number_ae_start_missing <- function(dat, sel_aestdy, sel_aedecod, sel_aeendy) {

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if sel_aedecod is null or character
  if (!is.null(sel_aestdy)) {
    if (!is.character(sel_aestdy)) {
      stop("Parameter 'sel_aestdy' must be a character!")
    }
  }

  if (sel_aestdy %in% colnames(dat)) {
    number_ae_start_missing <- dat %>%
      dplyr::filter(is.na(!!rlang::sym(shiny::req(sel_aestdy))) & (!is.na(!!rlang::sym(sel_aedecod)) | !is.na(!!rlang::sym(sel_aeendy)) )) %>%
      nrow()
  } else {
    number_ae_start_missing <- 0
  }

    return(number_ae_start_missing)
 }
