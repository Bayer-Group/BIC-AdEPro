#' Calculate the Number of Adverse Events End Day Missing
#'
#' @param dat data frame with adverse event data
#' @param sel_aeendy character with adverse event end day variable name
#'
#' @return numeric value with the number missing adverse event end days
#'

calculate_number_ae_end_missing <- function(dat, sel_aeendy, sel_aedecod, sel_aestdy) {

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if sel_aedecod is null or character
  if (!is.null(sel_aeendy)) {
    if (!is.character(sel_aeendy)) {
      stop("Parameter 'sel_aeendy' must be a character!")
    }
  }

  if (sel_aeendy %in% colnames(dat)) {
    number_ae_end_missing <- dat %>%
      dplyr::filter(is.na(!!rlang::sym(shiny::req(sel_aeendy))) & (!is.na(!!rlang::sym(sel_aedecod)) | !is.na(!!rlang::sym(sel_aestdy)) )) %>%
      nrow()
  } else {
     number_ae_end_missing <- 0
  }

    return(number_ae_end_missing)
 }
