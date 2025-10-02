#' Calculate the Number of Adverse Events Removed due to Missing Treatment Emergency Flag
#'
#' @param dat data frame with adverse event data
#' @param sel_aedecod character with adverse event code variable name
#' @param sel_aetrtemn character with adverse event treatment emergency variable name
#'
#' @return numeric value with the number missing treatment emergency values
#'

calculate_number_aes_not_treatment_emergent <- function(dat, sel_aedecod, sel_aetrtemn){

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if sel_aedecod is null or character
  if (!is.null(sel_aedecod)) {
    if (!is.character(sel_aedecod)) {
      stop("Parameter 'sel_aedecod' must be a character!")
    }
  }

  #check if sel_aetrtemn is null or character
  if (!is.null(sel_aetrtemn)) {
    if (!is.character(sel_aetrtemn)) {
      stop("Parameter 'sel_aetrtemn' must be a character!")
    }
  }

   #get length total adverse events
   length_aes_total <- dat %>%
      dplyr::filter(shiny::req(sel_aetrtemn) == "Y") %>%
      dplyr::filter(shiny::req(sel_aedecod)  != "") %>%
      dplyr::pull(shiny::req(sel_aedecod) ) %>%
      unique() %>%
      length()

    length_aes_treatment_emergent <- dat %>%
      dplyr::filter(shiny::req(sel_aetrtemn) == "Y") %>%
      dplyr::filter(shiny::req(sel_aedecod) != "") %>%
      dplyr::pull(shiny::req(sel_aedecod) ) %>%
      unique() %>%
      length()

    aes_removed_since_treatment_emergent <- length_aes_total - length_aes_treatment_emergent

    return(aes_removed_since_treatment_emergent)
 }
