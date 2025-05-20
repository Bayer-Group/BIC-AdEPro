#' Calculate the Number of Unknown Adverse Events
#'
#' @param dat data frame with adverse event data
#' @param sel_aedecod character with adverse event code variable name
#' @param sel_aedecod character with adverse event start day variable name
#' @param sel_aedecod character with adverse event end day variable name
#'
#' @return numeric value with the number empty adverse events ("")
#'

calculate_number_unknown_aes <- function(dat, sel_aedecod, sel_aestdy, sel_aeendy)  {
  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if sel_aedeoc is null or character
  if (!is.null(sel_aedecod)) {
    if (!is.character(sel_aedecod)) {
      stop("Parameter 'sel_aedecod' must be a character!")
    }
  }

    if (!is.null(sel_aestdy)) {
    if (!is.character(sel_aestdy)) {
      stop("Parameter 'sel_aestdy' must be a character!")
    }
    }

    if (!is.null(sel_aeendy)) {
    if (!is.character(sel_aeendy)) {
      stop("Parameter 'sel_aeendy' must be a character!")
    }
  }

  if (!is.null(sel_aedecod)) {
    if (sel_aedecod %in% colnames(dat)) {

      number_unknown_aes <- dat %>%
        dplyr::filter((is.na(!!rlang::sym(sel_aedecod)) | !!rlang::sym(sel_aedecod) == "" ) & (!is.na(!!rlang::sym(sel_aestdy)) | !is.na(!!rlang::sym(sel_aeendy)) )) %>%
        nrow()

      #number_unknown_aes <- sum(dat[[sel_aedecod]] == "", na.rm = TRUE )
    } else {
      number_unknown_aes <- 0
    }
  } else {
    number_unknown_aes <- 0
  }
  return(number_unknown_aes)
}
