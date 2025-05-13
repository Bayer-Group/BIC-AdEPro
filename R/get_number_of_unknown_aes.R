get_number_of_unknown_aes <- function(data, data_adae, data_adsl, sel_aedecod) {
  if (!is.null(sel_aedecod)) {
    if (sel_aedecod %in% colnames(adae_data)) {
      number_unknown_aes <- sum(adae_data[[sel_aedecod]] == "", na.rm = TRUE )
    } else if (sel_aedecod %in% colnames(adae_data)) {
      number_unknown_aes <- sum(adsl_data[[sel_aedecod]] == "", na.rm = TRUE )
    } else {
      number_unknown_aes <- 0
    }
  } else {
    number_unknown_aes <- 0
  }
  if (number_unknown_aes > 0) {
    if (sel_aedecod %in% colnames(adae_data)) {
      data$AEDECOD[which(data[[sel_aedecod]] == "")] <- "Unknown type of AE"
    } else if (sel_aedecod %in% colnames(adae_data)) {
      adsl_data$AEDECOD[which(adsl_data[[sel_aedecod]] == "")] <- "Unknown type of AE"
    }
  }
}
