#' preproc_ae - Preprocessing adverse event data
#'
#' @description
#' Preprocesses adverse event dataset
#'
#' @param ae_data adverse event dataset
#'
#' @keywords internal

preproc_ae <- function(ae_data,grading=F) {
  ae_data <- ae_data[,1:5]
  if(grading==F){
    denom <- 4
    ae_data$r <- (ae_data$sev + 1) / denom
  } else {
    denom <- 20
    ae_data$r <- (3*ae_data$sev + 5 ) / denom
  }
  ae_data$d <- rep(NA, nrow(ae_data))
  return(ae_data)
}
