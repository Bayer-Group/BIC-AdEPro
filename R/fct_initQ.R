#' initQ - generates classification matrix
#'
#' @description
#' Creates the classification matrix of adverse events such as treatment-emergent
#'
#' @param ae_data adverse event dataset
#'
#' @keywords internal

initQ <- function(ae_data) {
  if (ncol(ae_data) == 5) {
    Q <- data.frame(trtem = rep(TRUE, nrow(ae_data)))
  }
  if (ncol(ae_data) == 6) {
    Q <- as.data.frame(as.logical(ae_data[,-c(1:5)]))
    colnames(Q) <- colnames(ae_data)[-c(1:5)]
  }
  if (ncol(ae_data) > 6)  {
    Q <- apply(ae_data[,-c(1:5)], 2, as.logical)
  }
  return(Q)
}

