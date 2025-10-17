#' set_group_lines - determines coordinates for the separating lines
#'
#' @description
#' Calculates coordinates for separating lines between the treatment groups
#'
#' @param patients patient dataset
#' @param height number of circles on the vertical axis
#'
#' @keywords internal

set_group_lines <- function(patients, height, treatment) {
  xlines <- 0
  ylines <- 0
  treatment <- sapply(unique(treatment), function(x) length(which(patients$treat == x)))
  l_trt  <- treatment[-1]
  l_trt1 <- treatment[1]
  plines <- ceiling(c(l_trt1 / height, l_trt / height)) * 2
  if (length(l_trt) > 0) {
    xlines <- matrix(rep(cumsum(plines)[-length(plines)], each = 2), nrow = 2)
    ylines <- matrix(rep(c(-2 * height, 0), each = ncol(xlines)), nrow = 2, byrow = TRUE)
  }
  return(list(xlines, ylines, plines))
}
