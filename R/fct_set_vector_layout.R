#' set_vector_layout - creates vector layout
#'
#' @description
#' creates vector layout for circles considering number of rows and treatment groups
#'
#' @param patients patient dataset
#' @param height number of circles on the vertical axis
#'
#' @keywords internal

set_vector_layout <- function(patients, height) {
  treatment <- sapply(unique(patients$treat), function(x) length(which(patients$treat == x)))
  l_trt  <- treatment[-1]
  l_trt1 <- treatment[1]
  vec_lay <- c(1:l_trt1, rep(0, ifelse(l_trt1 %% height == 0, 0, height - l_trt1 %% height)))
  if (length(l_trt) > 0) {
    for (z in seq_along(l_trt)) {
      if (l_trt[z] %% height == 0) {
        diff <- 0
      } else {
        diff <- height - l_trt[z] %% height
      }
      vec_lay_add <- c((1:l_trt[z]) + max(vec_lay), rep(0, diff))
      vec_lay <- c(vec_lay, vec_lay_add)
    }
  }
  return(vec_lay)
}
