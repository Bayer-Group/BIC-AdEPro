#' preproc_patients - Preprocessing Patient Data
#'
#' @description
#' Preprocesses patient dataset
#'
#' @param patients patient dataset
#' @param height number of circles to be displayed on the vertical axis
#'
#' @keywords internal

preproc_patients <- function(patients, height) {
  # set patient positions in layout of circles:
  # use function set_vector_layout from set_vector_layout.R file
  vec_lay    <- set_vector_layout(patients, height)
  # use function set_width() from set_width.R file
  width      <- set_width(patients, height)
  if (dim(patients)[1] == length(rep(2 * cumsum(width) - 1, each = height)[which(vec_lay != 0)])) {
  patients$X <- rep(2 * cumsum(width) - 1, each = height)[which(vec_lay != 0)]
  patients$Y <- rep(seq(-1, -2 * height + 1, by = -2), length(width))[which(vec_lay != 0)]
  return(patients)
  }
}
