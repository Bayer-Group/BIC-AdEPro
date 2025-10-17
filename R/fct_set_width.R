#' set_width - calculates width (number of columns with circles)
#'
#' @description
#' Calculates vector with 1's with its length as the number of circles on the
#' vertical axis
#'
#' @param patients patient dataset
#' @param height number of circles on the vertical axis
#'
#' @keywords internal

set_width <- function(patients, height) {
  # use function set_vector_layout from set_vector_layout.R file
  vec_lay <- set_vector_layout(patients, height)
  width <- rep(1, ceiling(length(vec_lay) / height))
  return(width)
}
