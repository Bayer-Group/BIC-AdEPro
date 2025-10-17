#' set_global_params - sets all global parameters
#'
#' @description
#' Function that sets all global parameters
#'
#' @param ae_data adverse event dataset
#' @param patients patient dataset
#' @param title titles for the treatment groups
#' @param height number of circles on the vertical axis
#'
#' @keywords internal

set_global_params <- function(
  ae_data,
  patients,
  title = NULL,
  height = NULL,
  treatment = NULL
) {
  ## Consistency checks
  if (is.null(title)) {
    title <- rep("", length(unique(patients$treat)))
  }
  if (is.null(height)) {
    height <- ceiling(sqrt(850 / 1920 * nrow(patients)))
  }

  check_data(ae_data, patients)

  ## Local variables
  # classification matrix of AEs (treatment-emergent, serious etc.)
  # remove variables that creat character(0) in drop down menu
  q <- init_q(ae_data %>% dplyr::select(
    -c("replace_ae_start", "replace_ae_end")
  ))
  ae_options <- seq_len(ncol(q)) # descriptions of AE classifications
  type_names <- data.frame(
    short = c(
      "trtem", "ser", "nonser", "studrel", "studrelser", "relprot", "resdisc",
      "studrelresdisc"
    ),
    long = c(
      "all treatment-emergent", "serious", "non-serious", "study drug-related",
      "study-drug related and serious",
      "related to procedures required by the protocol",
      "resulting in discontinuation of study drug",
      "study drug-related and resulting in discontinuation of study drug"
    )
  )
  names(ae_options) <- sapply(
    seq_len(ncol(q)),
    function(x) type_names$long[which(type_names$short == colnames(q)[x])]
  )

  xylines <- set_group_lines(patients, height, treatment)
  xlines  <- xylines[1]
  ylines  <- xylines[2]
  plines  <- xylines[3]

  globals <- list(
    titles = title,
    footnote = paste("SAF (N=", nrow(patients), ")", sep = ""),
    q = q,
    ae_options = ae_options,
    width = set_width(patients, height),
    height = height,
    xlines = xlines,
    ylines = ylines,
    plines = plines
  )
  return(globals)
}
