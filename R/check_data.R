#' check_data - Consistency checks on the data
#'
#' @description
#' Runs consistency checks on the adverse event and patient data
#'
#' @param ae_data adverse event dataset
#' @param patients patient dataset
#'
#' @keywords internal

check_data <- function(ae_data, patients) {

  # Basic parameter checks:
  if (!is.data.frame(ae_data)) stop("ae_data has to be a data frame")
  if (!is.data.frame(patients)) stop("patients has to be a data frame")
  if(!all(is.element(unique(ae_data$patient), unique(patients$ps)))) {stop("Patient IDs do not match!")}
  if (any(colnames(ae_data)[1:5] != c("day_start", "day_end", "patient", "ae", "sev"))) stop("columns in ae_data are not named correctly")
  if (any(!(colnames(ae_data)[-c(1:5)] %in%
            c("trtem", "ser", "nonser", "studrel", "studrelser", "relprot", "resdisc", "studrelresdisc","replace_ae_start","replace_ae_end")))) stop("columns in ae_data are not named correctly")
  if (any(colnames(patients)[1:4] != c("ps", "treat", "end", "death"))) stop("columns in patients are not named correctly")
  if (!is.factor(ae_data[,4])) stop("ae_data$ae has to be of type of factor")
  if (!all(c(apply(ae_data[, -4], 2, is.numeric), apply(patients[,c(1, 3, 4)], 2, is.numeric)))) stop("all variables expect 'ae', 'treat' and sorting variables have to be of type numeric")
  if (!all(ae_data$sev %in% c(1, 2, 3, 4, 5))) stop("ae_data$sev must include only numbers from 1 to 3 or 1 to 5, respectively")
  if (any(ae_data$day_start < 1 | ae_data$day_end < 1)) stop("'day_start' and 'day_end' must be 1 or greater")
  if (any(ae_data$day_start > ae_data$day_end)) stop("'day_end' must be greater or equal to 'day_start'")
}
