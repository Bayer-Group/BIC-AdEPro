#' Calculate the Number of Missing Severity Grades
#'
#' @param dat data frame with adverse event data
#' @param sel_aesevn character with severity grade variable name
#' @param severity_grading_flag logical value if 3 or 5 step grading is used
#'
#' @return numeric value with the number missing grades
#'

calculate_number_missing_severity_flag <- function(dat = adae_data, sel_aesevn = NULL, severity_grading_flag)  {

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #check if sel_aedeoc is null or character
  if (!is.null(sel_aesevn)) {
    if (!is.character(sel_aesevn)) {
      stop("Parameter 'sel_aesevn' must be a character!")
    }
  }
   if (is.character(dat[[sel_aesevn]])) {
      number_severe_missing <- ifelse(
        severity_grading_flag == "Severity",
        sum(!dat[[sel_aesevn]] %in% c("MILD","MODERATE","SEVERE",NA)),
        sum(!dat[[sel_aesevn]] %in% c("MILD","MODERATE","SEVERE","LIFE-THREATENING","DEATH",NA))
      )

   } else if (is.numeric(dat[[sel_aesevn]])) {
      number_severe_missing <- ifelse(
        severity_grading_flag=="Severity",
        sum(!dat[[sel_aesevn]] %in% c(1,2,3,NA)),
        sum(!dat[[sel_aesevn]] %in% c(1,2,3,4,5,NA))
      )
   }
  return(number_severe_missing)
 }
