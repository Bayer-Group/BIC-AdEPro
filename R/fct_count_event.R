#' count_event  Count the event number for every AE at specified timepoint
#'
#' @description  Count the event number for every AE at specified timepoint
#'
#' @param total data set with ae data and patient data merged
#' @param day The study day of interest
#'
#' @keywords internal


count_event <- function(total = tot, day = 1){

  tmp <- total %>%
    tidyr::drop_na() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(flag = any(dplyr::between(day, day_start, day_end))) %>%
    dplyr::ungroup()

  tmp2 <- tmp %>%
    dplyr::group_by(ae, treat, .drop = FALSE) %>%
    dplyr::filter(flag == TRUE) %>%
    dplyr::summarise(n = dplyr::n())
  tmp2$treat <- as.character(tmp2$treat)

  tmp2$treat <- as.factor(tmp2$treat)
  return(tmp2)
}
