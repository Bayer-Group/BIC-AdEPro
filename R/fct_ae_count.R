#' ae_count - provides adverse event count dataset
#'
#' @description
#' Provides data set with one adverse event by day and by treatment groups with variable 'freq', giving a
#' categorization (integer between 0 and 4) of the frequency of the adverse event on a specific day by treatment
#' group.
#'
#' @param ae_data adverse event dataset
#' @param patient patient dataset
#'
#' @keywords internal

ae_count <- function(ae_data, patient) {
  #max.day <- max(patient$end) # number of days
  max.day <- max(ae_data$day_end)
  ae_data$treat <- sapply(seq_len(nrow(ae_data)), function(x) patient$treat[which(patient$ps == ae_data$patient[x])])
  all_trt <- unique(patient$treat)
  K <- length(all_trt) # (number of) unique treatment group identifiers

  # prepare data frame:
  counts <- data.frame(day = rep(1:max.day, K), treat = rep(all_trt, each = max.day), freq = numeric(K * max.day))
  # calculate relative frequency of adverse event per day and treatment group
  freq <- as.vector(sapply(1:K, function(k) {
    n_k <- sum(patient$treat == all_trt[k])
    sapply(1:max.day, function(j) {
      rel <- length(which(ae_data$day_start <= j & ae_data$day_end >= j & ae_data$treat == all_trt[k])) / n_k
      return(rel)
    })
  }))

  # normalize and categorize relative frequencies:
  counts$freq <- ceiling(freq / max(freq) * 3) + 1
  # remove sounds for days when nothing changes from the previous day to this day (freq=0)
  for (j in max.day:2) {
    current <- which(counts$day == j)
    previous <- which(counts$day == j - 1)
    if (all(counts$freq[previous] == counts$freq[current])) {
      counts$freq[current] <- 0
    }
  }
  return(counts)
}
