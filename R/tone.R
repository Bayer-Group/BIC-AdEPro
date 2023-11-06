#' tone - Function that plays two different sounds for adverse events
#'
#' @description
#' Plays two different sounds (after one another) for treatment group comparison for a specific adverse event
#'
#' @param no1 classification of the frequency of the AE in the first treatment group
#' (0=no AE chosen, 1=no occurrence, 2=rare, 3=occasionally, 4=often)
#' @param no2 classification of the frequency of the AE in the second treatment group (
#' 0=no AE chosen, 1=no occurrence, 2=rare, 3=occasionally, 4=often)
#' @param d Duration of the sounds (numeric, in seconds)
#'
#' @keywords internal

tone <- function(no1 = 0, no2 = 0, d = 0.7) {
  to <- c(65.4064, 87.3071, 123.471, 164.814)
  if (no1 == 0) sound1 <- numeric(0) else sound1 <- sin(2 * pi * to[no1] * seq(0, d, length.out = d * 16000))
  if (no2 == 0) sound2 <- numeric(0) else sound2 <- sin(2 * pi * to[no2] * seq(0, d, length.out = d * 16000))
  sound <- c(sound1, sound2)
  if (length(sound) > 0) {
    audio::play(audio::audioSample(sound, 4 * 16000))
  }
}
