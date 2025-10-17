#' pie_legend - Function to create legend for displayed adverse events
#'
#' @description
#' Creates legend object for base R plots
#'
#' @param aes chosen adverse events to display (character)
#' @param colors Colors for slices (this should be maximally 8) (character)
#'
#' @keywords internal

pie_legend <- function(
    aes,
    colors = c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3",
      "#ff7f00", "#ffff33", "#a65628", "#f781bf",
      "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
    )
) {
  on_ex <- graphics::par("oma", "mar", "font")
  on.exit(graphics::par(on_ex))
  graphics::par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), font = 1)
  u <- graphics::par("usr")
  graphics::rect(u[1], u[3], u[2], u[4], col = "#424242", border = "#424242")
  if (length(aes) > 0) {
    graphics::legend(
      "topleft",
      legend = aes,
      col = colors[seq_along(aes)],
      lwd = 15,
      cex = 1,
      bty = "n",
      ncol = 6,
      bg = "#424242",
      box.col = "#424242",
      text.col = "#ffffff"
    )
  }
}
