#' circle_legend - Function to create legend for displayed subjects
#'
#' @description
#' Creates legend object for base R plots
#'
#' @keywords internal

circle_legend <- function() {
  on_ex <- par("oma", "mar", "font")
  on.exit(par(on_ex))
  par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), font = 1)
  u <- par("usr")
  graphics::rect(u[1], u[3], u[2], u[4], col = "#424242", border = "#424242")
  graphics::legend(
    "topleft",
    legend = c("ongoing    ","drop-out    ","death     "),
    pch = c(19, 21, 19),
    col = c("#383838", "#383838", "#000000"),
    cex = 1,
    pt.cex = 3,
    pt.lwd = 3,
    bty = "n",
    horiz = TRUE,
    bg = "#424242",
    box.col = "#424242",
    text.col = "#ffffff"
  )
}
