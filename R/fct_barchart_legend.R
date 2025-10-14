#' barchart_legend - Creates Legend for adverse events barchart view in AdEPro
#'
#' @description
#' Drawing legends for adverse event colours in app
#'
#' @param aes list of selected adverse events
#' @param colors color vector for adverse events
#' @param color_theme logical value for dark/light theme
#'
#' @keywords internal

barchart_legend <- function(
    tmp,
    aes,
    colors = c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3",
      "#ff7f00", "#ffff33", "#a65628", "#f781bf",
      "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
    ),
    legend_click = NULL,
    info,
    color_theme = TRUE
) {

  if(color_theme) {
    bg_col <- "#424242"
    fg_col <- "#383838"
    text_col <- "white"
  } else {
    bg_col <- "#ffffff"
    fg_col <- "#dedede"
    text_col <- "black"
  }
  on_ex <- graphics::par("oma", "mar", "font")
  on.exit(graphics::par(on_ex))
  graphics::par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), font = 1)

  poly_t <- function(num, rad = 1, fg = graphics::par('fg'), bg = graphics::par('fg'),num_aes = length(aes),...) {
    x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    graphics::polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
    NULL
  }

  if (length(aes) > 0) {

    tmp <- tmp %>%
      dplyr::mutate(
        ae = dplyr::case_when(
          nchar(ae) > 19 ~ paste0(substr(ae,1,19),"..."), nchar(ae) <= 19  ~ ae
        )
      )
    MASS::eqscplot(
      x =NA,
      y = NA,
      xlim = c(0.5, 1.5),
      ylim = c(0, (12 * 3) + 1),
      col.lab = text_col,
      axes = FALSE,
    )

    graphics::rect(-100, -100, 100, 100, col = fg_col, border = fg_col)

    for(i in 1:length(aes)) {
      graphics::rect(tmp$X[i]-1,tmp$Y[i]-1, tmp$X[i]+1, tmp$Y[i]+1, col = bg_col)
      graphics::text(x=tmp$X[i] , y=tmp$Y[i]+1.5, tmp$ae[i], col = text_col)
    }

    graphics::symbols(
      tmp$X[1:length(aes)],
      tmp$Y[1:length(aes)],
      squares = 2*tmp$r[1:length(aes)],
      inches = FALSE,
      add = TRUE,
      fg = tmp$col[1:length(aes)],
      bg = tmp$bg[1:length(aes)],
      lwd = 1
    )

  }
  return(info)
}


