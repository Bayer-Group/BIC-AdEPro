#' pie_legend2 - Creates Legend for adverse events slices in AdEPro (version 2)
#'
#' @description
#' Drawing legends for adverse event symbols in app
#'
#' @param aes list of selected adverse events
#' @param colors color vector for adverse events
#' @param color_theme logical value for dark/light theme
#'
#' @keywords internal

pie_legend2 <- function(
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
  on_ex <- par("oma", "mar", "font")
  on.exit(par(on_ex))
  par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), font = 1)
  if(color_theme) {
    bg_col <- "#424242"
    fg_col <- "#383838"
    dth_col <- "black"
    text_col <- "white"
    arrow_col <- "#bababa"
    highlight_col <- "#ffffff10"
  } else {
    bg_col <- "#ffffff"
    fg_col <- "#dedede"
    dth_col <- "black"
    text_col <- "black"
    arrow_col <- "#383838"
      highlight_col <- "#f5e44960"
  }

  poly_t <- function(num, rad = 1, fg = par('fg'), bg = par('fg'),num_aes = length(aes),...) {
    x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
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
      xlim = c(0.9, 1.1),
      ylim = c(0, (12 * 3) + 1),
      col.lab = text_col,
      axes = FALSE,
    )

    graphics::rect(-100, -100, 100, 100, col = fg_col, border = fg_col)

    for(i in 1:length(aes)) {
      rect(tmp$X[i]-1,tmp$Y[i]-1, tmp$X[i]+1, tmp$Y[i]+1, col = bg_col)
      text(x=tmp$X[i] , y=tmp$Y[i]+1.5, tmp$ae[i], col = text_col)
    }

    graphics::symbols(
      tmp$X,
      tmp$Y,
      circles = cbind(rep(0.9, length(tmp$Y))),
      inches = FALSE,
      add = TRUE,
      fg = tmp$cont_bg,
      bg = tmp$cont_bg
    )

    if (length(aes) > 1) {
      my.symbols(
        x = tmp$X[1:length(aes)],
        y = tmp$Y[1:length(aes)],
        symb = poly_t,
        num = tmp$num[1:length(aes)],
        rad = tmp$r[1:length(aes)],
        bg = tmp$bg[1:length(aes)],
        fg = tmp$col[1:length(aes)],
        xsize = 2,
        add = TRUE
      )
    } else {
      graphics::symbols(
        tmp$X[1:length(aes)],
        tmp$Y[1:length(aes)],
        circles = 0.85*tmp$r[1:length(aes)],
        inches = FALSE,
        add = TRUE,
        fg = tmp$col[1:length(aes)],
        bg = tmp$bg[1:length(aes)],
        lwd = 1
      )
    }

    if (!is.null(info)) {
      if (dim(info)[1] > 0) {
       graphics::symbols(
          info$X,
          info$Y,
          squares = cbind(rep(2, length(info$Y))),
          inches = FALSE,
          add = TRUE,
          fg = highlight_col,
          bg = highlight_col,
          lwd = 3,
          xlab = "",
          ylab = "",
          main = "",
        )
      }
    }
  }
  return(info)
}


