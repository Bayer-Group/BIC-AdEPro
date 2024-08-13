#' pie_legend2 - Creates Legend for adverse events slices in AdEPro (version 2)
#'
#' @description
#' Drawing legends for symbols in app
#'
#' @param aes list of selected adverse events
#' @param colors color vector for adverse events
#'
#' @keywords internal

pie_legend2 <- function(
    aes,
    colors = c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3",
      "#ff7f00", "#ffff33", "#a65628", "#f781bf",
      "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
    )
) {
  on_ex <- par("oma", "mar", "font")
  on.exit(par(on_ex))
  par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), font = 1)

  poly_t <- function(num, rad = 1, fg = par('fg'), bg = par('fg'),num_aes = length(aes),...) {
    x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
    NULL
  }

  if (length(aes) > 0) {
    tmp <- data.frame(
      "day_start"=rep(1, 12),
      "day_end" = rep(3, 12),
      "patient" = 1:12,
      "ae" = c(aes,rep(NA, 12 - length(aes))),
      "sev" = rep(3, 12),
      "r" = rep(1, 12),
      "d" = rep(NA, 12),
      "Y" = rev(seq(1, 12 * 3, by = 3)),
      "X" = rep(1, 12),
       "cont" = "#424242",
      "cont_bg" = c(rep("#383838", length(aes)), rep("#383838", 12 - length(aes))),
      "col" = c(colors[1:length(aes)], rep(NA, 12 - length(aes))),
      "num" = c(1:length(aes), rep(NA, 12 - length(aes))),
      "bg" = c(colors[1:length(aes)], rep(NA, 12 - length(aes)))
    )

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
      col.lab = "white",
      axes = FALSE,
    )

    graphics::rect(-100, -100, 100, 100, col = "#383838", border = "#383838")

    for(i in 1:length(aes)) {
      rect(tmp$X[i]-1,tmp$Y[i]-1, tmp$X[i]+1, tmp$Y[i]+1, col = "#424242")
      text(x=tmp$X[i] , y=tmp$Y[i]+1.5, tmp$ae[i], col = "white")
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
      TeachingDemos::my.symbols(
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
  }
}


