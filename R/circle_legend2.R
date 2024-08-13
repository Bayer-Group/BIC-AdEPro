#' circle_legend2 - Creates Legend for Subject elements in AdEPro (version 2)
#'
#' @description
#' Drawing legends for symbols in app
#'
#' @param aes list of selected adverse events
#' @param colors color vector for adverse events
#'
#' @keywords internal

circle_legend2 <- function(
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

  tmp <- data.frame(
    "day_start"=rep(1, 12),
    "day_end" = rep(3, 12),
    "patient" = 1:12,
    "ae" = c(rep(NA,4),rep(aes[1],3),NA,aes[1],rep(NA,3)),
    "sev" = c(rep(NA,4),1:3,NA,3,rep(NA,3)),
    "r" = c(rep(NA,4),c(0.5,0.75,1),NA,1,rep(NA,3)),
    "d" = rep(NA, 12),
    "Y" = rev(seq(1, 12 * 3, by = 3)),
    "X" = rep(1, 12),
    "cont" = c(c("#383838", "#383838", "#000000"),NA,rep("#383838",8)),
    "cont_bg" = c(c("#383838", "#424242", "#000000"),NA,rep("#383838",8)),
    "col" = rep(colors[1],12),
    "num" = c(rep(NA,4),rep(1,3),NA,1,rep(NA,3)),
    "bg" = c(rep(colors[1],8),"#383838",rep(colors[1],3))
  )


   poly_t <- function(num, rad = 1, fg = par('fg'), bg = par('fg'),num_aes = length(aes),...) {
    x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
    NULL
  }

  MASS::eqscplot(
    x =NA,
    y = NA,
    xlim = c(0.9, 1.1),
    ylim = c(0, (12 * 3) + 1),
    col.lab = "white",
    axes = FALSE,
  )

  graphics::rect(-100, -100, 100, 100, col = "#383838", border = "#383838")

  for(i in c(1,2,3,5,6,7,9)) {
    rect(tmp$X[i]-1,tmp$Y[i]-1, tmp$X[i]+1, tmp$Y[i]+1, col = "#424242")
    text(x=tmp$X[i] , y=tmp$Y[i]+1.5, c("Ongoing","Drop-out","Death","","Mild","Moderate","Severe","","Resolved")[i], col = "white")
  }

  graphics::symbols(
    tmp$X,
    tmp$Y,
    circles = cbind(rep(0.9, length(tmp$Y))),
    inches = FALSE,
    add = TRUE,
    fg = tmp$cont,
    bg = tmp$cont_bg
  )

  my.symbols(
    x = tmp$X[c(5,6,7,9)],
    y = tmp$Y[c(5,6,7,9)],
    symb = poly_t,
    num = tmp$num[c(5,6,7,9)],
    rad = tmp$r[c(5,6,7,9)],
    bg = tmp$bg[c(5,6,7,9)],
    fg = tmp$col[c(5,6,7,9)],
    xsize = 2,
    add = TRUE
  )
}

