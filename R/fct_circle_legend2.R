#' circle_legend2 - Creates Legend for Subject elements in AdEPro (version 2)
#'
#' @description
#' Drawing legends for symbols in app
#'
#' @param aes list of selected adverse events
#' @param colors color vector for adverse events
#' @param color_theme logical value for dark/light theme
#'
#' @keywords internal

circle_legend2 <- function(
    aes,
    colors = c(
      "#e43157", "#377eb8", "#4daf4a", "#984ea3",
      "#ff7f00", "#ffff33", "#a65628", "#f781bf",
      "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
    ),
    grading=F,
    color_theme =TRUE
) {
  on_ex <- graphics::par("oma", "mar", "font")
  on.exit(graphics::par(on_ex))
  graphics::par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), font = 1)
  if(color_theme) {
    bg_col <- "#424242"
    fg_col <- "#383838"
    dth_col <- "black"
    text_col <- "white"
  } else {
    bg_col <- "#ffffff"
    fg_col <- "#dedede"
    dth_col <- "black"
    text_col <- "black"
  }
  if (grading==F){
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
      "cont" = c(c(fg_col, fg_col, dth_col),NA,rep(fg_col,8)),
      "cont_bg" = c(c(fg_col, bg_col, dth_col),NA,rep(fg_col,8)),
      "col" = rep(colors[1],12),
      "num" = c(rep(NA,4),rep(1,3),NA,1,rep(NA,3)),
      "bg" = c(rep(colors[1],8),fg_col,rep(colors[1],3))
    )
  } else {
    tmp <- data.frame(
      "day_start"=rep(1, 12),
      "day_end" = rep(3, 12),
      "patient" = 1:12,
      "ae" = c(rep(NA,4),rep(aes[1],5),NA,aes[1],rep(NA,1)),
      "sev" = c(rep(NA,4),1:5,NA,5,rep(NA,1)),
      "r" = c(rep(NA,4),c(0.4,0.55,0.7,0.85,1),NA,1,rep(NA,1)),
      "d" = rep(NA, 12),
      "Y" = rev(seq(1, 12 * 3, by = 3)),
      "X" = rep(1, 12),
      "cont" = c(c(fg_col, fg_col, dth_col),NA,rep(fg_col,8)),
      "cont_bg" = c(c(fg_col, bg_col, dth_col),NA,rep(fg_col,8)),
      "col" = rep(colors[1],12),
      "num" = c(rep(NA,4),rep(1,5),NA,1,rep(NA,1)),
      "bg" = c(rep(colors[1],10),fg_col,rep(colors[1],1))
    )
  }

  poly_t <- function(num, rad = 1, fg = graphics::par('fg'), bg = graphics::par('fg'),num_aes = length(aes),...) {
    x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
    graphics::polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
    NULL
  }

  MASS::eqscplot(
    x =NA,
    y = NA,
    xlim = c(0.9, 1.1),
    ylim = c(0, (12 * 3) + 1),
    col.lab = text_col,
    axes = FALSE,
  )

  graphics::rect(-100, -100, 100, 100, col = fg_col, border = fg_col)

  if (grading==F){
    for(i in c(1,2,3,5,6,7,9)) {
      graphics::rect(tmp$X[i]-1,tmp$Y[i]-1, tmp$X[i]+1, tmp$Y[i]+1, col = bg_col)
      graphics::text(x=tmp$X[i] , y=tmp$Y[i]+1.5, c("Ongoing","Drop-out","Death","","Mild","Moderate","Severe","","Resolved")[i], col = text_col)
    }
  } else {
    for(i in c(1,2,3,5,6,7,8,9,11)) {
      graphics::rect(tmp$X[i]-1,tmp$Y[i]-1, tmp$X[i]+1, tmp$Y[i]+1, col = bg_col)
      graphics::text(x=tmp$X[i] , y=tmp$Y[i]+1.5, c("Ongoing","Drop-out","Death","","Mild","Moderate","Severe","Life-threatening","Leading to death","","Resolved")[i], col = text_col)
    }
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

  if (grading==F){
    my_symbols(
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
  } else {
    my_symbols(
      x = tmp$X[c(5,6,7,8,9,11)],
      y = tmp$Y[c(5,6,7,8,9,11)],
      symb = poly_t,
      num = tmp$num[c(5,6,7,8,9,11)],
      rad = tmp$r[c(5,6,7,8,9,11)],
      bg = tmp$bg[c(5,6,7,8,9,11)],
      fg = tmp$col[c(5,6,7,8,9,11)],
      xsize = 2,
      add = TRUE
    )
  }
}

