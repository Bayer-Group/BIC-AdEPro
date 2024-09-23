#' adepro_slice_plot - function to create pie chart graph
#'
#' @description
#' Create pie chart graph for app adepro
#'
#' @param data adverse event data set
#' @param patients patient data set
#' @param ae_list list with selected adverse events
#' @param global_params global parameter
#' @param height height size
#' @param width width size
#' @param xlines lines position x
#' @param ylines lines position y
#' @param xval label x-axis
#' @param title graph title
#' @param subgroup selected subgroup
#' @param slider day
#' @param subjidn subjidn variable
#' @param adepro_colors colors used in adepro (max 12)
#' @param arrow_data data.frame with information which start/end date are imputed
#' @param show_arrows logical value if arrows should be displayed for imputed data
#'
#' @importFrom stats end
#' @keywords internal

adepro_slice_plot <- function(
  data,
  patients,
  ae_list,
  global_params,
  height,
  width,
  xlines,
  ylines,
  xval,
  title,
  subgroup,
  slider,
  subjidn,
  adepro_colors = c(
    "#e43157", "#377eb8", "#4daf4a", "#984ea3",
    "#ff7f00", "#ffff33", "#a65628", "#f781bf",
    "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
  ),
  info = NULL,
  legend_ae = NULL,
  arrow_data = NULL,
  show_arrows = FALSE
) {
  ae <- day_start <- ps <- X <- Y <- patient <- r <- NULL

  on_ex <- par("oma","mar","plt")
  on.exit(par(on_ex))

  if (!is.null(subgroup)) {
    index <- length(unique(patients[[subgroup]]))
    names <- sort(unique(patients[[subgroup]]))
  } else {
    index <- 1
    names <- NULL
  }

  par(
    mfrow = c(index, 1),
    oma = c(0, 0, 0, 0),
    mar = c(0, 0, 0, 0),
    plt = c(0.1, 0.99, 0.01, 0.99)
  )

  for (i in 1:index) {
    if (!is.null(subgroup)) {
      patients_tmp <- patients %>%
        dplyr::filter(!!rlang::sym(subgroup) == names[i])
    } else {
      patients_tmp <- patients
    }

    filtered_subjects <- patients_tmp[[subjidn]]

    if (!is.null(patients_tmp$X) & !is.null(patients_tmp$Y)) {

      MASS::eqscplot(
        patients_tmp$X,
        patients_tmp$Y,
        tol = 0,
        axes = FALSE,
        xlab = "",
        ylab = ifelse(!is.null(subgroup), paste0(subgroup, " : ", names[i]), ""),
        main = "",
        xlim = c(0, max(patients$X) + 1),
        ylim = c(min(patients$Y) - 1, 2),
        col.lab = "white",
        type = "n"
      )


      cont <- ifelse(slider > patients_tmp$end, "#424242", "#383838")
      cont <- ifelse(slider >= patients_tmp$death, "black", cont)
      cont_bg <- ifelse(slider >= patients_tmp$death, "black","#383838")
      patients_tmp$cont <- cont
      patients_tmp$cont_bg <- cont_bg

      graphics::symbols(
        patients_tmp$X,
        patients_tmp$Y,
        circles = cbind(rep(0.85, length(patients_tmp$Y))),
        inches = FALSE,
        add = TRUE,
        fg = cont_bg,
        bg = cont,
        lwd = 3,
        xlab = "",
        ylab = "",
        main = "",
        xlim = c(0, 2 * sum(width)),
        ylim = c(-2 * height, 3)
      )

      graphics::matplot(
        xlines[[1]],
        ylines[[1]],
        type = "l",
        lty = 2,
        col = "#6b6b6b",
        add = TRUE
      )

      if (i == 1) {
        graphics::text(
          x = xval,
          y = rep(1, length(xval)),
          labels = title,
          col = "white",
          cex = 1.5
        )
      }

      if (!is.null(subgroup)) {
        tmp_start <- data[data$patient %in% filtered_subjects,]
      } else {
        tmp_start <- data
      }


      if (length(ae_list) > 0) {
        tmp <- tmp_start %>%
          #insert arrow data
          left_join(arrow_data %>%
            dplyr::select(patient,ae,day_start,day_end,replace_ae_start,replace_ae_end),
            by = c("patient","ae","day_start","day_end")) %>%
          dplyr::filter(ae %in% ae_list) %>%
          dplyr::filter(day_start <= slider) %>%
          dplyr::left_join(
            patients_tmp %>%
              dplyr::select(ps,X,Y,cont,cont_bg) %>%
              dplyr::rename(patient = ps),
            by = "patient"
          ) %>% dplyr::left_join(
             data.frame(ae = ae_list, col = adepro_colors[1:length(ae_list)], num = 1:length(ae_list)),
             by = "ae"
          ) %>% dplyr::mutate(
            bg = dplyr::case_when(
              slider > day_end ~ cont,
              slider <= day_end ~ col
            )
          ) %>% dplyr::arrange(patient,desc(r))


        if (dim(tmp)[1] > 0) {
          poly_t <- function(num, rad = 1, fg = par('fg'), bg = par('fg'),num_aes = length(ae_list),...) {
            x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
            y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
            polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
            NULL
          }
          poly_t2 <- function(num, rad = 1, fg = par('fg'), bg = par('fg'),num_aes = length(ae_list),...) {
            x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 2)))
            y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 2)))
            return(c(mean(x_tmp, x_tmp[1]), mean(y_tmp, y_tmp[1])))
          }

          #filter for imputed data
          arrow_tmp <- tmp %>%
            dplyr::filter(replace_ae_start + replace_ae_end != 0 )

          if (length(ae_list) > 1) {
          my.symbols(
            x = tmp$X,
            y = tmp$Y,
            symb = poly_t,
            num = tmp$num,
            rad = tmp$r,
            bg = tmp$bg,
            fg = tmp$col,
            xsize = 2,
            add = TRUE
          )
            if (show_arrows) {
              if(!is.null(arrow_tmp)){
              if(!dim(arrow_tmp)[1] == 0){
              for(i in 1:dim(arrow_tmp)[1]){
                coord <- poly_t2(arrow_tmp[i,]$num,1,num_aes = length(ae_list))
                if(arrow_tmp[i,]$replace_ae_start == 1 & arrow_tmp[i,]$replace_ae_end == 0){
                  text(arrow_tmp[i,]$X+coord[1], arrow_tmp[i,]$Y+coord[2],expression(symbol("\334")),col ="#bababa",cex=1.5)
                }
                if(arrow_tmp[i,]$replace_ae_start == 0 & arrow_tmp[i,]$replace_ae_end == 1){
                  text(arrow_tmp[i,]$X+coord[1], arrow_tmp[i,]$Y+coord[2],expression(symbol("\336")),col ="#bababa",cex =1.5)
                }
                if(arrow_tmp[i,]$replace_ae_start == 1 & arrow_tmp[i,]$replace_ae_end == 1){
                  text(arrow_tmp[i,]$X+coord[1], arrow_tmp[i,]$Y+coord[2],expression(symbol("\333")),col ="#bababa",cex=1.5)
                }
              }
              }
              }
            }
          } else if (length(ae_list) == 1) {
            graphics::symbols(
              tmp$X,
              tmp$Y,
              circles = 0.85 * tmp$r,
              inches = FALSE,
              add = TRUE,
              fg = tmp$col,
              bg = tmp$bg,
              lwd = 1
            )
            if (show_arrows){
              if(!is.null(arrow_tmp)){
              if(!dim(arrow_tmp)[1] == 0){
              for(i in 1:dim(arrow_tmp)[1]){
                if(arrow_tmp[i,]$replace_ae_start == 1 & arrow_tmp[i,]$replace_ae_end == 0){
                  text(arrow_tmp[i,]$X, arrow_tmp[i,]$Y,expression(symbol("\334")),col ="#bababa",cex=1.5)
                }
                if(arrow_tmp[i,]$replace_ae_start == 0 & arrow_tmp[i,]$replace_ae_end == 1){
                  text(arrow_tmp[i,]$X, arrow_tmp[i,]$Y,expression(symbol("\336")),col ="#bababa",cex=1.5)
                }
                if(arrow_tmp[i,]$replace_ae_start == 1 & arrow_tmp[i,]$replace_ae_end == 1){
                  text(arrow_tmp[i,]$X, arrow_tmp[i,]$Y,expression(symbol("\333")),col ="#bababa",cex=1.5)
                }
              }
            }
              }
            }}
        }
      }
      #draw arrows
      # highlight selected subject/adverse event(s)
      if (!is.null(info)) {
        if (dim(info)[1] == 1) {
          graphics::symbols(
            info$X,
            info$Y,
            circles = cbind(rep(1, length(info$Y))),
            inches = FALSE,
            add = TRUE,
            fg = "#ffffff10",
            bg = "#ffffff10",
            lwd = 3,
            xlab = "",
            ylab = "",
            main = "",
          )
        }
      }
      if (!is.null(legend_ae)) {
        if (length(legend_ae) != 0) {
          tmp_clicked <- tmp %>%
            dplyr::filter(ae == legend_ae)
          if (dim(tmp_clicked)[1] > 0) {
            graphics::symbols(
              tmp_clicked$X,
              tmp_clicked$Y,
              circles = cbind(rep(1, length(tmp_clicked$Y))),
              inches = FALSE,
              add = TRUE,
              fg = "#ffffff10",
              bg = "#ffffff10",
              lwd = 3,
              xlab = "",
              ylab = "",
              main = "",
            )
          }
        }
      }
    }
  }
}
