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
#' @param subroup selected subgroup
#' @param slider day
#' @param subjidn subjidn variable
#' @param adepro_colors colors used in adepro (max 12)
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
  )
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

      if(!is.null(subgroup)) {
        tmp_start <- data[data$patient %in% filtered_subjects,]
      } else {
        tmp_start <- data
      }

      if(length(ae_list) > 0) {
        tmp <- tmp_start %>% dplyr::filter(ae %in% ae_list) %>%
          dplyr::filter(day_start <= slider) %>%
          dplyr::left_join(
          patients_tmp %>%
            dplyr::select(ps,X,Y,cont,cont_bg) %>%
            dplyr::rename(patient = ps),
          by = "patient"
        ) %>% dplyr::left_join(
           data.frame(ae = ae_list, col = adepro_colors[1:length(ae_list)], num = 1:length(ae_list)),
           by = "ae"
        ) %>% dplyr::mutate(bg =
          case_when(
            slider > day_end ~ cont,
            slider <= day_end ~ col
          )
        ) %>% arrange(patient,desc(r))

        if(dim(tmp)[1] > 0) {
          poly_t <- function(num, rad = 1, fg = par('fg'), bg = par('fg'),num_aes = length(ae_list),...) {
            x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
            y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / num_aes * (num - 1), pi / 2 - 2 * pi / num_aes * num, length = 25)))
            polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
            NULL
          }


          if(length(ae_list) > 1) {
          TeachingDemos::my.symbols(
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
          } else if (length(ae_list) == 1) {
          graphics::symbols(
            tmp$X,
            tmp$Y,
            circles = 0.85*tmp$r,
            inches = FALSE,
            add = TRUE,
            fg = tmp$col,
            bg = tmp$bg,
            lwd = 1
          )
          }
        }
      }
    }
  }
}
