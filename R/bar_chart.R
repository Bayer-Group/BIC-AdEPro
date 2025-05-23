utils::globalVariables(c("ae", "day_slider", "vars", "day_mx", "count_mx", "ps", "treat", "summarise", "n", ".", "N"))

#' bar_chart - creates barchart of AEs for all patient
#' @description
#' Creates bar charts daily and total for all patients separately according to treatment (R Package required: 'dplyr')
#'
#' @param ae_data Adverse event dataset
#' @param patients Patient dataset
#' @param day The study day of interest
#' @param variables Vector of Adverse events
#' @param day_max Maximum Day
#' @param count_max Maximum Counts
#' @param cex.n Font size of the text in the bars
#' @param color_theme logical value for dark/light theme
#'
#' @keywords internal

bar_chart <- function(
  ae_data = ae,
  patients = patient,
  day = day_slider,
  variables = vars,
  day_max = day_mx,
  count_max = count_mx,
  treatments = treatments,
  cex.n = 2,
  color_theme = TRUE
){

  #app colors
  colors <- c(
    "#e43157", "#377eb8", "#4daf4a", "#984ea3",
    "#ff7f00", "#ffff33", "#a65628", "#f781bf",
    "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
  )

  if(color_theme) {
    bg_col <- "#424242"
    fg_col <- "#383838"
    text_col <- "white"
  } else {
    bg_col <- "#ffffff"
    fg_col <- "#dedede"
    text_col <- "black"
  }
  # merge ae's and colors
  col_ae <- data.frame('ae' = variables,
                       'col' = colors[1:length(variables)])
  col_ae$ae <- factor(col_ae$ae, levels = col_ae$ae)
  #rename variable ps from patient data set for merging
  patient <- patients %>%
    dplyr::rename(patient = ps)
  #merging ae and patient data set as tot (Total)
  tot <- ae_data %>%
    dplyr::right_join(patient %>%
                 dplyr::select(patient, treat), by = 'patient') %>%
                 na.omit()
  #filter for used variables (ae's)
  tot <- tot %>%
    dplyr::filter(ae %in% variables)
  #'refactor' to remove non used ae's
  tot$ae <- factor(tot$ae, levels = variables)

    #use count_event function to calculate the counts of AE's in Treatment groups
    #and total
    count_ev <- count_event(total = tot, day = day)
    count_ev$ae <- factor(count_ev$ae, levels = col_ae$ae)

    count_ev2 <- count_ev %>%
      dplyr::left_join(col_ae, by = 'ae') %>%
      dplyr::left_join(patients %>%
                         dplyr::select(ps,treat) %>%
                         unique() %>%
                         dplyr::group_by(treat) %>%
                         dplyr::summarise(N = n()), by = 'treat') %>%
      dplyr::mutate(n_rel = n/N)

    #create multiple plots
    on_ex <- par("mfrow", "mai")
    on.exit(par(on_ex))
    par(mfrow = c(2, length(treatments)) , mai = c(1.12, 0.82, 0.62, 0.42),  bg = bg_col)

    #for every treatment group + Total draw a barplot
    for(i in 1:(length(treatments))){

      vec <- count_ev2 %>%
        dplyr::filter(treat == treatments[i]) %>%
        .$n

      names(vec) = variables

        ylim <- c(0, count_max + (count_max/15))

        posi <-  vec + (count_max/30)

        axpos <- c(0, 2 * count_max)

      #draw barplot
      Barplot <- graphics::barplot(vec,
              cex.main = 1.4,
              col.main = text_col,
              col = c(as.character(count_ev2 %>%
                                     dplyr::filter(treat == treatments[i]) %>%
                                     .$col)),
              ylim = ylim,
              ylab = "",
              las = 2,
              add = FALSE,
              col.lab = text_col,
              main = paste0(treatments[i], "\n", "AE Frequency on Day ", day),
              axisnames = FALSE,
              cex.lab = 1,
              axes = FALSE,
              border = NA,
              font.axis = 2,
              col.axis =text_col
              )
      graphics::axis(2, col = text_col, col.axis = (text_col), cex.axis = 1.7, las = 1)
      graphics::axis(2, axpos, labels = FALSE, col = text_col, col.axis = (text_col), cex.axis = 1.7, las = 1)
      #numbers over bars
      text(Barplot, posi, paste0(round(vec,1)), cex = cex.n, col = text_col)
    }


    y_max <- as.numeric(table(rep((tot %>%
                                     drop_na() %>%
                                     .$ae),
                                  ifelse((pmin(day,(tot %>%
                                                      drop_na() %>%
                                                      .$day_end)) - (tot %>%
                                                                       drop_na() %>%
                                                                       .$day_start) + 1) < 1 ,
                                         0, pmin(day,(tot %>%
                                                        drop_na() %>%
                                                        .$day_end) - (tot %>%
                                                                        drop_na() %>%
                                                                        .$day_start) + 1))))[1])
    #create an empty matrix for the total values
    res_max <- matrix(NA, length(treatments), length(variables))
    for (i in 1:(length(treatments))) {
    vec <- table(rep((tot %>%
                        drop_na() %>%
                        filter(treat == treatments[i])%>%
                        .$ae),
                     ifelse((pmin(day_max,(tot %>%
                                             drop_na() %>%
                                             filter(treat == treatments[i]) %>%
                                             .$day_end)) - (tot %>%
                                                              drop_na() %>%
                                                              filter(treat == treatments[i]) %>%
                                                              .$day_start) + 1) < 1 ,
                            0, pmin(day_max,(tot %>%
                                               drop_na() %>%
                                               filter(treat == treatments[i])) %>%
                                      .$day_end) - (tot %>%
                                                      drop_na() %>%
                                                      filter(treat == treatments[i]) %>%
                                                      .$day_start) + 1)))
    res_max[i,] <- vec
    }

    res_max <- max(res_max)

    result <- matrix(NA, length(treatments), length(variables))

      axpos <- c(0, 2 * res_max)

    for(i in 1:(length(treatments))){
      vec <- table(
        rep(
          (tot %>%
            drop_na() %>%
            filter(treat == treatments[i]) %>%
            .$ae
          ),
          ifelse(
            (pmin(day,
              (tot %>%
                 drop_na() %>%
                 filter(treat == treatments[i]) %>%
                 .$day_end)) - (tot %>%
                                  drop_na() %>%
                                  filter(treat == treatments[i]) %>%
                                  .$day_start) + 1) < 1 ,
                              0, pmin(day,(tot %>%
                                             drop_na() %>%
                                             filter(treat == treatments[i])) %>%
                                        .$day_end) - (tot %>%
                                                        drop_na() %>%
                                                        filter(treat == treatments[i]) %>%
                                                        .$day_start) + 1)))
      result[i,] <- vec

      Barplot <- graphics::barplot(
        vec,
        cex.main = 1.4,
        col = c(as.character(count_ev2 %>%
                               dplyr::filter(treat == treatments[i]) %>%
                               .$col)),
        ylab = "",
        las = 2,
        ylim = c(0, res_max + (res_max/15)),
        add = FALSE,
        col.lab = text_col,
        main = paste0(treatments[i], "\n", "Patient Days with AE ", "\n"," until Day ", day),
        col.main = text_col,
        cex.lab = 1,
        axes = FALSE,
        border = NA,
        axisnames = FALSE,
        font.axis = 2,
        col.axis = text_col,
        col.sub = text_col,
        cex.sub = 1.6
      )
      graphics::axis(2, col = text_col, col.axis = (text_col), cex.axis = 1.7, las = 1)
      graphics::axis(2, axpos, labels = FALSE, col = text_col, col.axis = (text_col), cex.axis = 1.7, las = 1)
      text(Barplot, vec + res_max/30, paste0(round(vec, 1)), cex = cex.n, col = text_col)
    }
}
