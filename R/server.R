utils::globalVariables(c("day_end", "day_start", "r", "ps", "treat", "N", ".", "end", "death", "SEQUENCING"))

#' Server part of the AdEPro application
#'
#'@return No return value. Server part of the app, used in launch_adepro-function.
#'
#'@keywords internal

server <- shiny::shinyServer(function(input, output, session) {

  #Shiny options for maximal upload size
  options(shiny.maxRequestSize = 110*1024^2)

  #define adverse event colors
  adepro_colors <- c(
    "#e43157", "#377eb8", "#4daf4a", "#984ea3",
    "#ff7f00", "#ffff33", "#a65628", "#f781bf",
    "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
  )

  #### Data upload ####
  # use shinyjs package to disable the submit until data are uploaded and checked
  shinyjs::disable("submit")

  # create a reactive Value called 'submit_flag' and set initial value to 0
  # use reactive value submit_flag$dat to create an output$submitted which can be
  # used with outputOptions() for the conditionalPanel() function.
  # e.g.: condition = "output.submitted == 1" if submit button was pressed.
  submit_flag <- shiny::reactiveValues(dat = 0)
  output$submitted <- shiny::reactive(submit_flag$dat)
  shiny::outputOptions(output, "submitted", suspendWhenHidden = FALSE)
  # The reactive value submit_flag$dat will be updated if submit button was pressed.
  shiny::observeEvent(input$submit, {
    submit_flag$dat <- 1
  })

  shiny::observeEvent(input$return_upload, {
    submit_flag$dat <- 0
  })

  output$file_upload <- shiny::renderUI({
      shiny::uiOutput('tot_dat')
  })

  shiny::observeEvent(c(input$reset_fileinput_adae), {
    shinyjs::reset('tot_dat')
    infile_adae$val <- NULL
  })
  shiny::observeEvent(c(input$reset_fileinput_adsl), {
    shinyjs::reset('tot_dat2')
    infile_adsl$val <- NULL
  })
  # create a reactive Value called 'loaded' and set initial value to 0
  # use reactive value loaded$dat to create an output$load which can be
  # used with outputOptions() for the conditionalPanel() function.
  # e.g.: condition = "output.load == 1" if adae data are uploaded.
  # The reactive value loaded$dat will be updated in prepared_merged_data_reac() if
  # adae is available.
  loaded <- shiny::reactiveValues(dat = 0)
  output$load <- shiny::reactive(loaded$dat)
  shiny::outputOptions(output, "load", suspendWhenHidden = FALSE)

  #### Top panel ####
  output$speed <- shiny::renderUI({
    #show Speed slider only if data were uploaded/ upload was submitted
    shiny::req(submit_flag$dat)
    if (submit_flag$dat > 0) {
      shinyWidgets::knobInput(
        inputId = "speed",
        label = shiny::HTML('<p style="color:white"> Animation Speed (sec.)</p>'),
        value = 2,
        min = 1,
        max = 10,
        step = 1,
        inputColor = "white",
        fgColor = "#377EB8",
        width = "70px",
        height = "70px",
        cursor = TRUE
      )
    } else {
      NULL
    }
  })

  # slider of study days - required adverse event data
  output$slider <- shiny::renderUI({
    prepared_merged_data_reac()
    if (is.null(shiny::isolate(ae_data())) | is.null(shiny::isolate(patient_data()))) {
      return(NULL)
    }
    ae_data <- shiny::isolate(ae_data())
    #use latest end day as slider end
    day_max <- ifelse(
      length(ae_data$day_end) == 0,
      1,
      max(ae_data$day_end, na.rm = TRUE)
    )

    shiny::sliderInput(
      inputId = "slider",
      label = NULL,
      min = 1,
      max = day_max,
      value = 1, step = 1,
      width = '100%'
    )
  })

   output$circle_legend<- shiny::renderUI({
    # if(!is.null(input$heightSlider)) {
     shiny::plotOutput(
       outputId = "legend",
       height = "800px",
       click = clickOpts(id = "legend_click"),
      )
    # }
  })

  output$circle_legend2<- shiny::renderUI({
    # if(!is.null(input$heightSlider)) {
     shiny::plotOutput(
       outputId = "legend2",
       height = "800px"
      )
    # }
  })

  output$barchart_legend<- shiny::renderUI({
    shiny::plotOutput(
      outputId = "legend_bar",
      height = "800px"
    )
  })

  legend_click <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$legend_click,{
    legend_click$val <- input$legend_click
  })
  # create a legend with function 'pie_legend'
  output$legend <- shiny::renderPlot({
    session$clientData$output_slicePlots_width
    session$clientData$output_legend_width
    session$clientData$output_circle_legend_width
    session$clientData$output_circle_legend2_width
    input$heightSlider
    ## input$zoom
    if (is.null(input_var())) {
      return(NULL)
    } else {
      pre_value_legend_ae <- shiny::isolate(legend_ae$val)

      if (length(input_var()) > 0) {
        colors = c(
          "#e43157", "#377eb8", "#4daf4a", "#984ea3",
          "#ff7f00", "#ffff33", "#a65628", "#f781bf",
          "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
        )
        #create dummy data set to draw legend
        tmp <- data.frame(
          "day_start"=rep(1, 12),
          "day_end" = rep(3, 12),
          "patient" = 1:12,
          "ae" = c(input_var(),rep(NA, 12 - length(input_var()))),
          "sev" = rep(3, 12),
          "r" = rep(1, 12),
          "d" = rep(NA, 12),
          "Y" = rev(seq(1, 12 * 3, by = 3)),
          "X" = rep(1, 12),
           "cont" = "#424242",
          "cont_bg" = c(rep("#383838", length(input_var())), rep("#383838", 12 - length(input_var()))),
          "col" = c(colors[1:length(input_var())], rep(NA, 12 - length(input_var()))),
          "num" = c(1:length(input_var()), rep(NA, 12 - length(input_var()))),
          "bg" = c(colors[1:length(input_var())], rep(NA, 12 - length(input_var())))
        )
        #get information about nearest ae clicked
        info <- shiny::nearPoints(
          tmp,
          legend_click$val,
          threshold = 30,
          maxpoints = 1,
          xvar = "X",
          yvar = "Y"
        )
      } else {
        info <- NULL
      }

      #compare ae clicked before to remove an ae after second click
      post_value_legend_ae <- info$ae

      if (is.null(post_value_legend_ae)) {
        legend_ae$val <- NULL
        info <- NULL
      } else if (length(post_value_legend_ae) == 0) {
        legend_ae$val <- NULL
        info <- NULL
      }   else if (is.na(post_value_legend_ae)) {
        legend_ae$val <- NULL
        info <- NULL
      } else if (is.null(pre_value_legend_ae)) {
         plot_click$val <- NULL
        legend_ae$val <-  post_value_legend_ae

      } else if (post_value_legend_ae == pre_value_legend_ae){
         legend_ae$val <- NULL
         info <- NULL
      } else {
        plot_click$val <- NULL
        legend_ae$val <- post_value_legend_ae
      }
      #draw legend
      tmp <- pie_legend2(
        tmp = tmp,
        aes = input_var(),
        legend_click = legend_click$val,
        info = info
      )
    }
  }, bg = "#424242")

  legend_ae <- shiny::reactiveValues(val = NULL)

  # create a legend with function 'pie_circle'
  output$legend2 <- shiny::renderPlot({
    session$clientData$output_slicePlots_width
    session$clientData$output_legend_width
    session$clientData$output_circle_legend_width
    session$clientData$output_circle_legend2_width
    input$heightSlider
    if (is.null(input_var())) {
      return(NULL)
    } else {
      return(circle_legend2(aes = input_var(),grading=ifelse(input$severity_grading_flag=="Severity",FALSE,TRUE)))
    }
  }, bg = "#424242")

  #### create a legend for barchart
  output$legend_bar <- shiny::renderPlot({
    session$clientData$output_barchart_width
    # session$clientData$output_legend_width
    # session$clientData$output_circle_legend_width
    # session$clientData$output_circle_legend2_width
    input$add_row
    input$rem_row
    input$type
    input$heightSlider
    input$plus_zoom
    input$minus_zoom
    ## input$zoom
    if (is.null(input_var())) {
      return(NULL)
    } else {
      pre_value_legend_ae <- shiny::isolate(legend_ae$val)

      if (length(input_var()) > 0) {
        colors = c(
          "#e43157", "#377eb8", "#4daf4a", "#984ea3",
                   "#ff7f00", "#ffff33", "#a65628", "#f781bf",
                   "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
        )
        #create dummy data set to draw legend
        tmp <- data.frame(
          "day_start"=rep(1, 12),
          "day_end" = rep(3, 12),
          "patient" = 1:12,
          "ae" = c(input_var(),rep(NA, 12 - length(input_var()))),
          "sev" = rep(3, 12),
          "r" = rep(1, 12),
          "d" = rep(NA, 12),
          "Y" = rev(seq(1, 12 * 3, by = 3)),
          "X" = rep(1, 12),
          "cont" = "#424242",
          "cont_bg" = c(rep("#383838", length(input_var())), rep("#383838", 12 - length(input_var()))),
          "col" = c(colors[1:length(input_var())], rep(NA, 12 - length(input_var()))),
          "num" = c(1:length(input_var()), rep(NA, 12 - length(input_var()))),
          "bg" = c(colors[1:length(input_var())], rep(NA, 12 - length(input_var())))
        )
        #get information about nearest ae clicked
        info <- shiny::nearPoints(
          tmp,
          legend_click$val,
          threshold = 30,
          maxpoints = 1,
          xvar = "X",
          yvar = "Y"
        )
      } else {
        info <- NULL
      }

      #compare ae clicked before to remove an ae after second click
      post_value_legend_ae <- info$ae

      if (is.null(post_value_legend_ae)) {
        legend_ae$val <- NULL
        info <- NULL
      } else if (length(post_value_legend_ae) == 0) {
        legend_ae$val <- NULL
        info <- NULL
      }   else if (is.na(post_value_legend_ae)) {
        legend_ae$val <- NULL
        info <- NULL
      } else if (is.null(pre_value_legend_ae)) {
        plot_click$val <- NULL
        legend_ae$val <-  post_value_legend_ae

      } else if (post_value_legend_ae == pre_value_legend_ae){
        legend_ae$val <- NULL
        info <- NULL
      } else {
        plot_click$val <- NULL
        legend_ae$val <- post_value_legend_ae
      }
      #draw legend
      tmp <- barchart_legend(
        tmp = tmp,
        aes = input_var(),
        legend_click = legend_click$val,
        info = info
      )
    }
  }, bg = "#424242")


  # # create a legend with function 'pie_circle'
  # output$legend2 <- shiny::renderPlot({
  #   session$clientData$output_slicePlots_width
  #   session$clientData$output_legend_width
  #   session$clientData$output_circle_legend_width
  #   session$clientData$output_circle_legend2_width
  #   input$heightSlider
  #   if (is.null(input_var())) {
  #     return(NULL)
  #   } else {
  #     return(circle_legend2(aes = input_var()))
  #   }
  # }, bg = "#424242")

  # CSS style information of the output$dayinfo - see below (actual day number of the slider)
  output$dynamic_css <- shiny::renderUI({
    shiny::req(input$slider)
    shiny::tags$style(
      type = "text/css",
      "#dayinfo { overflow-y: hidden; overflow-x: hidden }",
      paste0(
        "#dayinfo {background-color: #E43157; width: ",
        15 + (22 * nchar(input$slider)),
        "px; color: #ffffff; text-align = left;
        border-radius: 10px;
        border-color: #383838;
        font: Arial;
        font-size: 40px;}"
      )
    )
  })

  # The acutal day number of the slider in a bigger font size
  output$dayinfo <- shiny::renderText({
    if (is.null(ae_data()) & is.null(patient_data())) {
      return(cat(""))
    } else {
      input$slider
    }
  })

  #### Modify data (collapsePanel) ####
  #### ... AI ####
  output$varSeq <- shiny::renderUI({
    shiny::req(all_aes())
    choices <- all_aes()
    if(input$ae_var_sorting == "frequency") {
      choices <- unlist(lapply(strsplit(choices, " \\(N = "), first))
    } else if(input$ae_var_sorting == "days") {
      choices <- unlist(lapply(strsplit(choices, " \\(days of ae = "), first))
    }
    choices <- c(
      choices,
      paste(choices, "sev1", sep = "_"),
      paste(choices, "sev2", sep = "_"),
      paste(choices, "sev3", sep = "_")
    )
    most_freq_aes <- head(choices, 8)
    shinyWidgets::pickerInput(
      inputId = 'varSeq',
      label = 'Sequencing input',
      choices,
      multiple = TRUE,
      selected = most_freq_aes,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = 'count > 0',
        `count-selected-text` = '{0} selected (of {1})',
        `live-search`=TRUE,
        `style`='background: btn-primary',
        `header`='Select multiple items',
        `none-selected-text`='All dropped!'
      )
    )
  })

  output$ae_type <- shiny::renderUI({
    global_params <- shiny::isolate(global_params())
    shiny::selectizeInput(
      inputId = "type",
      label = "Select type of Adverse Event:",
      choices  = c(global_params$AE_options),
      selected = 1
    )
  })

  output$ae_sorting <- shiny::renderUI({
    if (is.null(shiny::isolate(ae_data())) | is.null(shiny::isolate(patient_data()))) {
      return(NULL)
    }
    var_sort <- colnames(prepared_merged_data_reac()$pat_data)[-c(1:4)]
    shiny::selectInput(
      inputId = "sorting",
      label = "Sort patients by:",
      choices = c(var_sort),
      selected = "SUBJIDN"
    )
  })

  AI.AdePro_reac <- shiny::reactiveValues(val = 1)

  shiny::observeEvent(c(input$AI.Update, patient_data()), {
    AI.AdePro_reac$val <- AI.AdePro_reac$val + 1
  })

  # height Slider
  output$heightSlider <- shiny::renderUI({
    shiny::sliderInput(
      inputId = "heightSlider",
      label = "Choose Plot height (in pixel)",
      value = 800,
      min =  400,
      max = 1600,
      step = 100
    )
  })

  shiny::observeEvent(input$subgroup, {
    if (!is.null(input$subgroup)) {
      ind <- length(unique(patients()[[input$subgroup]]))
      value <- min(c(800 * ind, 8000))
      max <- min(c(1600 * ind, 8000))
      shiny::updateSliderInput(
        session,
        inputId = "heightSlider",
        max = max
      )
    } else {
      shiny::updateSliderInput(
        session,
        inputId = "heightSlider",
        max = 1600
      )
    }
  })

  #number of rows
  output$numberRows <- shiny::renderUI({
    shiny::req(patient_data())
    patient <- patient_data()
    value <- ceiling(sqrt((850/(1920 * 1)) * nrow(patient)))
    shiny::sliderInput(
      inputId = "numberRows",
      label = "Number of Rows",
      min = 1,
      max = 2 * value,
      value = value,
      step = 1
    )
  })

  #### Adverse Event for animation (collapsePanel) ####
  output$ae_var <- shiny::renderUI({
    shiny::req(all_aes())
    most_freq_aes <- head(all_aes(), 8)
    rare_ae <- all_aes()[length(all_aes())]

    shiny::selectizeInput(
      inputId = "var",
      label = "Choose Adverse Events for display (max. 12):",
      options  = list(
        placeholder = "Please select an adverse event!",
        maxItems = 12,
        'plugins' = list('remove_button','drag_drop')
      ),
      choices  = all_aes(),
      selected = most_freq_aes,
      multiple = TRUE
    )
  })

  # observe button click to remove all adverse events from selectizeInput
  shiny::observeEvent(input$remove_all_aes, {
    shiny::updateSelectizeInput(
      session,
      inputId ="var",
      choices = all_aes(),
      selected = NULL
    )
  })

  output$ae_audio <- shiny::renderUI({
    if (is.null(ae_data()) | is.null(patient_data())) {
      return(NULL)
    }
    rare_ae <- all_aes()[length(all_aes())]
    shiny::selectInput(
      inputId = "ae_audio",
      label = "Choose Adverse Event for audio:",
      choices = all_aes(),
      selected = rare_ae
    )
  })

  output$ae_sound1 <- shiny::renderUI({
    if (is.null(ae_data()) | is.null(patient_data())) {
      return(NULL)
    }
    trt <- levels(factor(patient_data()$treat))
    shiny::selectInput(
      inputId = "sound1",
      label = "Choose Treatment Group for first sound:",
      choices = c("- none -", trt),
      selected = "- none -"
    )
  })

  output$ae_sound2 <- shiny::renderUI({
    if (is.null(ae_data()) | is.null(patient_data())) {
      return(NULL)
    }
    trt <- levels(factor(patient_data()$treat))
    shiny::selectInput(
      inputId = "sound2",
      label = "Choose Treatment Group for second sound:",
      choices = c("- none -", trt),
      selected = "- none -"
    )
  })

  plot_click <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(c(input$plot_click, input$subgroup), {
    if (is.null(input$subgroup)) {
      legend_ae$val <- NULL
      plot_click$val <- input$plot_click
    } else {
      legend_ae$val <- NULL
      plot_click$val <- NULL
    }
  })

  prev_clicked_subject <- shiny::reactiveValues(val = NULL)

  reac_info_click <- eventReactive(c(plot_click$val), {
    info <- shiny::nearPoints(
      patients(),
      plot_click$val,
      threshold = 30,
      maxpoints = 1,
      xvar = "X",
      yvar = "Y"
    )
    if (dim(info)[1] == 1) {
      if (!is.null(shiny::isolate(prev_clicked_subject$val))) {
        if (info$X == shiny::isolate(prev_clicked_subject$val$X) & info$Y == shiny::isolate(prev_clicked_subject$val$Y)) {
           prev_clicked_subject$val <- NULL
           info <- NULL
           legend_click$val <- NULL
        } else {
          prev_clicked_subject$val <- info
          legend_click$val <- NULL
        }
      } else {
        prev_clicked_subject$val <- info
        legend_click$val <- NULL
      }
    }# else {
    #  legend_ae$val <- NULL
    #}
    info
  }, ignoreNULL = FALSE)

  #### Piecharts ####
  output$slicePlots <- shiny::renderPlot({
    prepared_merged_data_reac2()

    if (is.null(prepared_merged_data_reac2()) | is.null(ae_data()) | is.null(patient_data())) {
      return(NULL)
    }

    #react to the following input$ variables to re-generate plot
    session$clientData$output_slicePlots_width
    input$add_row
    input$rem_row
    input$heightSlider
    input$plus_zoom
    input$minus_zoom

    input$type
    shiny::req(input$type)
    shiny::req(data_type())
    shiny::req(global_params())
    info <- reac_info_click()

    adepro_slice_plot(
      data = selected_slice_plot_data(),
      patients = patients(),
      ae_list = input_var(),
      global_params = global_params(),
      height = global_params()$height,
      width  = global_params()$width,
      xlines = global_params()$xlines,
      ylines = global_params()$ylines,
      xval = c(0, cumsum(global_params()$plines[[1]])[-length(global_params()$plines[[1]])]) + global_params()$plines[[1]]/2,
      title = as.character(unique(patients()$treat)),
      subgroup = input$subgroup,
      subjidn = input$sel_subjidn,
      slider = input$slider,
      info = info,
      legend_ae = legend_ae$val,
      arrow_data = prepared_merged_data_reac2()$ae_data,
      show_arrows = input$show_imputations
    )

    #sound
    counts <- counts()
    tf1 <- max(c(0, counts$freq[which(counts$day == input$slider & counts$treat == input$sound1)]))
    tf2 <- max(c(0, counts$freq[which(counts$day == input$slider & counts$treat == input$sound2)]))
    tone(tf1, tf2)

    }, bg = "#424242", height = function(){heightSlider$val}, width = function() {
    session$clientData$output_slicePlots_width
    }
  )

  #### Barplots ####
  output$barchart <- shiny::renderPlot({
    session$clientData$output_barchart_width
    input$add_row
    input$rem_row
    input$type
    input$heightSlider
    input$plus_zoom
    input$minus_zoom
    prepared_merged_data_reac2()
    shiny::req(input_var())
    shiny::req(data_type())
    shiny::req(count_max())
    shiny::req(prepared_merged_data_reac2())
    shiny::req(all_aes())
    ae <- data_type()
    patient <- prepared_merged_data_reac2()$pat_data
    day_slider <- input$slider
    vari <- input_var()
    day_mx <- ifelse(
      length(ae$day_end) == 0,
      1,
      max(ae$day_end)
    )
    count_mx <- count_max()

    bar_chart(
        ae_data = ae,
        patients = patient,
        day = day_slider,
        variables = vari,
        day_max = day_mx,
        count_max = count_mx,
        treatments = levels(patient$treat)[1:length(unique(patient$treat))],
        cex.n = (((2.5 - 1.5) * heightSlider$val + (1.5 * 1600 - 3 * 400)) / (1600 - 400))
      )
    }, bg = "#424242", height = function(){heightSlider$val }, width = function() {
    session$clientData$output_barchart_width}
  )

  #### Absolute Panels ####
    ####... Number of Rows ####
    output$rowpanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = "controls",
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        top = 80,
        left = "auto",
        right = 25,
        bottom = "auto",
        width = 75,
        height = 75,
        shiny::HTML('<p style="color:white"> Number of Rows: </p>'),
        shiny::fluidRow(
          shiny::column(2,
            div(style = "display:inline-block",
              shinyWidgets::circleButton(
                inputId = "rem_row",
                icon = icon("minus"),
                size = "xs",
                status = "primary"
              )
            )
          ),
          shiny::column(2,
            div(style = "display:inline-block",
              shinyWidgets::circleButton(
                inputId = "add_row",
                icon = icon("plus"),
                size = "xs",
                status = "primary"
              )
            )
          )
        )
      )
    })

    ####... Height ####
    output$zoompanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = "controls",
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        top = 5,
        left = "auto",
        right = 25,
        bottom = "auto",
        width = 75,
        height = 75,
        shiny::HTML('<p style="color:white"> Change Plot height: </p>'),
        shiny::fluidRow(
          shiny::column(2,
            div(style = "display:inline-block",
              shinyWidgets::circleButton(
                inputId = "minus_zoom",
                icon = icon("minus"),
                size = "xs",
                status = "primary"
              )
            )
          ),
          shiny::column(2,
            div(style = "display:inline-block",
              shinyWidgets::circleButton(
                inputId = "plus_zoom",
                icon = icon("plus"),
                size = "xs",
                status = "primary"
              )
            )
          )
        )
      )
    })

  ####... Patient Information ####
  output$overall_info <- shiny::renderPrint({
    if (is.null(ae_data()) | is.null(patient_data())) {
      return(cat(""))
    }
    patients <- patients()
    global_params <- global_params()
    footnote <- global_params$footnote
    # proportion of TEAEs that have already started
    prop <- round(mean(ae_data()$day_start <= input$slider) * 100, 0)
    txt <- footnote
    cat(txt, paste(prop, "% of TEAEs have \nalready started", sep = ""), sep = "\n")
  })


  hoverinf <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$plot_hover, {
    if (!is.null(input$plot_hover)) {
      hoverinf$val <- input$plot_hover
    }
  })

  output$plot_hoverinfo <- shiny::renderPrint({
    shiny::req(prepared_merged_data_reac2())
    if (is.null(ae_data()) | is.null(patient_data())) {
      return(cat(""))
    }
    patients <- patients()
    global_params <- global_params()
    footnote <- global_params$footnote
    # proportion of TEAEs that have already started
    prop <- round(mean(ae_data()$day_start <= input$slider) * 100, 0)

    info <- shiny::nearPoints(
      patients,
      hoverinf$val,
      threshold = 30,
      maxpoints = 1,
      xvar = "X",
      yvar = "Y"
    )

    if(nrow(info) > 0) {
      info <- info[-c(2:4, length(info), length(info) - 1)]
      pr <- paste("Subject ID:", info[1])
      if (ncol(info) != 2) {
        pr2 <- sapply(2:(ncol(info) - 1), function(x) paste(colnames(info)[x], ": ", info[, x], sep = ""))
        for (i in 1:length(pr2)) {
          pr <- paste(pr, pr2[i], sep = "\n")
        }
      }
      txt <- pr
    } else {
      txt <- "(hover over circle)"
    }
    cat(txt, sep = "\n")
  })

  output$patientpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "controls",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      shiny::HTML(
        paste0(
          "<button style = 'background: #424242;
          color:#ffffff',
          data-toggle = 'collapse' data-target = '#demo3'>
          <i class='fa-solid fa-compress'></i> Info Box</button>"
        )
      ),
      top = 45,
      left = "auto",
      right = 105,
      bottom = "auto",
      width = 250,
      height = "auto",
      tags$div(id = 'demo3',  class = "collapse",
         shiny::HTML('<p style="color:white"> Overall information: </p>'),
         shiny::fluidRow(
          shiny::column(12,
            shiny::verbatimTextOutput("overall_info")
          )
         ),
         shiny::HTML('<p style="color:white"> Patient information: </p>'),
         shiny::fluidRow(
          shiny::column(12,
            shiny::verbatimTextOutput("plot_hoverinfo")
          )
         )
      )
    )
  })

  output$ae_summary_box <- shiny::renderUI({
    shiny::absolutePanel(
      id = "controls",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      shiny::HTML(
        paste0(
          "<button style = 'background: #424242;
          color:#ffffff',
          data-toggle = 'collapse' data-target = '#demo4'>
          <i class='fa-solid fa-compress'></i> Adverse event summary </button>"
        )
      ),
      top = 5,
      left = "auto",
      right = 105,
      bottom = "auto",
      width = 250,
      height = "auto",
      style = "z-index: 10;",
      tags$div(id = 'demo4',  class = "collapse",
        shiny::fluidRow(
          shiny::column(12,
            shiny::uiOutput("summary_text"),
            shiny::radioButtons(
              inputId = "summary_option",
              label = "Display:",
              choices = c("All", "Ongoing", "Resolved"),
              selected = "All",
              inline = TRUE
            ),
            shiny::checkboxInput(
              inputId = "percentage",
              label ="Show percentages",
              value = FALSE
            ),
            shiny::helpText("Note: Every subject is only counted once per adverse event!")
          )
        )
      )
    )
  })

  output$summary_text <- shiny::renderUI({
    if (is.null(ae_data()) | is.null(patient_data())| length(input_var()) <= 0 ) {
      return(NULL)
    } else {
      ae_data0 <- prepared_merged_data_reac2()$ae_data
      Q <- initQ(ae_data0)
      flag_name <- colnames(Q)[as.numeric(input$type)]
      #1. join treatment variable to adverse event data for grouping

      tmp <- dplyr::left_join(
        ae_data() %>%
          dplyr::filter(!!rlang::sym(flag_name) == 1),
        patient_data() %>%
          dplyr::mutate(patient = ps) %>%
          dplyr::select(treat, patient),
        by = "patient"
      )

      # remove rows with same ae for same subject
      # tmp <- tmp[!duplicated(tmp[,c("patient","ae")]),]

      #complete treatment and adverse events table:

      full_list <- merge(levels(tmp$treat), input_var())
      colnames(full_list) <- c("treat", "ae")

      full_list <- full_list[which(full_list$treat %in% input$sortTreatments),]

      #Perform ae_summary update only when input_var are in correct format.
      if(input_var()[1] %in% tmp$ae){
        #2. get number of events until slider day grouped by treatement and events
        tmp2a <- tmp %>%
          dplyr::filter(ae %in% input_var()) %>%
          dplyr::filter(day_start <= input$slider)

        # Check if the filtered dataset is empty
        if (nrow(tmp2a) > 0) {
          tmp2a <- tmp2a %>%
            mutate(Ongoing_AE = case_when(
              day_end >= input$slider ~ 1,
              TRUE ~ 0
            ),
            Resolved_AE = case_when(
              day_end <= input$slider ~ 1,
              TRUE ~ 0
            )) %>%
            dplyr::group_by(patient, ae) %>%
            dplyr::summarise(
              treat = first(treat),
              Ongoing_AE = max(Ongoing_AE, na.rm = TRUE),  # Use na.rm = TRUE to avoid warnings
              Resolved_AE = max(Resolved_AE, na.rm = TRUE)
            )
        } else {
          tmp2a <- data.frame(patient = character(), ae = character(), treat = character(), Ongoing_AE = integer(), Resolved_AE = integer())
        }

        tmp2 <- tmp2a %>%
          dplyr::group_by(treat,ae) %>%
          summarize(
            N = n(),  # Count the total number of rows per treat-ae combination
            Ongoing = sum(Ongoing_AE == 1),  # Count of Ongoing == 1
            Resolved = sum(Resolved_AE == 1 & Ongoing_AE == 0),  # Count of Resolved == 1 where Ongoing == 0
          )

        full_list <- full_list[order(match(full_list$treat, input$sortTreatments)),]

        tmp2 <- tmp2 %>% dplyr::right_join(
          full_list,
          by = c("treat","ae")
        )
        tmp2[is.na(tmp2)] <- 0
        tmp2 <- tmp2[order(match(tmp2$treat, input$sortTreatments)),]
        #3. get number of events until slider day over all treatments
        tmp3 <- tmp2a %>%
          dplyr::group_by(ae) %>%
          summarize(
            N = n(),  # Count the total number of rows per treat-ae combination
            Ongoing = sum(Ongoing_AE == 1),  # Count of Ongoing == 1
            Resolved = sum(Resolved_AE == 1 & Ongoing_AE == 0),  # Count of Resolved == 1 where Ongoing == 0
          )
        #4. join grouped counts and total counts and transfer to wider format
        tmp4 <- dplyr::full_join(
          tmp3,
          tmp2 %>%
            tidyr::pivot_wider(names_from = treat, values_from = c(N, Ongoing, Resolved)),
          by = "ae"
        )

        #4b. replace na's with 0
        tmp4[is.na(tmp4)] <- 0
        #5. create column 'text' with counts: total N (treatment1 N/ treatment2 N/...)
        tmp5 <- tmp4 %>%
          dplyr::mutate(
            N_text = paste0(tmp4[["N"]], " (",apply(tmp4[, -c(1, 2)] %>% select(-contains(c("Ongoing","Resolved"))), 1, paste, collapse = "/"), ")"),
            Ongoing_text = paste0(tmp4[["Ongoing"]], " (",apply(tmp4[, -c(1,3)] %>% select(contains(c("Ongoing"))), 1, paste, collapse = "/"), ")"),
            Resolved_text = paste0(tmp4[["Resolved"]], " (",apply(tmp4[, -c(1,4)] %>% select(contains(c("Resolved"))), 1, paste, collapse = "/"), ")")
          )

        tmp5a  <- tmp5 %>% select("ae","N",starts_with("N_"))
        Ongoing  <- tmp5 %>% select("ae",starts_with("Ongoing"))# %>% rename_with(~ str_replace_all(., "Ongoing", "N"))
        Resolved  <- tmp5 %>% select("ae",starts_with("Resolved"))# %>% rename_with(~ str_replace_all(., "Resolved", "N"))

        #5b. order data frame by input_var() (adverse event selection)
        tmp5a <- tmp5a[match(input_var(),tmp5a$ae),]
        Ongoing <- Ongoing[match(input_var(),Ongoing$ae),]
        Resolved <- Resolved[match(input_var(),Resolved$ae),]

        patients_ <- patients()[order(match(patients()$treat, input$sortTreatments)),]
         N_treat <- patients_ %>%
            dplyr::group_by(treat) %>%
            dplyr::summarise(N = n()) %>%
            tidyr::pivot_wider(names_from = treat, values_from = N, names_prefix = "N_") %>%
            dplyr::mutate(N = rowSums(.)) %>%
            dplyr::select(N, everything())

          Big_N <- N_treat %>% dplyr::mutate(
            N_text = paste0(N_treat[,1],"(",apply(N_treat[, -1], 1, paste, collapse = "/"),")"),
            ae = "N:"
          )
          Big_Ongoing <- N_treat %>%
            rename_with(~ stringr::str_replace_all(., "N_", "Ongoing_")) %>%
            rename("Ongoing"="N") %>%
            dplyr::mutate(
              Ongoing = NA_integer_,
              across(starts_with("Ongoing"), ~ NA_integer_),
              Ongoing_text = "",
              ae = "Ongoing:"
            )
          Big_Resolved <- N_treat %>%
            rename_with(~ stringr::str_replace_all(., "N_", "Resolved_")) %>%
            rename("Resolved"="N") %>%
            dplyr::mutate(
                Resolved = NA_integer_,
                across(starts_with("Resolved"), ~ NA_integer_),
                Resolved_text = "",
                ae = "Resolved:"
              )

        tmp5b <- rbind(Big_N,tmp5a) %>%
          dplyr::relocate(ae)
        Ongoing <- rbind(Big_Ongoing,Ongoing)%>%
          dplyr::relocate(ae)
        Resolved <- rbind(Big_Resolved,Resolved)%>%
          dplyr::relocate(ae)

        #calculate percentages when selected (input$percentage == TRUE):S
        if (input$percentage) {
          tmp5b <- tmp5b %>%
            select(-c(ae, N_text)) %>%
            {round(mapply('/', ., N_treat) * 100, 1)} %>%
            cbind(tmp5b %>% select(ae), .) %>%
            mutate(N_text = paste0(N, " (", paste(!!!rlang::syms(colnames(.)[-c(1, 2)]), sep = "/"), ")"))
          Ongoing <- Ongoing %>%
            select(-c(ae, Ongoing_text)) %>%
            {round(mapply('/', ., N_treat) * 100, 1)} %>%
            cbind(Ongoing %>% select(ae), .) %>%
            mutate(Ongoing_text = case_when(
              !is.na(Ongoing) ~ paste0(Ongoing, " (", paste(!!!rlang::syms(colnames(.)[-c(1, 2)]), sep = "/"), ")"),
              TRUE ~ "")
            )
          Resolved <- Resolved %>%
            select(-c(ae, Resolved_text)) %>%
            {round(mapply('/', ., N_treat) * 100, 1)} %>%
            cbind(Resolved %>% select(ae), .) %>%
            mutate(Resolved_text = case_when(
              !is.na(Resolved) ~ paste0(Resolved, " (", paste(!!!rlang::syms(colnames(.)[-c(1, 2)]), sep = "/"), ")"),
              TRUE ~ "")
            )
          text2 <- c("Percent",input_var())
        } else {
          text2 <- c("N",input_var())
        }
        text3 <- c("Ongoing",input_var())
        text4 <- c("Resolved",input_var())

        if (input$summary_option == "All")  {
           summary_text <- paste(
                "<p style = 'line-height: 0.9; color: ",
                c("white",c("#e43157", "#377eb8", "#4daf4a", "#984ea3",
                                     "#ff7f00", "#ffff33", "#a65628", "#f781bf",
                                     "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
                )[1:length(input_var())]),";'>",
                text2,": ",tmp5b$N_text,"
                </p>", collapse = ""
              )
        } else if (input$summary_option == "Ongoing") {
          summary_text <- paste(
                "<p style = 'line-height: 0.9; color: ",
                c("white",c("#e43157", "#377eb8", "#4daf4a", "#984ea3",
                                     "#ff7f00", "#ffff33", "#a65628", "#f781bf",
                                     "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
                )[1:length(input_var())]),";'>",
                text3,": ",Ongoing$Ongoing_text,"
                </p>", collapse = ""
              )
        } else if (input$summary_option == "Resolved") {
          summary_text <- paste(
                "<p style = 'line-height: 0.9; color: ",
                c("white",c("#e43157", "#377eb8", "#4daf4a", "#984ea3",
                                     "#ff7f00", "#ffff33", "#a65628", "#f781bf",
                                     "#21d4de", "#91d95b", "#b8805f", "#cbbeeb"
                )[1:length(input_var())]),";'>",
                text4,": ",Resolved$Resolved_text,"
                  </p>", collapse = ""
              )
        }
        HTML(
          paste(
            paste(
              "<p style='color:white'> Subjects with adverse event (",names(global_params()$AE_options)[as.numeric(input$type)],") occurrence until day ",input$slider,": Total (", paste(input$sortTreatments, collapse = "/"), ") </p>"
            ),
            summary_text
          , collapse = ""
          )
        )
      }
    }
  })
  #### REACTIVE OBJECTS ####
  #initialize timer with 1 second
  adepro_timer <- shiny::reactiveTimer(1000)
  adepro_n <- 10

  onoff <- shiny::reactiveValues(val = FALSE)

  my <- shiny::reactiveValues(inc = 0,
                              started = FALSE)


  start <- shiny::reactiveValues(dat = 0)
  output$flag <- shiny::reactive(start$dat)
  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)

  ae_input <- shiny::reactiveValues(val = NULL)
  patient_input <- shiny::reactiveValues(val = NULL)


  heightSlider <- shiny::reactiveValues(val = 800)

  infile_adae <- shiny::reactiveValues(val = NULL)
  infile_adsl <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$tot_dat, {
    infile_adae$val <- input$tot_dat$datapath
  })

  shiny::observeEvent(input$tot_dat2, {
    infile_adsl$val <- input$tot_dat2$datapath
  })

  #### Upload adae data ####
  adae_data_reac <- shiny::reactive({
    input$radiobutton_data
    input$reset_fileinput_adae
    inFile <- infile_adae$val
      if (is.null(inFile) & input$radiobutton_data == "File upload") {
        output$wrong_adae_format_text <- shiny::renderUI({
          HTML(
            paste0("
              <b style = 'color:#E43157'>
                Please upload adae data set!
              </b>"
            )
          )
        })
        return(NULL)
      } else if (!is.null(inFile) & input$radiobutton_data == "File upload") {
        split_path <- strsplit(x = inFile, split = "[.]")
        path_ending <- split_path[[1]][length(split_path[[1]])]
        if (path_ending %in% c("csv", "sas7bdat", "sas7cdat")) {
          if (path_ending == "csv") {
            adae <- suppressWarnings(readr::read_csv(inFile, col_types = readr::cols("SEX" = "c"), na = c(".", "NA")))
            colnames(adae) <- toupper(colnames(adae))
          } else if (path_ending == "sas7bdat" | path_ending == "sas7cdat") {
          adae <- haven::read_sas(inFile)
          adae <- adae %>%
            dplyr::select(stats::setNames(colnames(adae), toupper(colnames(adae))))
          }
          output$wrong_adae_format_text <- shiny::renderUI({
              HTML(paste0(""))
            })
        } else {
            output$wrong_adae_format_text <- shiny::renderUI({
              HTML(paste0("
              <b style = 'color:#E43157'>
                Wrong data format! Please upload .csv/.sas7bdat/.sas7cdat files!
              </b>"))
            })
            adae <- NULL
          }
      } else if (input$radiobutton_data == "Demo data") {
        load("data//adae_data.rda")
        adae <- adae_data
         output$wrong_adae_format_text <- shiny::renderUI({
              HTML(paste0(""))
         })
      }
      adae
    })

   #### Upload adsl data ####
   adsl_data_reac <- shiny::reactive({
    input$radiobutton_data
    input$reset_fileinput_adsl
      inFile2 <- infile_adsl$val
      if (is.null(inFile2) & input$radiobutton_data == "File upload") {
        adsl <- NULL
        output$wrong_adsl_format_text <- shiny::renderUI({
            HTML(paste0(""))
        })
      } else if (!is.null(inFile2) & input$radiobutton_data == "File upload") {
        adsl_path <- inFile2
        split_path <- strsplit(x = adsl_path, split = "[.]")
        path_ending <- split_path[[1]][length(split_path[[1]])]
        if (path_ending %in% c("csv", "sas7bdat", "sas7cdat")) {
          if (path_ending == "csv") {
            adsl <- suppressWarnings(readr::read_csv(inFile2, col_types = readr::cols("SEX" = "c"), na = c(".", "NA")))
            colnames(adsl) <- toupper(colnames(adsl))
            #adsl[adsl == "."] <- NA
          } else if (path_ending == "sas7bdat" | path_ending == "sas7cdat") {
            adsl <- haven::read_sas(inFile2)
            adsl <- adsl %>%
              dplyr::select(stats::setNames(colnames(adsl), toupper(colnames(adsl))))
          }
          output$wrong_adsl_format_text <- shiny::renderUI({
            HTML(paste0(""))
          })
        } else {
          output$wrong_adsl_format_text <- shiny::renderUI({
            HTML(paste0("
            <b style = 'color:#E43157'>
              Wrong data format! Please upload .csv/.sas7bdat/.sas7cdat files!
            </b>"))
          })
          adsl <- NULL
      }
    } else if (input$radiobutton_data == "Demo data") {
      load("data/adsl_data.rda")
      adsl <- adsl_data
      output$wrong_adsl_format_text <- shiny::renderUI({
            HTML(paste0(""))
      })
    }
    adsl
  })


    merged_adae_adsl_data <- reactive({
      adae_data_reac()
      adsl_data_reac()
      if (!is.null(adae_data_reac())) {
        adae <- adae_data_reac()
        if (!is.null(adsl_data_reac())) {
          if (!is.null(input$sel_subjidn)){
            adsl <- adsl_data_reac()
            adae <- join_adae_and_adsl(dat_adae = adae, dat_adsl = adsl, SUBJIDN = shiny::req(input$sel_subjidn))
          }
        }
        adae
      }
    })



  #### load data and prepare for graphics ####
  prepared_merged_data_reac <- shiny::reactive({
    print("prepared__start:")
    #react for file input radiobuttons or demo data
    input$radiobutton_data

    #adae or adae+adsl
    data <- merged_adae_adsl_data()

    print("data:")
    print(data)
    #initiate loading flag when data are available
    if (!is.null(data)) {
      loaded$dat <- 1
    } else  {
      loaded$dat <- 0
    }
    # filter all data sets (when available) for safety flag (requires safety flag selected)
    if (!is.null(shiny::req(input$sel_saffn))) {
      if (!is.null(data)) {
        if(input$sel_saffn %in% colnames(data)) {
          data <- filter_for_safety_flag(data, input$sel_saffn)
        }
      }
    }
    if (!is.null(data)) {
      #calculate number of missing values and imputate missing data
      missing_replaced_data <- calculate_and_impute_required_variables_missing_values(
        data = data,
        SUBJIDN = input$sel_subjidn,
        LVDT = input$sel_lvdt,
        DTHDT = ifelse(input$sel_dthdt == "NA", "DTHDT", input$sel_dthdt),
        TRTSDT = input$sel_trtsdt,
        AEDECOD = input$sel_aedecod,
        AESTDY = input$sel_aestdy,
        AETRTEMN = input$sel_aetrtemn,
        AEENDY = input$sel_aeendy,
        AESEVN = input$sel_aesevn,
        severity_grading_flag = input$severity_grading_flag
      )
      print("missing_replaced_data:")
      print(missing_replaced_data)
      # Create app text for upload page about number missing (and replaced) data
      if (missing_replaced_data$number_days_removed > 0) {
        output$sel_aestdy_check3 <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color: #aed5f5"> <i class="fa-solid fa-exclamation"></i> Note: Adverse event start day was later than end date for ',
              missing_replaced_data$number_days_removed,
              ' subject(s). The end date is/was set to start date in these cases. </span>'
            )
          )
        })
      }
      if (missing_replaced_data$number_missing_lvdt  > 0) {
        output$sel_lvdt_check2 <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color: #aed5f5"> <i class="fa-solid fa-exclamation"> </i> Note: Last visit date is missing for ',
              missing_replaced_data$number_missing_lvdt,
              ' subjects. The last visit date is/was set to maximum treatment day/date in these cases.</span>'
            )
          )
        })
      } else {
        output$sel_lvdt_check2 <- shiny::renderUI({
          shiny::HTML(paste0(''))
        })
      }

      if (missing_replaced_data$number_ae_end_missing > 0) {
        output$sel_aeendy_check2 <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color: #aed5f5"> <i class="fa-solid fa-exclamation"></i> Note: Analysis end date is missing for ',
              missing_replaced_data$number_ae_end_missing,' events. The adverse event end day is/was set to maximum treatment day or death day in these cases. </span>'
            )
          )
        })

      } else {
        output$sel_aeendy_check2 <- shiny::renderUI({
          shiny::HTML(paste0(''))
        })
      }

      if (missing_replaced_data$number_ae_start_missing > 0) {
        output$sel_aestdy_check2 <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color: #aed5f5"> <i class="fa-solid fa-exclamation"></i> Note: Analysis start day is missing for ',
              missing_replaced_data$number_ae_start_missing,
              ' events. The adverse event start is/was set to 1 in these cases. </span>'
            )
          )
        })
      } else {
        output$sel_aestdy_check2 <- shiny::renderUI({
          shiny::HTML(paste0(''))
        })
      }

      if (missing_replaced_data$number_severe_missing > 0) {
        output$sel_aesevn_check2 <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color: #aed5f5"> <i class="fa-solid fa-exclamation"></i> Note: Variable is not as expected or missing for ',
              missing_replaced_data$number_severe_missing,
              ' entries and was set to grade SEVERE. </span>'
            )
          )
        })
      } else {
        output$sel_aesevn_check2 <- shiny::renderUI({
          shiny::HTML(paste0(''))
        })
      }

      if (missing_replaced_data$number_unknown_aes > 0) {
        output$sel_aedecod_check2 <- shiny::renderUI({
          shiny::HTML(
            paste0(
            '<span style = "color: #aed5f5"> <i class="fa-solid fa-exclamation"></i> Note: ', missing_replaced_data$number_unknown_aes,' adverse events
            have an Unknown type and where set to "Unknown type of AE".</span>'
            )
          )
        })
      } else {
        output$sel_aedecod_check2 <- shiny::renderUI({
          shiny::HTML(
            paste0(
            ''
            )
          )
        })
      }
    } else {
      saffn_check_flag$val <- FALSE
      subjidn_check_flag$val <- FALSE
      aedecod_check_flag$val <- FALSE
      trt01a_check_flag$val <- FALSE
      lvdt_check_flag$val <- FALSE
      dthdt_check_flag$val <- FALSE
      trtsdt_check_flag$val<- FALSE
      aestdy_check_flag$val <- FALSE
      aetrtemn_check_flag$val <- FALSE
      aeendy_check_flag$val<- FALSE
      aesevn_check_flag$val<- FALSE
    }

    # all required variables need to be valid:
    if (
      saffn_check_flag$val &&
      subjidn_check_flag$val &&
      aedecod_check_flag$val &&
      trt01a_check_flag$val &&
      lvdt_check_flag$val &&
      dthdt_check_flag$val &&
      trtsdt_check_flag$val &&
      aestdy_check_flag$val &&
      aetrtemn_check_flag$val &&
      aeendy_check_flag$val &&
      aesevn_check_flag$val
    ) {

      prepared_data_result_object <- prepare_data_for_adepro(
        dat = missing_replaced_data$data,
        SUBJIDN = input$sel_subjidn,
        TRT01A = input$sel_trt01a,
        SAFFN = input$sel_saffn,
        LVDT = input$sel_lvdt,
        DTHDT = ifelse(input$sel_dthdt == "NA","DTHDT",input$sel_dthdt),
        TRTSDT = input$sel_trtsdt,
        AEDECOD = input$sel_aedecod,
        AESTDY = input$sel_aestdy,
        AETRTEMN = input$sel_aetrtemn,
        AEENDY = input$sel_aeendy,
        AESEVN = input$sel_aesevn,
        AESERN = input$sel_aesern,
        AERELN = input$sel_aereln,
        AERELPRN = input$sel_aerelprn,
        AEACNN = input$sel_aeacnn
      )

      print("prepared_data_result_object")
      print(prepared_data_result_object)
      # Fri May 16 13:44:23 2025 ------------------------------
      #to be done


      if (dim(prepared_data_result_object$ae_data)[1] == 0) {
        return(NULL)
      }

      shinyjs::enable("submit")

      prepared_data_result_object
    } else {
      shinyjs::disable("submit")
    }
  })

  prepared_merged_data_reac2 <- shiny::reactive({
    tot_dat <- prepared_merged_data_reac()

    if (!is.null(tot_dat)) {
      if (all(input$sortTreatments %in% levels(tot_dat$pat_data[["treat"]]))) {
        if(!all(is.null(input$sortTreatments))) {
          sortTreatments <- input$sortTreatments
           tot_dat$pat_data <- tot_dat$pat_data %>%
             dplyr::filter(treat %in% sortTreatments)
          tot_dat$ae_data <- tot_dat$ae_data %>%
            dplyr::filter(patient %in% tot_dat$pat_data$ps)

          tot_dat$pat_data$treat <- forcats::fct_relevel(tot_dat$pat_data$treat, sortTreatments)
        }
      }
    }
    tot_dat
  })

  ae_data <- shiny::reactive({
    shiny::req(prepared_merged_data_reac2())
    prepared_merged_data_reac2()
    tmp <- prepared_merged_data_reac2()$ae_data
    tmp
  })

  patient_data <- shiny::reactive({
    shiny::req(prepared_merged_data_reac2())
    tmp <- prepared_merged_data_reac2()$pat_data
    tmp
    ae_data <- ae_data()
    # add column in patient_data: ae_frequency - AE frequency
    tmp$ae_frequency <- numeric(nrow(tmp))
    for (i in 1:nrow(tmp)) {
      indices <- which(ae_data$patient == tmp$ps[i])
      tmp$ae_frequency[i] <- sum(ae_data$day_end[indices] - ae_data$day_start[indices] + rep(1, length(indices)))
    }
    tmp
  })


  has_data <- shiny::reactive({
    return(!(is.null(ae_data()) & is.null(patient_data())))
  })

  data_type <- shiny::reactive({
    shiny::req(input$type)
    shiny::req(prepared_merged_data_reac2())
    ae_data0 <- prepared_merged_data_reac2()$ae_data
    Q <- initQ(ae_data0)

    ae_data <- ae_data0[which(Q[,as.numeric(input$type)]),]
    ae_data
  })

  slice_plot_data <- shiny::reactive({
    shiny::req(prepared_merged_data_reac2())
    shiny::req(ae_data())
    shiny::req(input$type)
    shiny::req(input$severity_grading_flag)
    ae_data <- ae_data()
    Q <- initQ(ae_data)
    ae_data <- preproc_ae(ae_data,grading=ifelse(input$severity_grading_flag=="Severity",FALSE,TRUE))
    ae_data <- ae_data[which(Q[, as.numeric(input$type)]), ]
    ae_data
  })

  selected_slice_plot_data <- shiny::reactive({
    data <- slice_plot_data()
    selected <- slice_plot_data()$ae %in% input_var()
    data <- data[selected, ]
    data
  })

  all_aes <- shiny::reactive({
    shiny::req(prepared_merged_data_reac())
    shiny::req(input$type)
    dat <- prepared_merged_data_reac()$ae_data
    Q <- initQ(dat)
    flag_name <- colnames(Q)[as.numeric(shiny::isolate(input$type))]
    dat <- dat %>%
      dplyr::filter(!!rlang::sym(flag_name) == 1)
    if (input$ae_var_sorting == "frequency") {
      dat <- dat[!duplicated(dat[,c("patient","ae")]),]
    }
    ae_table <- sort(table(rep(dat$ae, dat$day_end - dat$day_start + 1)), decreasing = TRUE)
    ae_table_labels <- paste0(names(ae_table), " (days of ae = ",ae_table, ")")
    ae_table2 <- sort(table(dat$ae), decreasing = TRUE)
    ae_table2_labels <- paste0(names(ae_table2), " (N = ", ae_table2, ")")
    ae_table <- ae_table[ae_table > 0]
    ae_table2 <- ae_table2[ae_table2 > 0]

    aes <- names(ae_table)
    aes2 <- names(ae_table2)

    if(input$ae_var_sorting == "days") {
      ae_table_labels
    } else if (input$ae_var_sorting == "frequency") {
      ae_table2_labels
    } else {
      NULL
    }
  })

  input_var <- shiny::reactive({
    if (!is.null(input$var)){
      if(input$ae_var_sorting == "frequency") {
        unlist(lapply(strsplit(input$var, " \\(N = "), first))
      } else if(input$ae_var_sorting == "days") {
        unlist(lapply(strsplit(input$var, " \\(days of ae = "), first))
      } else {
        NULL
      }
    } else {NULL}
  })

  seq_matrix <- shiny::eventReactive(c(input$AI.AdEPro, input$AI.Update, input$remove_adsl), {
    shiny::req(ae_data())
    shiny::req(patient_data())
    shiny::req(input$varSeq)
    ae_data <- ae_data()
    patient_d <- patient_data()

    tmp <- order_patient(ae_data = ae_data,
                         patients = patient_d,
                         variables = input$varSeq,
                         method_dist = 'euclidean',
                         method_seriate = input$methSeq)
    tmp
  })

  output$subgroup <- shiny::renderUI({

    choices <- names(which(apply(shiny::isolate(patients()),2,function(x){length(unique(x))}) < 20))
    choices <- choices[!choices %in% c("X","Y")]

    shinyWidgets::pickerInput(
      inputId = "subgroup",
      label = "Select subgroup variable",
      choices = c(choices),
      selected = NULL,
      multiple = TRUE,
      options = list(
        `max-options` = 1,
        `selected-text-format` = 'count > 0',
        `live-search`=TRUE,
        `style`='background: btn-primary',
        `none-selected-text`='No subgroup selection!'
      )
    )
  })

  #Count the Maximal Number of (selected) Adverse Events per Treatment per day for the y-Axis of the Barplots
  count_max <- shiny::reactive({
    shiny::req(data_type(), patient_data(), input_var())
    ae <- data_type()
    patient <- patient_data()
    all_aes <- shiny::isolate(input_var())

     if(dim(ae %>%
                 dplyr::filter(ae %in% all_aes))[1] > 0) {
      tst <- ae %>%
        dplyr::filter(ae %in% all_aes) %>%
        dplyr::right_join((patient %>%
                             dplyr::rename(patient = ps)), by = "patient")
      tst <- tst %>%
        dplyr::select(day_start, day_end, treat, ae) %>%
        stats::na.omit()

      tst <- tst%>%
        dplyr::group_by(ae, treat) %>%
        tidyr::nest()
      max_count <- numeric(dim(tst)[1])
      for (i in 1:dim(tst)[1]) {
        max_count[i] <- apply(tst$data[[i]], 1, function(x){x[1]:x[2]}) %>%
          unlist() %>%
          table() %>%
          max()
      }
      count_max <- max(max_count)
    count_max
    }
  })

  counts <- shiny::reactive({
    shiny::req(ae_data(),patient_data(), input$ae_audio, prepared_merged_data_reac2())
    ae_data <- ae_data()
    patient_data <- patient_data()


    dat <- ae_data[which(ae_data$ae == input$ae_audio), ]
    if(dim(dat)[1] > 0) {
      data <- ae_count(dat, patient_data)
    } else {
      data <- NULL
    }
    return(data)
  })

  global_params <- shiny::reactive({
    title <- as.character(unique(patient_data()$treat))
    if(!is.null(input$sortTreatments)) {
    globals <- set_global_params(
      ae_data(),
      patient_data(),
      title = title,
      height = input$numberRows,
      treatment = input$sortTreatments
    )
    return(globals)
    }
  })

  patients <- shiny::reactive({
    prepared_merged_data_reac2()
    global_params <- shiny::req(global_params())
    patient_data  <- shiny::req(patient_data())


    if (!is.null(dim(patient_data)[1])) {
    if (!is.null(input$sorting) && input$sorting != "randomization number" && input$sorting != "SEQUENCING") {

      colindex <- which(colnames(patient_data) == input$sorting)

      trt <- as.numeric(factor(patient_data$treat, levels = unique(patient_data$treat)))

      if (is.null(dim(patient_data[, colindex]))){
      patient_data <- patient_data[order(trt, patient_data[, colindex]), ]
      patient_data <- patient_data %>%
        dplyr::arrange(treat)
      } else {
        NULL
      }
    } else if (!is.null(input$sorting) && input$sorting != "randomization number" && input$sorting == "SEQUENCING") {
      patient_data <- patient_data %>%
        dplyr::right_join(seq_matrix() %>%
                            dplyr::rename(ps = patient), by = 'ps')
      patient_data <- patient_data %>%
        dplyr::filter(!is.na(treat))
      patient_data <- patient_data %>%
        dplyr::arrange(treat, SEQUENCING)
    }


      patient_data  <- preproc_patients(patient_data, global_params$height)
      patient_data
    } else {
      NULL
    }
  })


  #### OBSERVERS ####
  shiny::observeEvent(input$remove_adsl, {
    shinyjs::reset('tot_dat2')
    infile_adsl$val <- NULL
  })

  shiny::observeEvent(input$play, {
    my$started <- TRUE
  })

  shiny::observeEvent(input$pause, {
    my$started <- FALSE
  })

  shiny::observeEvent(input$play, {
    onoff$val <- TRUE
  })

  shiny::observeEvent(input$pause,{
    onoff$val <- FALSE
  })

  shiny::observeEvent(c(input$speed, input$play), {
    if(!is.null(input$speed)){
      adepro_n <<- shiny::isolate(input$speed)
      adepro_timer <<- shiny::reactiveTimer(adepro_n * 1000)
    }
  })

  shiny::observeEvent(input$slider, {
    my$inc <- shiny::isolate(input$slider)
  })

  shiny::observe({
    adepro_timer()
    if(shiny::isolate(my$started))
      my$inc <- shiny::isolate(my$inc) + 1
    if(onoff$val){
      shiny::updateSliderInput(session, inputId = "slider", value = isolate(my$inc))
    }
  })

  shiny::observeEvent(input$backward, {
    if (shiny::isolate(my$inc) - as.numeric(input$step_size) >= 1) {
      my$inc <- shiny::isolate(my$inc) - as.numeric(input$step_size)
    } else {
      my$inc <- 1
    }
    shiny::updateSliderInput(session, inputId = "slider", value = isolate(my$inc))
  })


  shiny::observeEvent(input$forward,{
    ae_data <- shiny::isolate(ae_data())

    day_max <- ifelse(
      length(ae_data$day_end) == 0,
      1,
      max(ae_data$day_end, na.rm = TRUE)
    )
    if (shiny::isolate(my$inc) + as.numeric(input$step_size) > day_max) {
       my$inc <- day_max
    } else {
       my$inc <- shiny::isolate(my$inc) + as.numeric(input$step_size)
    }

    shiny::updateSliderInput(session, inputId = "slider", value = isolate(my$inc))
  })

  shiny::observeEvent(input$ae_dat, {
    ae_input$val <- input$ae_dat
  })

  shiny::observeEvent(input$patient_dat, {
    patient_input$val <- input$patient_dat
  })


  shiny::observeEvent(input$refresh, {
    patient_input$val <- NULL
    ae_input$val <- NULL
  })

  shiny::observeEvent(loaded$dat == 1, {
    shinyjs::disable("patient_dat")
    shinyjs::disable("ae_dat")
  })

  shiny::observeEvent(input$submit, {
    shinyBS::updateCollapse(
      session,
      id = "collapse",
      open = c(
        shiny::HTML(
          '<p style="color:white; font-size:100%;"> Modify data </p>'
        ),
        shiny::HTML('<p style="color:white; font-size:100%;"> Adverse events for animation </p>'),
        shiny::HTML('<p style="color:white; font-size:100%;"> Subgroup setting </p>')
      )
    )
  })


  shiny::observeEvent(c(merged_adae_adsl_data(),input$sel_trt01a), {
    shiny::req(input$sel_trt01a)
    shiny::req(input$sel_aedecod)
    choices <- sort(unique(
      merged_adae_adsl_data()[!is.na(merged_adae_adsl_data()[input$sel_aedecod]),][[input$sel_trt01a]]
    ))



    shiny::updateSelectizeInput(
      session,
      inputId = "sortTreatments",
      choices = choices,
      selected = choices
    )
  })

  shiny::observeEvent(input$tot_dat, {
    shinyBS::updateCollapse(
      session,
      id = "collapse_adae",
      open = c(
        shiny::HTML('<p style="color:white; font-size:100%;"> Required variables: </p>'),
        shiny::HTML('<p style="color:white; font-size:100%;"> Optional variables: </p>' )
      )
    )
  })

  shiny::observeEvent(input$radiobutton_data, {
    if(input$radiobutton_data =="Demo data"){
      shinyBS::updateCollapse(
        session,
        id = "collapse_adae",
        open = c(
          shiny::HTML('<p style="color:white; font-size:100%;"> Required variables: </p>'),
          shiny::HTML('<p style="color:white; font-size:100%;"> Optional variables: </p>' )
        )
      )
    }
  })

  radio_data_reac <- shiny::reactiveValues(
    val = "File upload"
  )

  output$radio_data <- shiny::reactive({
    radio_data_reac$val
  })

  shiny::observe({
    radio_data_reac$val <-input$radiobutton_data
  })

  shiny::outputOptions(output, "radio_data", suspendWhenHidden = FALSE)

  demo_exists_reac <- shiny::reactiveValues(
    val = FALSE
  )

  output$demo_data_exists <- shiny::reactive({
    demo_exists_reac$val
  })

  shiny::observe({
    demo_exists_reac$val <- file.exists(here::here("data", "adae_data.rdata")) & file.exists(here::here("data", "adsl_data.rdata"))
  })

  shiny::outputOptions(output, "demo_data_exists", suspendWhenHidden = FALSE)

  shiny::observeEvent(demo_exists_reac$val, {
    shiny::updateRadioButtons(
        session,
        inputId ="radiobutton_data",
        choices = c("File upload","Demo data")
    )
  })

  shiny::observeEvent(input$type, {
    shinyBS::updateCollapse(
      session,
      id = "collapse",
      open = shiny::HTML('<p style="color:white; font-size:100%;"> Adverse events for animation </p>')
    )
  })

  shiny::observeEvent(input$rem_row, {
    shiny::updateSliderInput(session, inputId = "numberRows", value = (input$numberRows - 1))
  })

  shiny::observeEvent(input$add_row, {
    shiny::updateSliderInput(session, inputId = "numberRows", value = (input$numberRows + 1))
  })

  shiny::observeEvent(input$minus_zoom, {
    if (heightSlider$val > 400) {
     updateSliderInput(session, inputId = "heightSlider", value = input$heightSlider - 100)
    }
  })

  shiny::observeEvent(input$plus_zoom, {
    if (heightSlider$val < 1600) {
      updateSliderInput(session, inputId = "heightSlider", value = input$heightSlider + 100)
    }
  })

  shiny::observeEvent(heightSlider$val, {
    if (heightSlider$val == 400) {
      shinyjs::disable("minus_zoom")
    } else if (heightSlider$val > 400) {
      shinyjs::enable("minus_zoom")
    }
    if (heightSlider$val == 1600) {
      shinyjs::disable("plus_zoom")
    } else if(heightSlider$val < 1600) {
      shinyjs::enable("plus_zoom")
    }
  })

  shiny::observeEvent(input$heightSlider, {
    heightSlider$val <- input$heightSlider
  })

  shiny::observeEvent(has_data(), {
    if (!(is.null(ae_data())) & !is.null(patient_data())) {
    }
  })

  shiny::observeEvent(input$AI.AdEPro, {
    start$dat <- input$AI.AdEPro %% 2
  })

  shiny::observeEvent(input$AI.Update, {
    var_sort <- colnames(prepared_merged_data_reac()$pat_data)[-c(1:4)]
    var_sort <- c("SEQUENCING", var_sort)
    shiny::updateSelectInput(session, "sorting",choices = var_sort, selected = "SEQUENCING")
  })


  #### SUBJECT ID - SUBJIDN ####
  output$sel_subjidn <- shiny::renderUI({
    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    if (!is.null(adsl)) {
      choices <- intersect(names(adae),names(adsl))
      choices <- names(which(apply(adsl[choices], 2, function(x) {length(unique(x))}) == dim(adsl)[1]))
    } else {
      choices <- names(adae)
    }

    choices <- c(unique(choices))


    if (!any(c("USUBJIDN", "USUBJID","SUBJIDN", "SUBJID") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("USUBJIDN", "USUBJID","SUBJIDN", "SUBJID")[which(c("USUBJIDN", "USUBJID","SUBJIDN", "SUBJID") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_subjidn",
      label = shiny::HTML('<p style = "color:#ffffff"> Subject Identifier:</p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

  subjidn_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_subjidn), {


    shiny::req(merged_adae_adsl_data())
    if (is.null(input$sel_subjidn)) {
      output$sel_subjidn_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times"></i>
            Please select a variable for subject identifier. </span>'
          )
        )
      })
      subjidn_check_flag$val <- FALSE
    } else {
      if (input$sel_subjidn == "Nothing selected") {
         subjidn_check_flag$val <- FALSE
        output$sel_subjidn_check <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa-solid fa-times">
              </i>
              Variables SUBJIDN, SUBJID, USUBJIDN, USUBJID are not available.
              Please select another variable, upload adsl data or add one of
              these variables to your data set.
              </span>'
            )
          )
        })
        subjidn_check_flag$val <- FALSE
      } else {
        subjidn <- merged_adae_adsl_data()[[input$sel_subjidn]]
        if (!input$sel_subjidn %in% c("SUBJIDN", "SUBJID", "USUBJID", "USUBJIDN")) {
          subjidn_check_flag$val <- TRUE
          output$sel_subjidn_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question">
                </i>
                A variable different to SUBJIDN, SUBJID, USUBJIDN, USUBJID is selected.
                </span>'
              )
            )
          })
        } else if (input$sel_subjidn %in% c("SUBJIDN", "SUBJID", "USUBJID", "USUBJIDN")) {
          subjidn_check_flag$val <- TRUE
          output$sel_subjidn_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


  #### ADVERSE EVENT CODE - AEDECOD ####
   output$sel_aedecod <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())

    adae <- shiny::req(adae_data_reac())

    choices <- names(adae[unlist(lapply(adae, is.character), use.names = FALSE)])

    choices <- c(unique(choices))

    if (!"AEDECOD" %in% choices) {
      selected <- NULL
    } else {
      selected <- "AEDECOD"
    }

    shinyWidgets::pickerInput(
      inputId = "sel_aedecod",
      label = shiny::HTML('<p style = "color:#ffffff"> Dictionary Derived Term: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

 aedecod_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aedecod), {
    shiny::req(merged_adae_adsl_data())

    if (is.null(input$sel_aedecod)) {
      output$sel_aedecod_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for adverse event term.
            </span>'
          )
        )
      })
      aedecod_check_flag$val <- FALSE
    } else {
      if (input$sel_aedecod == "Nothing selected") {
       aedecod_check_flag$val <- FALSE
        output$sel_aedecod_check <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa-solid fa-times">
              </i>
               Variable AEDECOD is not available.
              Please select another variable, upload adsl data or add the variable to your data set.
              </span>'
            )
          )
        })
      } else {
        aedecod <- merged_adae_adsl_data()[[input$sel_aedecod]]
        if (!is.character(aedecod)) {
          aedecod_check_flag$val <- FALSE
          output$sel_aedecod_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<p style = "color:#E43157"> <i class="fa-solid fa-times"> </i> AEDECOD needs to be character </p>'
              )
            )
          })
        } else if (is.character(aedecod) & input$sel_aedecod != "AEDECOD") {
          aedecod_check_flag$val <- TRUE
          output$sel_aedecod_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"> </i>
                Variable AEDECOD is not available or selected.
                </span>'
              )
            )
          })
        } else if (is.character(aedecod) & input$sel_aedecod == "AEDECOD") {
          aedecod_check_flag$val <- TRUE
          output$sel_aedecod_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  #### TREATMENT - TRT01A ####
  output$sel_trt01a <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())
    if(is.null(adsl_data_reac())) {
      choices <- colnames(merged_adae_adsl_data())
    } else {
      choices <- colnames(adsl_data_reac())
    }

    choices <- c(unique(choices))
    # choices <- c(unique(choices),"Nothing selected")

    if (!any(c("TRT01A", "TRT01AN","TRT01P","TRT01PN") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("TRT01A","TRT01AN","TRT01P","TRT01PN")[which(c("TRT01A","TRT01AN","TRT01P","TRT01PN") %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = "sel_trt01a",
      label = shiny::HTML('<p style = "color:#ffffff"> Actual treatment: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

  trt01a_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_trt01a), {
    shiny::req(merged_adae_adsl_data())

    if (is.null(input$sel_trt01a)) {
      output$sel_trt01a_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for actual treatment.
            </span>'
          )
        )
      })
      trt01a_check_flag$val <- FALSE
    } else {

      # if (input$sel_trt01a == "Nothing selected") {
      #   trt01a_check_flag$val <- FALSE
      #   output$sel_trt01a_check <- shiny::renderUI({
      #     shiny::HTML(
      #       paste0(
      #         '<span style = "color:#E43157"> <i class="fa-solid fa-times"></i>
      #         Variables TRT01A, TRT01AN, TRT01P, TRT01PN are not available.
      #         Please select another variable, upload adsl data or add one of these variables to your data set.
      #         </span>'))
      #   })
      # } else {
        trt01a <- merged_adae_adsl_data()[[input$sel_trt01a]]

        if (!any(input$sel_trt01a %in% c("TRT01A", "TRT01AN", "TRT01P", "TRT01PN"))) {
          trt01a_check_flag$val <- TRUE
          output$sel_trt01a_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff">
                <i class="fa-solid fa-question">
                </i>
                Variable different to TRT01A, TRT01AN, TRT01P, TRT01PN is selected.
                </span>'
              )
            )
          })
        } else {
          trt01a_check_flag$val <- TRUE
          output$sel_trt01a_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f">
                <i class="fa-solid fa-check">
                </i>
                </span>'
              )
            )
          })
        }
      # }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  #### TREATMENT START DATE - TRTSDT ####
  output$sel_trtsdt <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())

    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))
    is.convertible.to.integer <- function(x) {suppressWarnings(x == as.integer(x))}


    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.date(x)}),2,any)))))
    choices_int <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.integer(x)}),2,any)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.date(x)}),2,any)))
      choices2_int <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.integer(x)}),2,any)))
      choices <- sort(c(choices, choices2))
      choices_int <- sort(c(choices_int, choices2_int))
    }
    choices <- sort(c(choices,choices_int))
    choices <- c(unique(choices))
    # choices <- c(unique(choices))

    if(!"TRTSDT" %in% choices) {
      selected <- NULL
    } else if ("TRTSDT" %in% choices) {
      selected <- "TRTSDT"
    }

    shinyWidgets::pickerInput(
      inputId = "sel_trtsdt",
      label = shiny::HTML('<p style = "color:#ffffff"> Date of First Exposure to Treatment: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

  trtsdt_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_trtsdt), {
    shiny::req(merged_adae_adsl_data())
    if (is.null(input$sel_trtsdt)) {
      output$sel_trtsdt_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for treatment start date.
            </span>'
          )
        )
      })
      trtsdt_check_flag$val <- FALSE
    } else {
      is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))
      is.convertible.to.integer <- function(x) {suppressWarnings(x == as.integer(x))}
      # if (input$sel_trtsdt == "Nothing selected") {
      #    trtsdt_check_flag$val <- FALSE
      #   output$sel_trtsdt_check <- renderUI({
      #    shiny::HTML(
      #     paste0('<span style = "color:#E43157"> <i class="fa-solid fa-times">
      #                       </i>
      #                       Variable TRTSDT is not available. Please select another variable,
      #                       upload adsl data or add the variable to your data set.
      #                       </span>'
      #               ))
      #   })
      # } else {
        trtsdt <- merged_adae_adsl_data()[[input$sel_trtsdt]]
      ##back ##

        if (!any(sapply(trtsdt, is.convertible.to.date)) & !any(sapply(trtsdt, is.convertible.to.integer))) {
         trtsdt_check_flag$val <- FALSE
         output$sel_trtsdt_check <- shiny::renderUI({
           shiny::HTML(
             paste0(
               '<p style = "color:#E43157"> <i class="fa-solid fa-times"></i> Treatment start date variable needs to be in date or integer format. </p>'
              )
            )
          })
        } else if ((any(sapply(trtsdt, is.convertible.to.date))| any(sapply(trtsdt, is.convertible.to.integer))) & input$sel_trtsdt != "TRTSDT") {
          trtsdt_check_flag$val <- TRUE
          output$sel_trtsdt_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"></i>
                 Variable TRTSDT is not available or selected. </span>'
              )
            )
          })
        } else if ((any(sapply(trtsdt, is.convertible.to.date)) | any(sapply(trtsdt, is.convertible.to.integer))) & input$sel_trtsdt == "TRTSDT") {
          trtsdt_check_flag$val <- TRUE
          output$sel_trtsdt_check <- renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
    # }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  #### LAST VISIT DATE - LVDT ####
  output$sel_lvdt <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())

    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))

    is.convertible.to.integer <- function(x) {suppressWarnings(x == as.integer(x))}


    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.date(x)}),2,any)))))
    choices_int <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.integer(x)}),2,any)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.date(x)}),2,any)))
      choices2_int <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.integer(x)}),2,any)))
      choices <- sort(c(choices, choices2))
      choices_int <- sort(c(choices_int, choices2_int))
    }
    choices <- sort(c(choices,choices_int))
    choices <- c(unique(choices))
    # choices <- c(unique(choices))

    # choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.date(x)}),2,any)))))
    #
    # if(!is.null(adsl)) {
    #   choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.date(x)}),2,any)))
    #   choices <- sort(c(choices, choices2))
    # }

    # choices <- c(unique(choices))

    if (!"LVDT" %in% choices) {
      selected <- NULL
    } else if ("LVDT" %in% choices) {
      selected <- "LVDT"
    }

    shinyWidgets::pickerInput(
      inputId = "sel_lvdt",
      label = shiny::HTML('<p style = "color:#ffffff"> Date of Last Visit: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1,`live-search` = TRUE)
    )
  })

  lvdt_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_lvdt), {
    shiny::req(merged_adae_adsl_data())

    if (is.null(input$sel_lvdt)) {
      output$sel_lvdt_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for last visit date.
            </span>'
          )
        )
      })
      lvdt_check_flag$val <- FALSE
    } else {
      is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))
       is.convertible.to.integer <- function(x) {suppressWarnings(x == as.integer(x))}
      if (input$sel_lvdt == "Nothing selected") {
        lvdt_check_flag$val <- FALSE
        output$sel_lvdt_check <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa-solid fa-times">
              </i>
              Variable LVDT is not available. Please select another variable, upload adsl data or add the variable to your data set.
              </span>'
            )
          )
        })
      } else {
        lvdt <- merged_adae_adsl_data()[[input$sel_lvdt]]
        if (!any(sapply(lvdt, is.convertible.to.date)) & !any(sapply(lvdt, is.convertible.to.integer))) {
          lvdt_check_flag$val <- FALSE
          output$sel_lvdt_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<p style = "color:#E43157"> <i class="fa-solid fa-times"></i> Last visit date variable needs to be in date format. </p>'
              )
            )
          })
        } else if ((any(sapply(lvdt, is.convertible.to.date))| any(sapply(lvdt, is.convertible.to.integer)))  & input$sel_lvdt != "LVDT") {
          lvdt_check_flag$val <- TRUE
          output$sel_lvdt_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"></i>  Variable LVDT is not available or selected.</span>'
              )
            )
          })
        } else if ((any(sapply(lvdt, is.convertible.to.date))| any(sapply(lvdt, is.convertible.to.integer)))  & input$sel_lvdt == "LVDT") {
          lvdt_check_flag$val <- TRUE
          output$sel_lvdt_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


 #### ADVERSE EVENT EMERGENT TREATMENT FLAG - AETRTEMN ####
  output$sel_aetrtemn <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())

    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.flag <- function(x) as.character(x) %in% c("Y","y","Yes","yes","YES","1","0","N","n","No","NO","",".")

    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.flag(x)}),2,all)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.flag(x)}),2,all)))
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))

    if (!any(c("AETRTEMN", "AETRTEM", "TRTEMFLN", "TRTEMFL") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("AETRTEMN","AETRTEM","TRTEMFLN","TRTEMFL")[which(c("AETRTEMN","AETRTEM","TRTEMFLN","TRTEMFL") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_aetrtemn",
      label = shiny::HTML('<p style = "color:#ffffff"> Treatment Emergent Analysis Flag: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

  aetrtemn_check_flag <- shiny::reactiveValues(val = FALSE)

 shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aetrtemn), {
   shiny::req(merged_adae_adsl_data())

    if (is.null(input$sel_aetrtemn)) {
      output$sel_aetrtemn_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for treatment emergency flag.
            </span>'
          )
        )
      })
      aetrtemn_check_flag$val <- FALSE
    } else {

      if (input$sel_aetrtemn == "Nothing selected") {
         aetrtemn_check_flag$val <- FALSE
        output$sel_aetrtemn_check <- shiny::renderUI({
          shiny::HTML(
            paste0('<span style = "color:#E43157"> <i class="fa-solid fa-times"></i>
                    Variables AETRTEMN, AETRTEM, TRTEMFLN, TRTEMFL are not available.
                    Please select another variable, upload adsl data or add one of these variables to your data set.
                   </span>'
            )
          )
        })
      } else {
        aetrtemn <- merged_adae_adsl_data()[[input$sel_aetrtemn]]

       if (!input$sel_aetrtemn %in% c("AETRTEMN", "AETRTEM", "TRTEMFLN", "TRTEMFL")) {
         aetrtemn_check_flag$val <- TRUE
          output$sel_aetrtemn_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question">
                </i>
                Variable different to AETRTEMN, AETRTEM, TRTEMFLN, TRTEMFL is selected.
                </span>'
              )
            )
          })
        } else {
          aetrtemn_check_flag$val <- TRUE
          output$sel_aetrtemn_check <- renderUI({
            shiny::HTML(
              paste0(
               '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
    }
  },ignoreNULL = FALSE, ignoreInit = TRUE)

   #### DEATH DATE - DTHDT ####
  output$sel_dthdt <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())

    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))
    is.convertible.to.integer <- function(x) {suppressWarnings(x == as.integer(x))}


    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.date(x)}),2,any)))))
    choices_int <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.integer(x)}),2,any)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.date(x)}),2,any)))
      choices2_int <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.integer(x)}),2,any)))
      choices <- sort(c(choices, choices2))
      choices_int <- sort(c(choices_int, choices2_int))
    }
    choices <- sort(c(choices,choices_int))

    choices <- c(unique(choices),"NA")
    #choices <- c(unique(choices),"NA", "Nothing selected")

    if(!"DTHDT" %in% choices) {
      selected <- NULL
    } else if ("DTHDT" %in% choices) {
      selected <- "DTHDT"
    }

    shinyWidgets::pickerInput(
      inputId = "sel_dthdt",
      label = shiny::HTML('<p style = "color:#ffffff"> Date of Death: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        maxOptions = 1,`live-search` = TRUE#, actionsBox = TRUE
      )
    )
  })

  dthdt_check_flag <- reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_dthdt), {
    shiny::req(merged_adae_adsl_data())
    if (is.null(input$sel_dthdt)) {
      output$sel_dthdt_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for death date.
            </span>'
          )
        )
      })
      dthdt_check_flag$val <- FALSE
    } else {
      is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))
      is.convertible.to.integer <- function(x) {suppressWarnings(x == as.integer(x))}
      ## back##
      if (input$sel_dthdt == "Nothing selected") {
          dthdt_check_flag$val <- FALSE
        output$sel_dthdt_check <- renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa-solid fa-times"></i>
               Variable DTHDT is not available.
              Please select another variable, upload adsl data or add the variable to your data set.
              </span>'
            )
          )
        })
      } else if (input$sel_dthdt == "NA") {
        output$sel_dthdt_check <- shiny::renderUI({
          shiny::HTML(paste0('<span style = "color:#ffffff"> <i class="fa-solid fa-exclamation"></i>
                              Variable DTHDT will not be used. </span>'))
        })
        dthdt_check_flag$val <- TRUE
      } else {
        dthdt <- merged_adae_adsl_data()[[input$sel_dthdt]]

        if (!any(sapply(dthdt, is.convertible.to.date)) & !all(is.na(dthdt)) & !any(sapply(dthdt, is.convertible.to.integer))) {
          dthdt_check_flag$val <- FALSE
          output$sel_dthdt_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<p style = "color:#E43157"> <i class="fa-solid fa-times"></i> Death date variable need to be in date format or NA. </p>'
              )
            )
          })
        } else if (((any(sapply(dthdt, is.convertible.to.date)) | any(sapply(dthdt, is.convertible.to.integer))) | all(is.na(dthdt))) & input$sel_dthdt != "DTHDT") {
          dthdt_check_flag$val <- TRUE
          output$sel_dthdt_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"></i>Variable DTHDT is not available or selected. </span>'
              )
            )
          })
        } else if (((any(sapply(dthdt, is.convertible.to.date)) | any(sapply(dthdt, is.convertible.to.integer))) | all(is.na(dthdt))) & input$sel_dthdt == "DTHDT") {
          dthdt_check_flag$val <- TRUE
          output$sel_dthdt_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
   }
 }, ignoreInit = TRUE, ignoreNULL = FALSE)

  #### SAFETY VARIABLE SAFFN ####
  output$sel_saffn <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())

    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.flag <- function(x) as.character(x) %in% c("Y","y","Yes","yes","YES","1","0","N","n","No","NO","",".")

    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.flag(x)}),2,all)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.flag(x)}),2,all)))
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))
    # choices <- c(unique(choices),"Nothing selected")

    # choices <- colnames(adae_data_reac2())

    if (!any(c("SAFFN","SAFFL") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("SAFFN","SAFFL")[which(c("SAFFN","SAFFL") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_saffn",
      label = shiny::HTML('<p style = "color:#ffffff"> Safety Population Flag: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

  saffn_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_saffn), {
    shiny::req(merged_adae_adsl_data())

    if (is.null(input$sel_saffn)) {
      output$sel_saffn_check <- renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>Please select a variable for safety flag. </span>'
          )
        )
      })
      saffn_check_flag$val <- FALSE
    } else {
      if (input$sel_saffn== "Nothing selected") {
         saffn_check_flag$val <- FALSE
        output$sel_saffn_check <- renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa-solid fa-times">
              </i>
              Variables SAFFN or SAFFL are not available.
              Please select another variable, upload adsl data or add one of these variables to your data set.
              </span>'
            )
          )
        })
      } else {
        saffn <- merged_adae_adsl_data()[[input$sel_saffn]]

        if (!any(input$sel_saffn %in% c("SAFFN", "SAFFL"))) {
          saffn_check_flag$val <- TRUE
          output$sel_saffn_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"> </i> Variable different to SAFFN, SAFFL is selected. </span>'
              )
            )
          })
        } else {
          saffn_check_flag$val <- TRUE
          output$sel_saffn_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
    }
  },ignoreInit = TRUE, ignoreNULL = FALSE)

  #### AE START DAY - AESTDY ####
  output$sel_aestdy <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())


    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    choices <- names(adae[unlist(lapply(adae, is.numeric), use.names = FALSE)])
    if(!is.null(adsl)) {
      choices2 <- names(adsl[unlist(lapply(adsl, is.numeric), use.names = FALSE)])
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))

    # choices <- colnames(merged_adae_adsl_data())

    if (!any(c("AESTDY","ASTDY") %in% choices)) {
      #choices <- c("Nothing selected", choices)
      selected <- NULL
    } else {
      selected <- c("AESTDY","ASTDY")[which(c("AESTDY","ASTDY") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_aestdy",
      label = shiny::HTML('<p style = "color:#ffffff"> Adverse Event Start Day: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, minOptions = 1, `live-search` = TRUE)
    )
  })

  aestdy_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aestdy), {
    shiny::req(merged_adae_adsl_data())
    if (is.null(input$sel_aestdy)) {
      output$sel_aestdy_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i> Please select a variable for adverse event start day.</span>'
          )
        )
      })
      aestdy_check_flag$val <- FALSE
    } else {

      if (input$sel_aestdy == "Nothing selected") {
             aestdy_check_flag$val <- FALSE
        output$sel_aestdy_check <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa-solid fa-times"> </i> Variables AESTDY or ASTDY are not available.
              Please select another variable, upload adsl data or add one of these variables to your data set. </span>'
            )
          )
        })
      } else {
        aestdy <- merged_adae_adsl_data()[[input$sel_aestdy]]

        if (!is.numeric(aestdy)) {
          aestdy_check_flag$val <- FALSE
          output$sel_aestdy_check <- shiny::renderUI({
            shiny::HTML(paste0('<p style = "color:#E43157"> <i class="fa-solid fa-times"> </i> Adverse event start date variable needs to be numeric </p>'))
          })
        } else if (is.numeric(aestdy) & !input$sel_aestdy %in% c("AESTDY","ASTDY")) {
          aestdy_check_flag$val <- TRUE
          output$sel_aestdy_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"> </i> Variable different to AESTDY, ASTDY is selected. </span>'
              )
            )
          })
        } else if (is.numeric(aestdy) & input$sel_aestdy %in% c("AESTDY","ASTDY")) {
          aestdy_check_flag$val <- TRUE
          output$sel_aestdy_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"> </i></span>'
              )
            )
          })
        }
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


  #### ADVERSE EVENT END DAY - AEENDY ####

  output$sel_aeendy <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())
    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    # is.convertible.to.num <- function(x) !is.na(as.numeric(as.character(x)))

    #choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.num(x)}),2,any)))))
    choices <- names(adae[unlist(lapply(adae, is.numeric), use.names = FALSE)])
    if(!is.null(adsl)) {
      choices2 <- names(adsl[unlist(lapply(adsl, is.numeric), use.names = FALSE)])
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))

    if (!any(c("AEENDY","AENDY") %in% choices)) {
      #choices <- c("Nothing selected", choices)
      selected <- NULL
    } else {
      selected <- c("AEENDY","AENDY")[which(c("AEENDY","AENDY") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_aeendy",
      label = shiny::HTML('<p style = "color:#ffffff"> Adverse Event End Day: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, minOptions = 1, `live-search` = TRUE)
    )
  })

  aeendy_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aeendy), {
    shiny::req(merged_adae_adsl_data())
    if (is.null(input$sel_aeendy)) {
      output$sel_aeendy_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i> Please select a variable for adverse event end day. </span>'
          )
        )
      })
      aeendy_check_flag$val <- FALSE
    } else {
      if (input$sel_aeendy == "Nothing selected") {
          aeendy_check_flag$val <- FALSE
        output$sel_aeendy_check <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa-solid fa-times"> </i> Variables AEENDY or AENDY are not available.
              Please select another variable, upload adsl data or add one of these variable to your data set. </span>'
            )
          )
        })
      } else {
        aeendy <- merged_adae_adsl_data()[[input$sel_aeendy]]

        if (!is.numeric(aeendy)) {
          aeendy_check_flag$val <- FALSE
          output$sel_aeendy_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<p style = "color:#E43157"> <i class="fa-solid fa-times"> </i> Adverse event end date variable needs to be numeric </p>'
              )
            )
          })
        } else if (is.numeric(aeendy) & !input$sel_aeendy %in% c("AEENDY", "AENDY")) {
          aeendy_check_flag$val <- TRUE
          output$sel_aeendy_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"> </i>
                Variables AEENDY or AENDY are not available or selected. </span>'
              )
            )
          })
        } else if (is.numeric(aeendy) & input$sel_aeendy %in% c("AEENDY", "AENDY")) {
          aeendy_check_flag$val <- TRUE
          output$sel_aeendy_check <- shiny::renderUI({
            shiny::HTML(paste0('<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'))
          })
        }
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

   #### ADVERSE EVENT SEVERITY FLAG - AESEVN ####
  output$sel_aesevn <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())

    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

      is.convertible.to.sev <- function(x) {
        as.character(x) %in% c(" 1", " 2", " 3","1","2","3",
                               "MILD","MODERATE","SEVERE","mild","moderate","severe","Mild","Moderate","Severe",
                               "",".",NA)
        }
      is.convertible.to.grad <- function(x) {
        as.character(x) %in% c(" 1", " 2", " 3", " 4", " 5","1","2","3","4","5",
                               "MILD","MODERATE","SEVERE","LIFE-THREATENING","DEATH",
                               "mild","moderate","severe","life-threatening","death",
                               "Mild","Moderate","Severe","Life-threatening","Life-Threatening","Death",
                               "",".",NA)
        }

    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.sev(x) | is.convertible.to.grad(x)}),2,all)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.sev(x) | is.convertible.to.grad(x)}),2,all)))
      choices <- sort(c(choices, choices2))
    }
    choices <- c(unique(choices))
    if (!any(c("AESEVN", "AESEV", "ASEVN", "AESEV") %in% choices)) {
      #choices <- c("Nothing selected", choices)
      selected <- NULL
    } else {
      selected <- c("AESEVN", "AESEV", "ASEVN", "AESEV")[which(c("AESEVN", "AESEV", "ASEVN", "AESEV") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_aesevn",
      label = shiny::HTML('<p style = "color:#ffffff"> Severity/Intensity/Grading: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

 aesevn_check_flag <- reactiveValues(val = FALSE)

 shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aesevn), {
   shiny::req(merged_adae_adsl_data())

   if (is.null(input$sel_aesevn)) {
      output$sel_aesevn_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times"></i>
            Please select a variable for adverse event severity/grading flag. </span>'
          )
        )
      })
      aesevn_check_flag$val <- FALSE
    } else {
      if (input$sel_aesevn == "Nothing selected") {
          aesevn_check_flag$val <- FALSE
        output$sel_aesevn_check <- renderUI({
         shiny::HTML(
           paste0(
             '<span style = "color:#E43157"> <i class="fa-solid fa-times"> </i>
             Variables AESEVN or AESEV are not available.
             Please select another variable, upload adsl data or add one of these variable to your data set. </span>'
            )
          )
        })
      } else {
        aesevn <- merged_adae_adsl_data()[[input$sel_aesevn]]

        if (!input$sel_aesevn %in% c("AESEVN", "AESEV", "AESEV", "ASEVN") & (is.numeric(aesevn) | all(aesevn %in% c("MILD", "MODERATE", "SEVERE", NA, "NA", "")))) {
          aesevn_check_flag$val <- TRUE
          output$sel_aesevn_check <- shiny::renderUI({
            shiny::HTML(paste0('<span style = "color: #ffffff"> <i class="fa-solid fa-question"></i>  Variable AESEVN is not available or selected. </span>'))
          })
        } else if (is.numeric(aesevn) & input$sel_aesevn %in% c("AESEVN","ASEVN")) {
          aesevn_check_flag$val <- TRUE
          output$sel_aesevn_check <- shiny::renderUI({
            shiny::HTML(paste0('<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'))
          })
        } else if(!is.numeric(aesevn) & all(aesevn %in% c("MILD","MODERATE","SEVERE", NA, "NA", ""))) {
          aesevn_check_flag$val <- TRUE
          output$sel_aesevn_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        } else if (!is.numeric(aesevn) & !all(aesevn %in% c("MILD", "MODERATE", "SEVERE", NA, "NA", ""))) {
          aesevn_check_flag$val <- FALSE
          output$sel_aesevn_check <- shiny::renderUI({
            shiny::HTML(paste0('<p style = "color:#E43157"> <i class="fa-solid fa-times"> </i>
                               AESEVN needs to be numeric or character with entries MILD, MODERATE,SEVERE.</p>'))
          })
        }
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


  #### ADVERSE EVENT SERIOUS FLAG - AESERN ####
  output$sel_aesern <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())
    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.flag <- function(x) as.character(x) %in% c("Y","y","Yes","yes","YES","1","0","N","n","No","NO","",".")

    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.flag(x)}),2,all)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.flag(x)}),2,all)))
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))

     if (!any(c("AESERN", "AESER") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("AESERN", "AESER")[which(c("AESERN", "AESER") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_aesern",
      label = shiny::HTML('<p style = "color:#ffffff"> Serious Event Flag: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

 aesern_check_flag <- shiny::reactiveValues(val = FALSE)

 shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aesern), {
   shiny::req(merged_adae_adsl_data())

   if (is.null(input$sel_aesern)) {
      output$sel_aesern_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i> Please select a variable for adverse event seriousness flag. </span>'
          )
        )
      })
      aesern_check_flag$val <- FALSE
    } else {
      if (input$sel_aesern == "Nothing selected") {
        aesern_check_flag$val <- FALSE
        output$sel_aesern_check <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#ffffff"> <i class="fa-solid fa-times"> </i>
              Optional variable AESERN is not available.
              Please select another variable to use the functionality. </span>'
            )
          )
        })
      } else {
        aesern <- merged_adae_adsl_data()[[input$sel_aesern]]

        if (
          !input$sel_aesern %in% c("AESERN", "AESER")) {
          aesern_check_flag$val <- TRUE
          output$sel_aesern_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #ffffff"> <i class="fa-solid fa-question"></i>
                 Variable AESERN is not available or selected. </span>'
              )
            )
          })
        } else if(
          input$sel_aesern %in% c("AESERN", "AESER")){
          aesern_check_flag$val <- TRUE
          output$sel_aesern_check <- shiny::renderUI({
            shiny::HTML(
              paste0(
                '<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'
              )
            )
          })
        }
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

#### AERELN ####

 output$sel_aereln <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())
   adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.flag <- function(x) as.character(x) %in% c("Y","y","Yes","yes","YES","1","0","N","n","No","NO","",".")

    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.flag(x)}),2,all)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.flag(x)}),2,all)))
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))

     if (!any(c("AERELN", "AEREL") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("AERELN", "AEREL")[which(c("AERELN", "AEREL") %in% choices)[1]]
    }


    shinyWidgets::pickerInput(
      inputId = "sel_aereln",
      label = shiny::HTML('<p style = "color:#ffffff"> Causality Flag: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

  aereln_check_flag <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aereln), {
    shiny::req(merged_adae_adsl_data())

    if (is.null(input$sel_aereln)) {
      output$sel_aereln_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for causality flag. </span>'
          )
        )
      })
      aereln_check_flag$val <- FALSE
    } else {
      if (input$sel_aereln == "Nothing selected") {
           aereln_check_flag$val <- FALSE
      output$sel_aereln_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#ffffff"> <i class="fa-solid fa-times"> </i>
            Optional variable AERELN/AEREL is not available.
            Please select another variable to use the functionality.
            </span>'
          )
        )
      })
   } else {
     aereln <- merged_adae_adsl_data()[[input$sel_aereln]]
       if (
         !input$sel_aereln %in% c("AERELN", "AEREL")) {
         aereln_check_flag$val <- TRUE
          output$sel_aereln_check <- renderUI({
         shiny::HTML(paste0('<span style = "color: #ffffff"> <i class="fa-solid fa-question"></i>
                             Variable AERELN/AEREL is not available or selected. </span>'))
        })
     } else if (
       input$sel_aereln %in% c("AERELN", "AEREL")
      ) {
       aereln_check_flag$val <- TRUE
        output$sel_aereln_check <- renderUI({
         shiny::HTML(paste0('<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'))
        })
     }
   }
    }
 }, ignoreNULL = FALSE, ignoreInit = TRUE)
#### AERELPRN ####
output$sel_aerelprn <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())
     adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.flag <- function(x) as.character(x) %in% c("Y","y","Yes","yes","YES","1","0","N","n","No","NO","",".")

    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.flag(x)}),2,all)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.flag(x)}),2,all)))
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))

     if (!any(c("AERELPRN", "AERELPR") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("AERELPRN", "AERELPR")[which(c("AERELPRN", "AERELPR") %in% choices)[1]]
    }

    shinyWidgets::pickerInput(
      inputId = "sel_aerelprn",
      label = shiny::HTML('<p style = "color:#ffffff"> Causality to Protocol Procedure: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

 aerelprn_check_flag <- reactiveValues(val = FALSE)

  shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aerelprn), {
    shiny::req(merged_adae_adsl_data())
    if (is.null(input$sel_aerelprn)) {
      output$sel_aerelprn_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times"></i>
            Please select a variable for causality to protocol procedure flag. </span>'
          )
        )
      })
      aerelprn_check_flag$val <- FALSE
    } else {
   if(input$sel_aerelprn == "Nothing selected") {
     aerelprn_check_flag$val <- FALSE
     output$sel_aerelprn_check <- renderUI({
       shiny::HTML(paste0('<span style = "color:#ffffff"> <i class="fa-solid fa-times">
                          </i>
                          Optional variable AERELPRN/AERELPR is not available. Please select another variable to use the functionality </span>'))
      })
   } else {
     aerelprn <- merged_adae_adsl_data()[[input$sel_aerelprn]]
       if (
         !input$sel_aerelprn %in% c("AERELPRN", "AERELPR")) {
         aerelprn_check_flag$val <- TRUE
          output$sel_aerelprn_check <- renderUI({
         shiny::HTML(paste0('<span style = "color: #ffffff"> <i class="fa-solid fa-question"> </i>
                            Variable AERELPRN/AERELPR is not available or selected. </span>'))
        })
     } else if(
         input$sel_aerelprn %in% c("AERELPRN", "AERELPR")){
       aerelprn_check_flag$val <- TRUE
        output$sel_aerelprn_check <- renderUI({
         shiny::HTML(paste0('<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'))
        })
     }
   }
   }
 }, ignoreNULL = FALSE, ignoreInit = TRUE)



#### AEACNN ####
output$sel_aeacnn <- shiny::renderUI({
    shiny::req(merged_adae_adsl_data())
    adae <- shiny::req(adae_data_reac())
    adsl <- adsl_data_reac()

    is.convertible.to.flag <- function(x) as.character(x) %in% c("Y","y","Yes","yes","YES","1","0","N","n","No","NO","",".")

    choices <- sort(c(names(which(apply(apply(adae,2,function(x){is.convertible.to.flag(x)}),2,all)))))
    if(!is.null(adsl)) {
      choices2 <- names(which(apply(apply(adsl,2,function(x){is.convertible.to.flag(x)}),2,all)))
      choices <- sort(c(choices, choices2))
    }

    choices <- c(unique(choices))

     if (!any(c("AEACNN", "AEACN") %in% choices)) {
      selected <- NULL
    } else {
      selected <- c("AEACNN", "AEACN")[which(c("AEACNN", "AEACN") %in% choices)[1]]
    }


    shinyWidgets::pickerInput(
      inputId = "sel_aeacnn",
      label = shiny::HTML('<p style = "color:#ffffff"> Action Taken with Study Treatment: </p>'),
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1, `live-search` = TRUE)
    )
  })

 aeacnn_check_flag <- reactiveValues(val = FALSE)

shiny::observeEvent(c(merged_adae_adsl_data(), input$sel_aeacnn), {
   shiny::req(merged_adae_adsl_data())

    if (is.null(input$sel_aeacnn)) {
      output$sel_aeacnn_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa-solid fa-times">
            </i>
            Please select a variable for action taken with study treatment flag. </span>'
          )
        )
      })
      aeacnn_check_flag$val <- FALSE
    } else {

     if(input$sel_aeacnn == "Nothing selected") {
        aeacnn_check_flag$val <- FALSE
        output$sel_aeacnn_check <- renderUI({
         shiny::HTML(paste0('<span style = "color:#ffffff"> <i class="fa-solid fa-times"></i>
                            Variable AEACNN is not available. Please select another variable or use the functionality. </span>'))
        })
     } else {
       aeacnn <- merged_adae_adsl_data()[[input$sel_aeacnn]]
         if (
             !input$sel_aeacnn %in% c("AEACNN", "AEACN")) {
           aeacnn_check_flag$val <- TRUE
            output$sel_aeacnn_check <- renderUI({
           shiny::HTML(paste0('<span style = "color: #ffffff"> <i class="fa-solid fa-question">
                              </i>Variable AEACNN/AEACN is not available or selected. </span>'))
          })
       } else if (
         input$sel_aeacnn %in% c("AEACNN", "AEACN")){
         aeacnn_check_flag$val <- TRUE
          output$sel_aeacnn_check <- renderUI({
           shiny::HTML(paste0('<span style = "color: #16de5f"> <i class="fa-solid fa-check"></i></span>'))
          })
     }
   }
    }
 }, ignoreNULL = FALSE, ignoreInit = TRUE)
 #### Button color ####
  output$cont1 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(
          HTML(
            '#submit{color: #ffffff; background-color:#e3e3e3;
            border-color: #858585}'
          )
        )
      ),
      shiny::tags$head(
        tags$style(
          HTML(
            '#reset_fileinput_adae{color: #ffffff; background-color:#4a4a4a;
            border-color: #858585}'
          )
        )
      ),
       shiny::tags$head(
        tags$style(
          HTML(
            '#reset_fileinput_adsl{color: #ffffff; background-color:#4a4a4a;
            border-color: #858585}'
          )
        )
      )
    )
  })

  output$cont1_text <- shiny::renderUI({
    HTML(paste0("<b style='color: #e3e3e3; border-color: #858585'>
      Please upload adverse event data set!
      After the upload check all required variables and press 'Submit'.
      For more information use the help buttons on top. </b>"))
  })

  output$text_imputations <- shiny::renderUI({
    HTML(paste0(
      "<p> Note: </p>",
      "<p> &#8656; Start day imputed </p>",
      "<p> &#8658; End day imputed </p>",
      "<p> &#8660; : Start and end day imputed</p>"
      )
    )
  })

  shiny::observeEvent(
    c(saffn_check_flag$val,
      subjidn_check_flag$val,
      aedecod_check_flag$val,
      trt01a_check_flag$val,
      lvdt_check_flag$val,
      dthdt_check_flag$val,
      aesevn_check_flag$val,
      aestdy_check_flag$val,
      aeendy_check_flag$val,
      trtsdt_check_flag$val,
      aetrtemn_check_flag$val
    ), {
    if (saffn_check_flag$val && subjidn_check_flag$val && aedecod_check_flag$val &&
        trt01a_check_flag$val && lvdt_check_flag$val && dthdt_check_flag$val &&
        aesevn_check_flag$val && aestdy_check_flag$val && aeendy_check_flag$val &&
        trtsdt_check_flag$val && aetrtemn_check_flag$val) {
      output$cont1 <- shiny::renderUI({
        list(
          shiny::tags$head(
            tags$style(HTML('#submit{color: #ffffff; background-color:#16de5f;
              border-color: #ffffff}'))
          )
        )
      })
      output$cont1_text <- shiny::renderUI({
        HTML(paste0("<b style='color: #16de5f; border-color: #858585'> Please press 'Submit' to start if you finished uploading! </b>"))
      })
    } else {
      shinyjs::disable("submit")
    }
  })

  output$table_ae <- DT::renderDataTable(adae_data_reac(), options = list(autoWidth = FALSE))
  output$table_pat <- DT::renderDataTable(adsl_data_reac(), options = list(autoWidth = FALSE))

})


