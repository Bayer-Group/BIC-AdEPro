#' User Interface of the AdEPro application
#'
#' @param id Internal parameters for {shiny}.
#'
#' @return No return value. User interface part of the app, used in launch_adepro-function.
#'
#' @keywords internal

ui <- shiny::shinyUI(
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    ## CSS code
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          "#controls {background-color: #383838;}
          .shiny-output-error-validation {color: #e43157;}
          .form-group {margin-bottom: 0 !important;}
          "
        )
      )
    ),
    shiny::tags$style(type = "text/css",# "body { overflow-y: scroll; }",
      paste0(".recalculating {opacity: 1.0;}
        .irs-bar {width: 100%; height: 25px; background: #377eb8; border-top: 1px solid #377eb8; border-bottom: 1px solid #377eb8;}
        .irs-bar-edge {background: #377eb8; border: 1px solid #377eb8; height: 25px; border-radius: 5px; width: 20px;}
        .irs-line {border: 1px solid #377eb8; height: 25px; border-radius: 5px;}
        #numberRows .irs-grid-text {display: none;}
        #heightSlider .irs-grid-text {display: none;}
        .irs-grid-pol {display: none;}
        .irs-max {font-family: 'arial'; color: #ffffff; height:15px; font-size:15px}
        .irs-min {font-family: 'arial'; color: #ffffff; height:15px; font-size:15px}
        #numberRows .irs-single {color:#ffffff; height:20px; font-size:15px;}
        #heightSlider .irs-single {color:#ffffff; height:20px; font-size:15px;}
        #slider .irs-single {color:#ffffff; background:#e43157; height:20px; font-size:15px;}
        .irs-slider {width: 30px; height: 30px; top: 22px;}
        .panel-group.sbs-panel-group{position: absolute;width: 90%;}
        .panel.panel-default{background-color:#383838; color:#ffffff;border-color:#6b6b6b}
        .panel-heading{color:#ffffff;padding:0;}
        .panel-title{background:#383838; color:#ffffff;margin-top:10px;margin-bottom:10px;padding-left:5px}
        body {background-color: #424242; color: #6b6b6b;}
        .panel-group.sbs-panel-group {position: absolute; width: 100%;}
        .panel.panel-default {background-color: #383838; color: #ffffff; border-color: #6b6b6b}
        .panel-heading {color: #ffffff; padding: 0;}
        .panel-title {background: #383838;color: #ffffff; margin-top: 10px; margin-bottom: 10px; padding-left: 5px}
        .myRow1 {background-color: #383838; height: 150px; }
        .myRow3 {background-color: #424242;}
        #plot_hoverinfo {background-color: #424242; color: #ffffff; border-color: #383838; font-size: 14px;}
        #overall_info{background-color: #424242; color: #ffffff; border-color: #383838; font-size: 14px;}
        "
      )
    ),
    # Main panel
    shiny::mainPanel(style = "padding-right: 0px; padding-left: 0px;", #class = "myRow1",
      shiny::fluidRow(class = "myRow1",
        shiny::column(2,
          shiny::br(),
            shiny::conditionalPanel(condition = "output.submitted == 1",
              shinyBS::bsCollapse(
                shinyBS::bsCollapsePanel(
                  shiny::HTML('<p style="color:white; font-size:100%;"> Modify data </p>'),
                  shiny::tags$style(".fa-robot {color:#E43157} "),
                  shiny::tags$style(".fa-refresh {color:#E43157} "),
                  shiny::conditionalPanel(condition = "input.view == 'pie'",
                    shinyWidgets::prettyToggle(
                      inputId = 'AI.AdEPro',
                      label_off = HTML("<span style='color: white; font-size: 15px;'> Add AdEPro AI </span>"),
                      label_on = HTML("<span style = 'color: white;font-size: 15px;'> Hide AdEPro AI </span>"),
                      value = FALSE,
                      outline = TRUE,
                      status_on = "default",
                      status_off = "default",
                      plain = TRUE,
                      icon_off = icon("robot"),
                      icon_on = icon ("robot")
                    )
                  ),
                  shiny::conditionalPanel(condition = "output.flag > 0",
                    shiny::fluidRow(
                      shiny::column(12,
                        shiny::conditionalPanel(condition = "input.view == 'pie'",
                          shinyWidgets::pickerInput(
                            inputId = 'methSeq',
                            label = 'Sequencing method',
                            choices = sort(
                              c('TSP',
                              'GW_single', 'GW_complete',
                              'GW_average', 'GW_ward',
                              'OLO_single', 'OLO_complete',
                              'OLO_average', 'OLO_ward',
                               'VAT'
                              )
                            ),
                            selected = 'OLO_single',
                            multiple = FALSE,
                            options = list(
                              'live-search'=TRUE,
                              'style'='background: btn-primary',
                              'header'='Select item'
                            )
                          )
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12,
                        shiny::conditionalPanel(condition = "input.view == 'pie'",
                          shiny::uiOutput('varSeq')
                        )
                      )
                    ),
                    shiny::conditionalPanel(condition = "input.view == 'pie'",
                      shiny::tags$br(),
                      shiny::helpText(
                        "Note: By clicking update the sorting will switch to 'SEQUENCING'."
                      ),
                      shiny::actionButton(
                        inputId = "AI.Update",
                        label = "Update!",
                        icon = icon("refresh"),
                        style = paste0("color:#FFFFFF ; background-color: #377EB8;")
                      ),
                      shiny::tags$br()
                    )
                  ),
                  shiny::uiOutput("ae_type"),
                  shiny::conditionalPanel(condition = "input.view == 'pie'",
                    shiny::uiOutput("ae_sorting")
                  ),
                  shiny::uiOutput("heightSlider"),
                  shiny::conditionalPanel(condition = "input.view == 'pie'",
                    shiny::uiOutput("numberRows")
                  ),
                  shiny::selectizeInput(
                    inputId = "sortTreatments",
                    label = 'Sort treatments',
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      'plugins' = list('remove_button','drag_drop')
                    )
                  )
                ),
                shinyBS::bsCollapsePanel(
                  shiny::HTML('<p style="color:white; font-size:100%;"> Adverse events for animation </p>'),
                  shiny::conditionalPanel(condition = "input.sortTreatments.length > 0",
                    shiny::radioButtons(
                      inputId = "ae_var_sorting",
                      label = "",
                      choices = c("frequency","days"),
                      selected = "days",
                      inline = TRUE
                    ),
                    shiny::br(),
                    shiny::tags$head(
                      shiny::tags$style(
                        "#remove_all_aes {
                          background-color: #424242;
                          color: #e43157;
                          border:none
                        }"
                      )
                    ),
                    shiny::actionButton(
                      inputId = "remove_all_aes",
                      label = "Remove all events",
                      icon = icon("times")
                    ),
                    shiny::br(),
                    shiny::tags$head(
                    tags$style("
                      #var ~ .selectize-control .item:nth-child(1) {
                        background-color: #e43157;
                      }
                      #var ~ .selectize-control .item:nth-child(2) {
                        background-color: #377eb8;
                      }
                      #var ~ .selectize-control .item:nth-child(3) {
                        background-color: #4daf4a;
                      }
                      #var ~ .selectize-control .item:nth-child(4) {
                        background-color: #984ea3;
                      }
                      #var ~ .selectize-control .item:nth-child(5) {
                        background-color: #ff7f00;
                      }
                      #var ~ .selectize-control .item:nth-child(6) {
                        background-color: #ffff33;
                      }
                      #var ~ .selectize-control .item:nth-child(7) {
                        background-color: #a65628;
                      }
                      #var ~ .selectize-control .item:nth-child(8) {
                        background-color: #f781bf;
                      }
                      #var ~ .selectize-control .item:nth-child(9) {
                        background-color: #21d4de;
                      }
                      #var ~ .selectize-control .item:nth-child(10) {
                        background-color: #91d95b;
                      }
                      #var ~ .selectize-control .item:nth-child(11) {
                        background-color: #b8805f;
                      }
                      #var ~ .selectize-control .item:nth-child(12) {
                        background-color: #cbbeeb;
                      }
                      "
                      )
                    ),
                    shiny::uiOutput("ae_var")
                  ),
                  shiny::conditionalPanel(condition = "input.view == 'pie'",
                    shiny::uiOutput("ae_audio"),
                    shiny::uiOutput("ae_sound1"),
                    shiny::uiOutput("ae_sound2")
                  ),
                  shinyWidgets::materialSwitch(
                    inputId = "show_imputations",
                    label = "Mark adverse events with imputed start or/and end date",
                    value = FALSE
                  ),
                  shiny::uiOutput("text_imputations")
                ),
                shinyBS::bsCollapsePanel(
                  shiny::HTML('<p style="color:white; font-size:100%;"> Subgroup setting </p>'),
                  shiny::conditionalPanel(condition = "input.view == 'pie'",
                  uiOutput("subgroup")
                  )
                ),
                multiple = TRUE,
                id = "collapse",
                open = shiny::HTML('<p style="color:white; font-size:100%;"> Plot settings</p>')
              )
            )
          ),
          shiny::column(1,
            shiny::uiOutput("speed")
          ),
          shiny::column(3,
            shiny::br(),
            shiny::conditionalPanel(condition = "output.submitted == 1",
              column(12,
                shiny::uiOutput("slider")
              ),
              shiny::conditionalPanel(condition = "output.submitted == 1",
                shiny::column(1,
                  shinyWidgets::circleButton(
                    inputId = "play",
                    label = NULL,
                    icon = icon("play"),
                    status = "primary",
                    size = "xs"
                  )
                ),
                shiny::column(1,
                  shinyWidgets::circleButton(
                    inputId = "pause",
                    label = "",
                    icon = icon("pause"),
                    no_outline = FALSE,
                    status = "primary",
                    size = "xs"
                  )
                ),
                shiny::column(1, offset = 2,
                  shinyWidgets::circleButton(
                    inputId = "backward",
                    label = "",
                    icon = icon("backward"),
                    no_outline = FALSE,
                    status = "primary",
                    size = "xs"
                  )
                ),
                shiny::column(4,
                  tags$head(
                    tags$style(
                      HTML('
                      #step_size {
                      font-family: "arial"; color: black;
                      width: "100%";
                      height: 20px;}
                      ')
                    )
                  ),
                  shinyWidgets::currencyInput(
                    inputId ="step_size",
                    label = NULL,
                    value = 1,
                    format = "integer"
                  )
                ),
                shiny::column(1, style = "padding-left:0;",
                  shinyWidgets::circleButton(
                    inputId = "forward",
                    label = "",
                    icon = icon("forward"),
                    no_outline = FALSE,
                    status = "primary",
                    size = "xs"
                  )
                )
              )
            )
          ),
          shiny::column(1,
            shiny::br(),
            shiny::tags$head(
              shiny::tags$style(
                shiny::HTML("#dayinfo {text-align: right;}"
                )
              )
            ),
            shiny::conditionalPanel(condition = "output.submitted == 1",
              shiny::uiOutput("dynamic_css"),
              shiny::verbatimTextOutput("dayinfo")
            )
          ),
          shiny::column(1,
            shiny::conditionalPanel(condition = "output.submitted == 1",
              shinyWidgets::radioGroupButtons(
                inputId = "view",
                label = "",
                choices = c('<i class="fa-solid fa-pie-chart"></i>' = "pie",
                            '<i class="fa-solid fa-bar-chart"></i>' = "chart"
                ),
                justified = TRUE,
                status = "primary"
              )
            ),
            shiny::br(),
            shiny::conditionalPanel(condition = "output.submitted == 1",
              shiny::actionButton(
                inputId = "return_upload",
                label ="Upload",
                icon = icon("step-backward"),
                style = paste0("color:#FFFFFF ; background-color: #377EB8;")
              )
            )
          ),
          shiny::column(2,
            adeproLogo(height = 120, width = 120, align = "right"),
            shiny::tags$div(style="text-align:center",
              shiny::HTML(paste0("<span style='color: white'> v.",packageVersion("adepro"),"</span>"))
            )
          )
        ),
        shiny::fluidRow(class = "myRow3",
        shiny::conditionalPanel(condition = "output.submitted == 0",
          shiny::fluidRow(
            shiny::column(2,
              shiny::tags$div(style="text-align:center",
                adeproLogo(height = 150, width = 150, align = "right")
              )
            ),
            shiny::column(9,
              shiny::column(12,
                shiny::tags$div(style="text-align:left",
                  shiny::tags$div(
                    shiny::HTML(
                      paste(
                        shiny::tags$span(
                          style = "font-size:110%; color:white;","
                          AdEPro (Animation of Adverse Event Profiles) is a shiny application for the (audio-)visualization
                          of adverse events during clinical trials. AdEPro allows the user to upload the clinical trial data using
                          the typical Analysis Data Model (ADaM) in Clinical Data
                          Interchange Standards (CDISC). For this, just upload the
                          adverse event dataset (ADAE) to AdEPro by means of the Upload
                          Data panel, either as a sas7bdat file or as comma-separated values (csv).
                          For more information click the buttons below.
                          "
                        )
                      )
                    )
                  )
                )
              ),
              shiny::column(12,
                shiny::column(2,
                  shinyWidgets::dropdownButton(
                    inputId = "MyDropDown1",
                    label = "Description",
                    shiny::tags$h4("Description"),
                    shiny::tags$h5(shiny::HTML("
                      <p>
        							The database in a clinical trial contains vast information on
        							<b>adverse events</b>, involving hundreds of different adverse
        							event terms with varying severity grades and different start and
        							end dates. Despite this plethora of information, insight into
        							adverse events in a clinical study is usually limited to simple
        							summary tables of absolute and relative numbers of adverse event
        							occurrences. <b>AdEPro</b>, an innovation of Bayer's Biostatistics
        							Innovation Center, is an unparalleled approach to <b>audio-visualize</b>
        							the safety profile of both the individual patient and the entire
        							study cohort, which enables every study team member to experience
        							the study and empathize with the patients.
                      </p>
                      <p>
        							The AdEPro shiny app depicts the <b>temporal progress of all adverse
        							events in every study subject</b> and enables the user to give
        							profound answers to complex questions surrounding adverse events
        							such as the <b>frequency, duration and correlation</b> of adverse
        							events of interest. The incorporated sound component stresses
        							changes in the adverse event profile of the study.
                      </p>
                      <p>
        							Additionally, to keep an overview of even <b>large trials</b>
        							(>500 patients), intelligent sorting algorithms under the name of
        							'<b>AdEPro AI</b>' (based on seriation techniques from the seriation
        							package by Hahsler) allow to sort the patients in a way that
        							<b>patients with similar adverse event profiles are close to one
        							another</b> and relevant differences between the treatment groups
        							regarding the displayed adverse events remain recognizable at
        							first sight.
                      </p>
                      <p>
        							As a complete alternative to the patient-by-patient 'circle view',
        							an aggregated '<b>barplot view</b>' is implemented which displays
        							the <b>aggregated numbers of subjects with adverse events</b>,
        							which can be especially helpful in case the numbers are too great
        							to be easily counted.
                      </p>
                      ")
                    ),
                    circle = TRUE,
                    status = "custom",
                    icon = icon("book-open"),
                    width = "450px",
                    tooltip = shinyWidgets::tooltipOptions(title = "Description")
                  )
                ),
                shiny::column(2,
                  shinyWidgets::dropdownButton(
                    inputId = "MyDropDown2",
                    label = "Surface",
                    shiny::tags$h4("Surface"),
                    shiny::tags$h5(shiny::HTML("
          						<p>
          							Via the 'Change view' button, the display can be switched from
          							'circle view'' to 'barplot view' and back.
          						</p>
          						<p>
          							In the 'circle view', every circle represents one subject.
          							<ul>
          							<li>A circle with <b>dark grey</b> background represents an enrolled subject </li>
          							<li>A circle with <b>light grey</b> background indicates discontinuation</li>
          							<li>A circle with <b>black</b> background indicates death</li>
          							</ul>
          						</p>
          						<p>
          							Each <b>slice</b> inside a circle  represents a different
          							<b>adverse event</b>, color-coded according to the legend in
          							the top bar. The <b>size of the slices indicates the
          							intensity</b> of the respective adverse event.  <b>Framed
          							unfilled slices</b> indicate adverse events which the patient
          							experienced at a previous time in the study, but are no
          							longer ongoing.
          						</p>
          						<p>
          							The '<b>barplot view</b>' displays the aggregated numbers
          							for the selected adverse events.
          						</p>
          						<p>
          							The top bar of the app includes several other options which
          							are further explained under 'Functionality'.
          						</p>
          						<p>
            						By clicking a single circle in the plot, the subject will be displayed as highlighted.
                        It is also possible to highlight all subjects with a specific adverse events by selecting the event
                        in the legend on the left side.
                        To remove the selection simply re-click adverse event or subject selected.
                      </p>
                      ")
                    ),
                    circle = TRUE,
                    status = "custom",
                    icon = icon("desktop"),
                    width = "450px",
                    tooltip = shinyWidgets::tooltipOptions(title = "Surface")
                  )
                ),
                shiny::column(2,
                  shinyWidgets::dropdownButton(
                    inputId = "MyDropDown3",
                    label = "Functionality",
                    shiny::tags$h4("Functionality"),
                    shiny::tags$h5(shiny::HTML("
                      <p>
            						Select adverse event and subject level data on the left-hand side
            						of the <b>Upload Data</b> page (start page) and '<b>submit</b>.'
            						For more information on the required data structure see
            						<b>Input data</b>.
            					</p>
            					<p>
            						Should the plot size of the app not fit the screen size, the
            						appearance of the app can be altered by changing the plot height
            						('<b>Choose plot height (in pixel) </b>') and the number of rows
            						('<b>Number of rows</b>') on the left-hand side in the '<b>Modify
            						data</b>' panel. Here, you can also '<b>Select type of adverse event</b>'
            						(such as serious or study drug-related), '<b>Sort patients by</b>'
            						a variable of your choosing and '<b>Sort treatments</b>'.<br>
            						If the button '<b>Add AdEPro AI</b>' is selected, additional
            						intelligent sorting algorithms are offered for use. For more
            						information on this, please see <b>AdEPro AI</b>.
            					</p>
            					<p>
            						The <b>timeline slider</b> in the top bar indicates the day of the
            						study that is currently displayed. It can be moved manually or
            						played automatically by using the navigation buttons underneath.
            						The number of days by which the slider is moved by using the forward
            						or backward button can be adjusted by changing the number in between.
            						To the left of the timeline slider is a circular slider to set the
            						'<b>Animation speed (sec.)</b>'.
            					</p>
            					<p>
            						The '<b>Adverse events for animation</b>' panel on the lower left
            						offers additional setting options. Per default, the eight events
            						with most patient days are displayed. Other events can be selected
            						using the '<b>Choose adverse events for display (max. 12)</b>' option
            						or the '<b>frequency</b>' option can be used to display the eight
            						most frequent events.
            					</p>
            					<p>
            						Via '<b>Choose adverse event for audio</b>' one specific adverse
            						event can be selected to be rendered audible for up to two treatment
            						groups that have to be selected via the '<b>Choose treatment group
            						for first sound</b>' and '<b>Choose treatment group for second
            						sound</b>' options. This activates a sound corresponding to the
            						frequency of the selected event. The sound represents a categorized
            						and normalized frequency grade of the chosen adverse event given the
            						size of the treatment group and the overall frequency of this adverse
            						event. There are four different types of sounds. The deepest sound
            						signals that no adverse event has occurred in the selected group.
            						The other three sounds increase in pitch level symbolizing equidistant
            						increases in adverse event frequency. A sound will only be audible
            						if there is a change in frequency grading in any of the treatment
            						groups of the study.
            					</p>
            					<p>
            					  The '<b>Subgroup setting</b>' panel can be used to split the plot
            					  vertically by a subgroup variable in addition to the horizontal
            					  split by treatment.
            					</p>
                      ")
                    ),
                    circle = TRUE,
                    status = "custom",
                    icon = icon("boxes"),
                    width = "450px",
                    tooltip = shinyWidgets::tooltipOptions(title = "Functionality")
                  )
                ),
                shiny::column(2,
                  shinyWidgets::dropdownButton(
                    inputId = "MyDropDown4",
                    label = "adepro Package Manual",
                    shiny::tags$h4("Input Data:"),
                    shiny::tags$h5(shiny::HTML("
                      <p>
                        AdEPro allows the user to upload the clinical trial data using the
            						standard Analysis Data Model (<b>ADaM</b>) in Clinical Data Interchange
            						Standards (<b>CDISC</b>). For this, just upload the adverse event
            						dataset (<b>adae</b>) to AdEPro by means of the '<b>Upload adverse
            						event data</b>' option on the Upload page (start page). <br>
            						In case the adae dataset is not complete (i.e., patients without any
            						adverse events are excluded), there is also an option to '<b>Upload
            						subject level data</b>' (ADSL) with complete subject information for
            						all patients in the safety analysis set. Both datasets should be in
            						sas7bdat- or csv-format.
            					</p>
            					<p>
            						Please ensure that your datasets contain all the <b>required
            						variables</b>. You may select other than the proposed default
            						variables via drop down menus.
            					</p>
            					<p>
            						Some variables which allow categorization of adverse events are
            						'<b>Optional variables</b>' and do not necessarily need to be included.
            					</p>
            					<p>
            						Further optional variables can be attached to ADAE (if not yet included)
            						to enable sorting of the patients in the 'circle view'. These must be
            						included in ADSL as well, if the decision is made to upload this
            						dataset, but do not need to be specified via drop-down menus.
            						The following variables just serve as some examples:
            						<ul>
            						  <li> Sex (e.g. SEXN) </li>
            						  <li> Race (e.g. RACEN) </li>
            						  <li> Region (e.g. CNTYGR1N) </li>
            						  <li> (other baseline variables) </li>
            						</ul>
            					</p>
                      ")
                    ),
                    circle = TRUE,
                    status = "custom",
                    icon = icon("file-import"),
                    width = "450px",
                    tooltip = shinyWidgets::tooltipOptions(title = "Input Data")
                  )
                ),
                shiny::column(2,
                  shinyWidgets::dropdownButton(
                    inputId = "MyDropDown5",
                    label = "AdEPro AI",
                    shiny::tags$h4("AdEPro AI:"),
                    shiny::tags$h5(shiny::HTML("
                      <p>
                        Animation of Adverse Event Profiles with Artificial Intelligence (in short
            						<b>AdEPro AI</b>) is a feature of AdEPro which allows a column-wise sorting
            						of patients according to similarities in their adverse event patterns to
            						facilitate an easier and more harmonious overview of the complete adverse
            						event data. To use AdEPro AI select '<b>Add AdEPro AI</b>' in the
            						'<b>Modify data </b>' panel.
            					</p>
            					<p>
            						Euclidean distances between subjects are calculated based on patient days
            						under specific adverse events. Per default, the eight most frequent adverse
            						event variables are selected as <b>sequencing input</b>. These variables
            						can also be further differentiated by severity grade (i.e., as patient days
            						under a specific adverse event with a specific severity). However, the user
            						may also select a completely different set of variables via the
            						'<b>Sequencing input</b>' option.
            					</p>
            					<p>
            						The sequence and allocation of the patients in the graphic display results
            						from following a seriation technique available in the seriation package
            						by Hahsler and can be chosen via the '<b>Sequencing method</b>' option:
            						<ul>
            						  <li> Gruvaeus Wainer heuristic with average linkage (GW_average) </li>
            						  <li> Gruvaeus Wainer heuristic with complete linkage (GW_complete) </li>
            						  <li> Gruvaeus Wainer heuristic with single linkage (GW_single) </li>
            						  <li> Gruvaeus Wainer heuristic with Ward linkage (GW_ward) </li>
            						  <li> Optimal Leaf Ordering with average linkage (OLO_average) </li>
            						  <li> Optimal Leaf Ordering with complete linkage (OLO_complete) </li>
            						  <li> Optimal Leaf Ordering with single linkage (OLO_single) </li>
            						  <li> Optimal Leaf Ordering with with Ward linkage (OLO_ward) </li>
            						  <li> Traveling Salesperson Problem (TSP) </li>
            						  <li> Visual Assessment of Tendency (VAT) </li>
            						</ul>
            					</p>
                      ")
                    ),
                    circle = TRUE,
                    status = "custom",
                    icon = icon("robot"),
                    width = "450px",
                    tooltip = shinyWidgets::tooltipOptions(title = "AdEPro AI")
                  )
                ),
                shiny::column(2,
                  shinyWidgets::dropdownButton(
                    inputId = "MyDropDown6",
                    label = "Additional information",
                    shiny::tags$h4("Additional information:"),
                    shiny::tags$h5(
                      shiny::HTML("
            					<p>
            						Please make sure to use a platform with an audio player to make full
            						use of all functionalities of AdEPro.
            					</p>
            					<p>
            						A publication with complete details on AdEPro can be found in
            						Mentenich et al. (2020): 'AdEPro: Animation of Adverse Event Profiles - Presentation
            						of an Easy-to-Use App for Visually Exploring Individual Study Data', Therapeutic
            						Innovation & Regulatory Science volume 54, pages 1512-1521 <br>
            						(https://link.springer.com/article/10.1007/s43441-020-00178-4).
            					</p>
            					<p>
            						A video for a quick introduction to AdEPro is available under: https://youtu.be/SumXdcOOrjA
            					</p>
                      ")
                    ),
                    circle = TRUE,
                    status = "custom",
                    icon = icon("info"),
                    width = "450px",
                    tooltip = shinyWidgets::tooltipOptions(title = "Additional information")
                  )
                )
              )
            )
          ),
          shiny::br(),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(3,
              shiny::column(12,
                    shiny::radioButtons(
                    inputId ="radiobutton_data",
                    label = HTML('<b style = "color: #ffffff"> Data type </b>'),
                    choices = c("File upload"),
                    selected = "File upload",
                    inline = TRUE
              ),
              shiny::br(),
              shiny::br(),

                shiny::column(8,
                  shiny::conditionalPanel("output.radio_data == 'File upload'",
                    shiny::fileInput(
                      inputId = 'tot_dat',
                      label = shiny::HTML('<p style = "color: #ffffff"> Upload adverse event data <span style = "color:#E43157">(required)</span> </p>')

                    )
                  )
                ),
                shiny::column(1,
                    shiny::conditionalPanel("output.radio_data == 'File upload'",
                      shiny::actionButton(inputId = "reset_fileinput_adae", label = "", icon =icon("times"))
                    )
                 ),
                  shiny::column(12,
                  shiny::conditionalPanel("output.radio_data == 'File upload'",
                  shiny::uiOutput("wrong_adae_format_text"),
                    shiny::br(),
                  )
                ),
                shiny::column(8,
                  conditionalPanel("output.radio_data == 'File upload'",
                  shiny::fileInput(
                    inputId = 'tot_dat2',
                    label = shiny::HTML('<p style = "color: #ffffff"> Upload subject level data <span style = "color:#16de5f">(optional)</span> </p>')
                  )
                  )
                ),
                shiny::column(1,
                    shiny::conditionalPanel("output.radio_data == 'File upload'",
                      shiny::actionButton(inputId = "reset_fileinput_adsl", label = "", icon =icon("times"))
                    )
                 ),
                shiny::column(12,
                  shiny::conditionalPanel("output.radio_data == 'File upload'",
                  shiny::uiOutput("wrong_adsl_format_text")
                  )

              ),
              # shiny::column(12,

              #   conditionalPanel("output.demo_data_exists == true",
              #     shiny::conditionalPanel("output.radio_data == 'Demo data'",
              #     shinyWidgets::prettyCheckbox(
              #       inputId ="use_demo_data",
              #       label = HTML('<b style = "color: #ffffff"> Use demo data </b>'),
              #       value = FALSE,
              #       status = "success",
              #       inline = TRUE,
              #       icon = icon("check-square")
              #     )
              #     )
              #   )
              #),
              shiny::br(),
              shiny::column(12,
                shiny::br(),
                shiny::actionButton(
                  inputId = "submit",
                  label = paste0('Submit'),
                  icon = icon("step-forward")
                )
              ),
              shiny::column(12,
                shiny::uiOutput('cont1'),
                shiny::uiOutput('cont1_text'),
              ),
              shiny::column(12,
                shiny::tags$style(".btn-custom {background-color: #ffffff; color: #000000;}"),
              )
            ),
            shiny::conditionalPanel(condition = "output.load == 1",
              shiny::column(8,
                shinyBS::bsCollapse(
                  shinyBS::bsCollapsePanel(
                    shiny::HTML('<p style="color:white; font-size:100%;"> Required variables: </p>'),
                    shiny::column(12,
                      shiny::uiOutput("aes_in_visit_check"),
                      shiny::column(2,
                        shiny::uiOutput("sel_subjidn"),
                        shiny::uiOutput("sel_subjidn_check")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_aedecod"),
                        shiny::uiOutput("sel_aedecod_check"),
                        shiny::uiOutput("sel_aedecod_check2")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_trt01a"),
                        shiny::uiOutput("sel_trt01a_check")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_trtsdt"),
                        shiny::uiOutput("sel_trtsdt_check"),
                        shiny::uiOutput("sel_trtstdt_check2")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_lvdt"),
                        shiny::uiOutput("sel_lvdt_check"),
                        shiny::uiOutput("sel_lvdt_check2")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_aetrtemn"),
                        shiny::uiOutput("sel_aetrtemn_check")
                      )
                    ),
                    shiny::column(12,
                      shiny::column(2,
                        shiny::uiOutput("sel_dthdt"),
                        shiny::uiOutput("sel_dthdt_check")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_saffn"),
                        shiny::uiOutput("sel_saffn_check")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_aestdy"),
                        shiny::uiOutput("sel_aestdy_check"),
                        shiny::uiOutput("sel_aestdy_check2"),
                        shiny::uiOutput("sel_aestdy_check3")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_aeendy"),
                        shiny::uiOutput("sel_aeendy_check"),
                        shiny::uiOutput("sel_aeendy_check2")
                      ),
                      shiny::column(2,
                        shiny::uiOutput("sel_aesevn"),
                        shiny::uiOutput("sel_aesevn_check"),
                        shiny::uiOutput("sel_aesevn_check2")
                      ),
                      shiny::column(2,
                        shiny::radioButtons(
                          inputId = "severity_grading_flag",
                          label = "Select Severity or Grading:",
                          choices = c("Severity","Grading"),
                          selected = "Severity"
                        )
                      )
                    )
                  ),
                  shinyBS::bsCollapsePanel(
                    shiny::HTML('<p style="color:white; font-size:100%;"> Optional variables: </p>'),
                    shiny::column(2,
                      shiny::uiOutput("sel_aesern"),
                      shiny::uiOutput("sel_aesern_check")
                    ),
                    shiny::column(2,
                      shiny::uiOutput("sel_aereln"),
                      shiny::uiOutput("sel_aereln_check")
                    ),
                    shiny::column(2,
                      shiny::uiOutput("sel_aerelprn"),
                      shiny::uiOutput("sel_aerelprn_check")
                    ),
                    shiny::column(2,
                      shiny::uiOutput("sel_aeacnn"),
                      shiny::uiOutput("sel_aeacnn_check")
                    )
                  ),
                  shinyBS::bsCollapsePanel(
                    shiny::HTML('<p style="color:white; font-size:100%;"> adae data (click to open/close table): </p>'),
                    shiny::wellPanel(
                      id = "table_ae_Panel",
                      style = "color:black; overflow-y:scroll; max-height: 600px",
                      shiny::dataTableOutput('table_ae')
                    )
                  ),
                  multiple = TRUE,
                  id = "collapse_adae",
                  open = shiny::HTML('<p style="color:white; font-size:100%;"> Plot settings</p>'),
                  shinyBS::bsCollapsePanel(
                    shiny::HTML('<p style="color:white; font-size:100%;"> adsl data (click to open/close table): </p>'),
                    shiny::wellPanel(id = "table_ae_Panel",style = "color:black; overflow-y:scroll; max-height: 600px",
                      shiny::dataTableOutput('table_pat')
                    )
                  )
                )
              )
            )
          )
        ),
        shiny::conditionalPanel(condition = "output.submitted == 1",
          conditionalPanel(condition = "input.sortTreatments.length > 0",
          shiny::uiOutput('ae_summary_box')
          ),
          #shiny::plotOutput(outputId = "legend", width = "100%"),#, height = "45px"),
          shiny::conditionalPanel(condition = "input.view == 'pie'",
            shiny::uiOutput('patientpanel'),

            shiny::uiOutput('rowpanel'),
            shiny::conditionalPanel(condition = "input.sortTreatments.length == 0",
              shiny::HTML('<p style="text-align:center;color:white;"> Please select at least one treatment in the "Modify data"-Panel! </p>')
            ),
            shiny::fluidRow(
              splitLayout(
                cellArgs = list(style='white-space: normal; resize: horizontal;'),
                cellWidths = c("9%", "82%","9%"),
                shiny::uiOutput("circle_legend"),
                shiny::plotOutput(
                  outputId = "slicePlots",
                  hover = hoverOpts(id ="plot_hover"),
                  click = clickOpts(id = "plot_click"),
                  height = "100%"
                ),
                shiny::uiOutput("circle_legend2")
              )
            )
          ),
          shiny::conditionalPanel(condition = "input.view == 'chart'",
            shiny::conditionalPanel(condition = "input.sortTreatments.length > 0",
               shiny::fluidRow(
                 splitLayout(
                   cellArgs = list(style='white-space: normal; resize: horizontal;'),
                   cellWidths = c("9%", "91%"),
                   shiny::uiOutput("barchart_legend"),
                   shiny::plotOutput(
                     outputId = "barchart",
                     height = "100%")
                 )
              )
            ),
            shiny::conditionalPanel(condition = "input.var.length == 0",
              shiny::HTML('<p style="text-align:center; color:white;"> To use the bar plot view, please select at least one adverse event in the "Adverse event for animation"-Panel! </p>')
            ),
            shiny::conditionalPanel(condition = "input.sortTreatments.length == 0",
              shiny::HTML('<p style="text-align:center;color:white;"> Please select at least one treatment in the "Modify data"-Panel! </p>')
            )
         ),
          shiny::uiOutput('zoompanel')
       )
    ), width = 12
  )
)
)
