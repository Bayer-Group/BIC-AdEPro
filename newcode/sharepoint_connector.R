Sys.setenv(sharepoint_url = "https://bayergroup.sharepoint.com/sites/023470/Shared%20Documents/test.csv")
Sys.setenv(sharepoint_url = "")

Sys.setenv(sharepoint_pw = "")
Sys.setenv(sharepoint_user = "mvazn")
Sys.setenv(sharepoint_api_endpoint="https://yavin.web.ddc.cnb/api/v1/sharepoint/download_file")
####################
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(readr)

fetch_csv_from_api <- function(sharepoint_api_endpoint, sharepoint_url, username, password) {
  response <- POST(
    url = sharepoint_api_endpoint,
    body = toJSON(list(path = sharepoint_url, username = username, password = password), auto_unbox = TRUE),
    encode = "json",
    content_type("application/json")
  )
  if (status_code(response) == 200) {
    list(success = TRUE, data = read_csv(content(response, "text")))
  } else {
    list(success = FALSE, error = paste("Failed to fetch CSV. HTTP Status:", status_code(response)))
  }
}

ui <- fluidPage(
  titlePanel("Adepro POC"),
  fileInput("adslFile", "Upload ADSLDATA (.sas7bdat):", accept = ".sas7bdat"),
  fileInput("adaeFile", "Upload ADAE (.sas7bdat):", accept = ".sas7bdat"),
  conditionalPanel(
    condition = "output.csvDataAvailable",
    tagList(
      selectInput("studyDropdown", "Select Study:", choices = NULL),  # Dynamische Auswahl später im Server gefüllt
      textInput("username", "Username:", ""),
      passwordInput("password", "Password:", "")
    )
  ),
  actionButton("submitBtn", "Submit"),
  textOutput("path")
)

server <- function(input, output, session) {
  sharepoint_url <- Sys.getenv("sharepoint_url", "")
  sharepoint_api_endpoint <- Sys.getenv("sharepoint_api_endpoint", "")
  username <- Sys.getenv("sharepoint_user", "")
  password <- Sys.getenv("sharepoint_pw", "")

  rv <- reactiveValues(studies = NULL, csvData = NULL)

  observe({
    if (all(nchar(c(sharepoint_url, sharepoint_api_endpoint, username, password)) > 0)) {
      result <- fetch_csv_from_api(sharepoint_api_endpoint, sharepoint_url, username, password)
      if (result$success) {
        rv$csvData <- result$data
        rv$studies <- unique(rv$csvData$Study)
      } else {
        showModal(modalDialog(title = "Error", result$error, easyClose = TRUE))
      }
    }
  })

  output$csvDataAvailable <- reactive({
    !is.null(rv$csvData)  # TRUE wenn csvData geladen ist
  })
  outputOptions(output, "csvDataAvailable", suspendWhenHidden = FALSE)

  observe({
    if (!is.null(rv$studies)) {
      updateSelectInput(session, "studyDropdown", choices = rv$studies)
    }
  })

  observeEvent(input$submitBtn, {
    output$path <- renderText({
      if (!is.null(input$studyDropdown)) {
        rv$csvData$path[rv$csvData$Study == input$studyDropdown]
      } else {
        "No study selected"
      }
    })
  })
}

shinyApp(ui = ui, server = server)
