# app.R
library(shiny)
library(googlesheets4)

# Deauthorize to access public sheets without authentication
gs4_deauth()

# Google Sheet ID
SHEET_ID <- "151X3VHUaGwcxEMXn3IEdhWKYYHxZCQ5P6_-bsGJV5wQ"

ui <- fluidPage(
  titlePanel("Google Sheets Reader"),

  sidebarLayout(
    sidebarPanel(
      h3("Data Controls"),
      actionButton("refresh_btn", "Reload Data", class = "btn-primary"),
      hr(),
      textOutput("last_updated")
    ),

    mainPanel(
      h3("Current Sheet Data"),
      tableOutput("sheet_data")
    )
  )
)

server <- function(input, output, session) {

  sheet_data <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)

  read_sheet_data <- function() {
    tryCatch({
      data <- read_sheet(SHEET_ID)
      sheet_data(data)
      last_update(Sys.time())
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error reading sheet:", e$message),
        type = "error"
      )
    })
  }

  # Load data on startup
  observe({
    read_sheet_data()
  })

  # Reload button
  observeEvent(input$refresh_btn, {
    read_sheet_data()
  })

  output$sheet_data <- renderTable({
    sheet_data()
  })

  output$last_updated <- renderText({
    if (!is.null(last_update())) {
      paste("Last updated:", format(last_update(), "%Y-%m-%d %H:%M:%S"))
    } else {
      "Not loaded yet"
    }
  })
}

shinyApp(ui = ui, server = server)
