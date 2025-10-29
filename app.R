# app.R
library(shiny)
library(googlesheets4)

# Deauthorize to access public sheets without authentication
gs4_deauth()

# Google Sheet ID from environment variable or hardcoded for testing
SHEET_ID <- Sys.getenv("GOOGLE_SHEET_ID", "your-sheet-id-here")

ui <- fluidPage(
  titlePanel("Google Sheets Integration (Public Sheet)"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Write Data"),
      p("Note: Writing requires sheet to be publicly editable (not recommended)"),
      textInput("name", "Name:"),
      textInput("value", "Value:"),
      actionButton("write_btn", "Add to Sheet", class = "btn-primary"),
      hr(),
      actionButton("refresh_btn", "Refresh Data", class = "btn-info")
    ),
    
    mainPanel(
      h3("Current Sheet Data"),
      tableOutput("sheet_data")
    )
  )
)

server <- function(input, output, session) {
  
  sheet_data <- reactiveVal(NULL)
  
  read_sheet_data <- function() {
    tryCatch({
      data <- read_sheet(SHEET_ID)
      sheet_data(data)
    }, error = function(e) {
      showNotification(
        paste("Error reading sheet:", e$message),
        type = "error"
      )
    })
  }
  
  observe({
    read_sheet_data()
  })
  
  observeEvent(input$write_btn, {
    req(input$name, input$value)
    
    tryCatch({
      new_row <- data.frame(
        Name = input$name,
        Value = input$value,
        Timestamp = Sys.time(),
        stringsAsFactors = FALSE
      )
      
      sheet_append(SHEET_ID, new_row)
      
      showNotification("Data added successfully!", type = "message")
      
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "value", value = "")
      
      read_sheet_data()
      
    }, error = function(e) {
      showNotification(
        paste("Error writing to sheet:", e$message),
        type = "error"
      )
    })
  })
  
  observeEvent(input$refresh_btn, {
    read_sheet_data()
    showNotification("Data refreshed!", type = "message")
  })
  
  output$sheet_data <- renderTable({
    sheet_data()
  })
}

shinyApp(ui = ui, server = server)
