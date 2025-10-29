# app.R
library(shiny)
library(googlesheets4)
library(googledrive)

# Authentication configuration
# For local development: set environment variables or use interactive auth
# For Connect.cloud: set environment variables in the deployment settings
# For shinyapps.io: set environment variables in the application settings

auth_method <- Sys.getenv("GOOGLE_AUTH_METHOD", "oauth")

if (auth_method == "service_account") {
  # Service account authentication (recommended for production)
  service_account_key <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY")
  
  if (service_account_key != "") {
    # Write the key to a temporary file
    key_file <- tempfile(fileext = ".json")
    writeLines(service_account_key, key_file)
    
    gs4_auth(path = key_file)
    drive_auth(path = key_file)
    
    # Clean up
    on.exit(unlink(key_file), add = TRUE)
  } else {
    stop("GOOGLE_SERVICE_ACCOUNT_KEY environment variable not set")
  }
  
} else if (auth_method == "oauth") {
  # OAuth authentication
  email <- Sys.getenv("GOOGLE_AUTH_EMAIL")
  
  if (email == "") {
    # Interactive authentication for local development
    gs4_auth(email = TRUE)
    drive_auth(email = TRUE)
  } else {
    # Use cached credentials with specific email
    gs4_auth(
      cache = ".secrets",
      email = email,
      use_oob = TRUE
    )
    
    drive_auth(
      cache = ".secrets", 
      email = email,
      use_oob = TRUE
    )
  }
}

# Google Sheet ID from environment variable
SHEET_ID <- Sys.getenv("GOOGLE_SHEET_ID")

if (SHEET_ID == "") {
  stop("GOOGLE_SHEET_ID environment variable not set")
}

ui <- fluidPage(
  titlePanel("Google Sheets Integration"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Write Data"),
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
