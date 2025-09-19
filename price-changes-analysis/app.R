# Demo application for Significant Price Changes Module
library(shiny)
library(dplyr)
library(DT)

# For PDF export functionality
library(webshot)
library(htmlwidgets)

# Optional but recommended for spinners
library(shinycssloaders)

# Import the price changes module
box::use(studies/view/priceChangesUI)

# UI definition
ui <- fluidPage(
  # Application title
  titlePanel("Stock Price Significant Changes Analyzer"),

  # Sidebar with basic description
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("About this tool"),
      p("This app allows you to analyze significant price changes in stock data."),
      p("Enter stock symbols, set detection parameters, and run the analysis."),
      hr(),
      h4("Features:"),
      tags$ul(
        tags$li("Multiple detection methods"),
        tags$li("Advanced filtering options"),
        tags$li("Customizable thresholds"),
        tags$li("Interactive result tables")
      ),
      hr(),
      p("Module based on Tdata price change detection functions."),
      p("All parameters from the enhanced_significant_price_changes function are exposed in the UI.")
    ),

    # Main panel containing the module
    mainPanel(
      width = 9,
      priceChangesUI$ui("price_changes")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Just initialize our module with default parameters
  price_change_results <- priceChangesUI$server("price_changes")

  # You could use the returned reactive:
  # observe({
  #   results <- price_change_results()
  #   if (!is.null(results)) {
  #     # Do something with the results
  #   }
  # })

  # Global error handler for session
  session$onSessionEnded(function() {
    tstudy_log_info("[SERVER] Session ended")
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
