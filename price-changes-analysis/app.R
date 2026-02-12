# Demo application for Significant Price Changes Module
library(shiny)
library(dplyr)
library(DT)

# For PDF export functionality
library(webshot)
library(htmlwidgets)

# Optional but recommended for spinners
library(shinycssloaders)

# Initialize logging
library(logger)
library(Tlogger)

setup_namespace_logging(
  "RStudies",
  console_level = "DEBUG",
  file_level = "INFO"
)

log_info("Logging initialized for RStudies applications", namespace="RStudies")

# Initialize Tdata with proper configuration
# The environment variables should handle the paths automatically
library(Tdata)

# Import the price changes module
box::use(studies/view/priceChangesUI)

# UI definition
ui <- fluidPage(
  # Application title
  titlePanel("Stock Price Analysis Tools"),

  # Module contains its own sidebar layout
  priceChangesUI$ui("price_changes")
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
    log_info("Session ended", namespace="RStudies")
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
