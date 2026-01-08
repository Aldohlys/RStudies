# Historical Options Demo Application
# Demonstrates loading a position from JSON and displaying historical option data
# ------------------------------------------------------------------

library(shiny)
library(jsonlite)
library(dplyr)

# Initialize Tdata for database and Python access
library(Tdata)

# Import the historical option UI module
box::use(studies/view/historicalOptionUI)

# Helper function to parse position JSON file
parse_position_json <- function(json_path) {
  tryCatch({
    position_data <- jsonlite::fromJSON(json_path)

    # Validate structure
    if (is.null(position_data$metadata) || is.null(position_data$position)) {
      stop("Invalid position JSON structure")
    }

    return(position_data)
  }, error = function(e) {
    logger::log_error("Error parsing position JSON: {e$message}", namespace = "HistOptDemo")
    return(NULL)
  })
}

# Helper function to extract contract specification from position
extract_contract_from_position <- function(position_data) {
  tryCatch({
    metadata <- position_data$metadata

    # Handle position - can be list or data frame
    if (is.data.frame(position_data$position)) {
      position <- as.list(position_data$position[1, ])
    } else {
      position <- position_data$position[[1]]
    }

    # Get ticker information for trading_class and exchange
    ticker_data <- getTicker(metadata$symbol)

    # Extract trading class from data frame (first row)
    trading_class <- if (is.data.frame(ticker_data)) {
      ticker_data$TradingClass[1]
    } else {
      ticker_data$TradingClass
    }

    # Get exchange - prefer from metadata if available, otherwise from ticker
    exchange <- if (!is.null(metadata$exchange)) {
      metadata$exchange
    } else {
      # Get from ticker data
      if (is.data.frame(ticker_data)) {
        ticker_data$OptExchange[1]
      } else {
        ticker_data$OptExchange
      }
    }

    # Parse expiration date
    exp_date <- as.Date(position$expdate)
    expiration_str <- format(exp_date, "%Y%m%d")

    # Calculate DTE
    dte <- as.numeric(difftime(exp_date, Sys.Date(), units = "days"))

    # Create contract specification
    contract <- list(
      symbol = metadata$symbol,
      trading_class = trading_class,
      exchange = exchange,
      strike = position$strike,
      type = position$type,
      exp_date = exp_date,
      expiration_str = expiration_str,
      currency = metadata$currency,
      multiplier = metadata$multiplier,
      dte = dte
    )

    return(contract)

  }, error = function(e) {
    logger::log_error("Error extracting contract: {e$message}", namespace = "HistOptDemo")
    return(NULL)
  })
}

# UI definition
ui <- fluidPage(
  titlePanel("Historical Option Data Viewer"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Position Loader"),

      # File selector for position JSON
      fileInput(
        "position_file",
        "Select Position JSON:",
        accept = c(".json"),
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),

      # Or use example position
      actionButton(
        "load_example",
        "Load Example Position",
        icon = icon("file"),
        class = "btn-info",
        style = "width: 100%; margin-bottom: 10px;"
      ),

      hr(),

      h4("Position Details"),
      uiOutput("position_summary"),

      hr(),

      h4("Data Options"),
      selectInput(
        "data_type",
        "Data Type:",
        choices = c("Historical" = "historical", "Intraday" = "intraday"),
        selected = "historical"
      ),

      selectInput(
        "what_to_show",
        "What to Show:",
        choices = c("TRADES", "MIDPOINT", "BID_ASK"),
        selected = "TRADES"
      ),

      hr(),

      h4("About"),
      p("This app demonstrates loading a saved position and retrieving historical option data."),
      tags$ul(
        tags$li("Load a position JSON file"),
        tags$li("View contract details"),
        tags$li("Retrieve historical price/volume data"),
        tags$li("Visualize data with interactive plots")
      )
    ),

    mainPanel(
      width = 9,

      # Status message
      uiOutput("load_status"),

      # Historical option data module
      historicalOptionUI$ui("historical_option")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Reactive value to store loaded position
  loaded_position <- reactiveVal(NULL)
  loaded_contract <- reactiveVal(NULL)

  # Load historical config
  historical_config <- tryCatch({
    config_path <- config::get("historical_config")
    jsonlite::fromJSON(config_path)
  }, error = function(e) {
    logger::log_error("Failed to load historical config: {e$message}", namespace = "HistOptDemo")
    list(
      intraday_what_to_show = c("TRADES", "BID_ASK"),
      historical_what_to_show = c("TRADES", "MIDPOINT", "BID_ASK")
    )
  })

  # Update what_to_show choices based on data_type selection
  observeEvent(input$data_type, {
    choices <- if (input$data_type == "intraday") {
      historical_config$intraday_what_to_show
    } else {
      historical_config$historical_what_to_show
    }

    updateSelectInput(
      session,
      "what_to_show",
      choices = choices,
      selected = choices[1]
    )
  })

  # Handle example position load
  observeEvent(input$load_example, {
    # Path to example position
    example_path <- "C:/Users/aldoh/Documents/RApplication/data/positions/Long_ITB_Call_110_20251128.json"

    if (!file.exists(example_path)) {
      showNotification(
        "Example position file not found. Please select a position file manually.",
        type = "error",
        duration = 5
      )
      return()
    }

    # Parse position
    position <- parse_position_json(example_path)

    if (is.null(position)) {
      showNotification("Failed to parse example position", type = "error")
      return()
    }

    loaded_position(position)

    # Extract contract
    contract <- extract_contract_from_position(position)

    if (is.null(contract)) {
      showNotification("Failed to extract contract from position", type = "error")
      return()
    }

    loaded_contract(contract)

    showNotification("Example position loaded successfully!", type = "message", duration = 3)
  })

  # Handle file upload
  observeEvent(input$position_file, {
    req(input$position_file)

    file_path <- input$position_file$datapath

    # Parse position
    position <- parse_position_json(file_path)

    if (is.null(position)) {
      showNotification("Failed to parse position file", type = "error")
      return()
    }

    loaded_position(position)

    # Extract contract
    contract <- extract_contract_from_position(position)

    if (is.null(contract)) {
      showNotification("Failed to extract contract from position", type = "error")
      return()
    }

    loaded_contract(contract)

    showNotification(
      paste0("Position loaded: ", position$metadata$name),
      type = "message",
      duration = 3
    )
  })

  # Display load status
  output$load_status <- renderUI({
    if (is.null(loaded_contract())) {
      div(
        class = "alert alert-info",
        icon("info-circle"),
        " Load a position file or click 'Load Example Position' to begin."
      )
    } else {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        " Position loaded successfully. Click 'Get Historical Data' below to retrieve option data."
      )
    }
  })

  # Display position summary
  output$position_summary <- renderUI({
    position <- loaded_position()

    if (is.null(position)) {
      return(p("No position loaded", style = "color: #999;"))
    }

    metadata <- position$metadata

    # Handle position - can be list or data frame
    if (is.data.frame(position$position)) {
      pos <- as.list(position$position[1, ])
    } else {
      pos <- position$position[[1]]
    }

    tagList(
      p(tags$strong("Name: "), metadata$name),
      p(tags$strong("Symbol: "), metadata$symbol),
      p(tags$strong("Strategy: "), metadata$strategy),
      hr(),
      p(tags$strong("Leg 1:")),
      tags$ul(
        tags$li(paste("Type:", pos$type)),
        tags$li(paste("Strike:", pos$strike)),
        tags$li(paste("Expiration:", pos$expdate)),
        tags$li(paste("Trade Price:", sprintf("$%.2f", pos$trade_price)))
      )
    )
  })

  # Initialize historical option module with contract and data options
  historical_result <- historicalOptionUI$server(
    "historical_option",
    contract = loaded_contract,
    data_type = reactive(input$data_type),
    what_to_show = reactive(input$what_to_show)
  )

  # Session cleanup
  session$onSessionEnded(function() {
    logger::log_info("Session ended", namespace = "HistOptDemo")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
