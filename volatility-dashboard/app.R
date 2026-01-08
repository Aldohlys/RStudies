# Shiny implementation using Plotly for interactive visualizations
# app.R

library(shiny)
library(plotly)
library(dplyr)
library(scales)
library(Tdata)

# Actual data loading logic
load_vol_data <- function() {

  all_tickers <- Tdata::getAllTickers()
  tickers_with_iv <- all_tickers[all_tickers$IV == "YES" & all_tickers$Type %in% c("STK", "IND"),]

  ### Include the following fields in data:
  ### datetime, sym, price, iv30, iv180, ivp, rv30, rvp and sector
  data <- Tdata::getStoredMetrics(tickers_with_iv$Name)
  data <- left_join(data, tickers_with_iv, join_by(sym == Name))

  # Keep only tickers where iv180 and iv30 are different from NA
  data <- data[!is.na(data$iv180) & !is.na(data$iv30),]

  # Remove duplicates (keep the latest entry for each symbol)
  data <- data |>
    select(datetime, sym, price, iv30, iv180, ivp, rv30, rvp, sector=Sector) |>
    arrange(datetime) |>
    group_by(sym) |>
    slice_tail(n = 1) |>
    ungroup()

  return(data)
}

# Plotly centered plot function
create_centered_plotly <- function(data, symbol_col, x_col, y_col, title = "Implied vol Dashboard", show_labels = TRUE) {
  # Verify that required columns exist
  required_cols <- c(symbol_col, x_col, y_col)
  if (!all(required_cols %in% names(data))) {
    stop(paste("Dataframe must contain columns:", paste(required_cols, collapse = ", ")))
  }

  # Compute medians for reference lines
  x_median <- stats::median(data[[x_col]], na.rm = TRUE)
  y_median <- stats::median(data[[y_col]], na.rm = TRUE)

  # Format hover text
  hover_text <- mapply(function(sym, price, ivp, iv30, iv180, iv_diff) {
    sprintf(
      "Symbol: %s<br>Price: $%.2f<br>IVP: %.1f<br>IV30: %.2f%%<br>IV180: %.2f%%<br>Term Structure: %.2f%%",
      sym, price, ivp, iv30*100, iv180*100, iv_diff*100
    )
  }, data[[symbol_col]], data$price, data[[x_col]], data$iv30, data$iv180, data$iv_diff, SIMPLIFY = FALSE)

  # Create plotly object
  p <- plot_ly(data, x = ~get(x_col), y = ~get(y_col),
               type = 'scatter', mode = 'markers',
               text = ~hover_text,
               color = ~sector,  # Maps sector to color automatically
               colors = viridis::cividis(n_distinct(data$sector)),
               hoverinfo = 'text',
               marker = list(size = 10, opacity = 0.8))

  # Add symbol labels if requested
  if (show_labels) {
    p <- add_annotations(p,
                         x = data[[x_col]],
                         y = data[[y_col]],
                         text = data[[symbol_col]],
                         showarrow = FALSE,
                         font = list(size = 10),
                         yshift = 10)
  }

  # Format axes labels based on column types
  if (grepl("^(ivp|rvp)$", x_col, ignore.case = TRUE)) {
    # IVP/RVP already in percentage
    x_label <- paste(x_col, "(median =", round(x_median, 1), ")")
  } else {
    # Volatility values in decimal
    x_label <- paste(x_col, "(median =", scales::percent(x_median, accuracy = 0.1), ")")
  }

  if (y_col == "iv_diff") {
    y_label <- paste("IV Term Structure (IV180 - IV30) (median =", scales::percent(y_median, accuracy = 0.1), ")")
  } else if (grepl("^(ivp|rvp)$", y_col, ignore.case = TRUE)) {
    # IVP/RVP already in percentage
    y_label <- paste(y_col, "(median =", round(y_median, 1), ")")
  } else {
    # Volatility values in decimal
    y_label <- paste(y_col, "(median =", scales::percent(y_median, accuracy = 0.1), ")")
  }

  # Add reference lines for medians
  p <- p %>%
    layout(
      title = title,
      xaxis = list(title = x_label),
      yaxis = list(title = y_label),
      shapes = list(
        # Vertical reference line at x_median
        list(
          type = "line",
          x0 = x_median,
          x1 = x_median,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "gray", dash = "dash")
        ),
        # Horizontal reference line at y_median
        list(
          type = "line",
          x0 = 0,
          x1 = 1,
          y0 = y_median,
          y1 = y_median,
          xref = "paper",
          line = list(color = "gray", dash = "dash")
        )
      )
      # Add quadrant annotations
      # annotations = list(
      #   list(
      #     x = x_median * 1.4,
      #     y = y_median * 1.4,
      #     text = "High IVP<br>+Term Structure",
      #     showarrow = FALSE,
      #     font = list(size = 9, color = "darkgray")
      #   ),
      #   list(
      #     x = x_median * 0.6,
      #     y = y_median * 1.4,
      #     text = "Low IVP<br>+Term Structure",
      #     showarrow = FALSE,
      #     font = list(size = 9, color = "darkgray")
      #   ),
      #   list(
      #     x = x_median * 1.4,
      #     y = y_median * 0.6,
      #     text = "High IVP<br>-Term Structure",
      #     showarrow = FALSE,
      #     font = list(size = 9, color = "darkgray")
      #   ),
      #   list(
      #     x = x_median * 0.6,
      #     y = y_median * 0.6,
      #     text = "Low IVP<br>-Term Structure",
      #     showarrow = FALSE,
      #     font = list(size = 9, color = "darkgray")
      #   )
      # )
    )

  return(p)
}

# Define UI for the Shiny application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-panel {
        background-color: #f5f5f5;
        padding: 10px;
        margin-bottom: 15px;
        border-bottom: 1px solid #ddd;
      }
      .controls-panel {
        background-color: #f9f9f9;
        padding: 15px;
        border-radius: 5px;
        border: 1px solid #eee;
        margin-bottom: 15px;
      }
      .plot-panel {
        border: 1px solid #eee;
        border-radius: 5px;
        padding: 15px;
        background-color: white;
      }
    "))
  ),

  # Title
  div(class = "title-panel",
      titlePanel("Implied Volatility Dashboard")
  ),

  sidebarLayout(
    # Sidebar with controls
    sidebarPanel(
      div(class = "controls-panel",
          # Symbol selection checkboxes
          checkboxGroupInput("sectors", "Select sectors:",
                             choices = NULL),

          # IVP range slider
          sliderInput("ivp_range", "IVP Range:",
                      min = 0, max = 100, value = c(0, 100)),

          # Show/hide labels
          checkboxInput("show_labels", "Show Symbol Labels", value = TRUE),

          # Action buttons
          actionButton("check_all", "Check All"),
          actionButton("uncheck_all", "Uncheck All"),
          actionButton("apply", "Apply Filters", class = "btn-primary"),
          actionButton("reset", "Reset", class = "btn-default")
      ),
      width = 3
    ),

    # Main panel with plot
    mainPanel(
      div(class = "plot-panel",
          plotlyOutput("volPlot", height = "500px"),
          downloadButton("download", "Download Plot"),

          hr(),

          h4("Quadrant Analysis"),
          p("This plot shows the relationship between IVP and the IV term structure:"),
      ),
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Load data once when app starts
  data <- load_vol_data()

  # Calculate IV difference (iv180 - iv30) and multiply by 100
  data$iv_diff <- (data$iv180 - data$iv30)*100

  # Helper function to get all sectors
  get_all_sectors <- function() {
    sort(unique(data$sector))
  }

  # Initialize UI controls immediately
  all_sectors <- get_all_sectors()
  updateCheckboxGroupInput(session, "sectors",
                           choices = all_sectors,
                           selected = all_sectors)  # Default: all checked

  ivp_min <- floor(min(data$ivp, na.rm = TRUE))
  ivp_max <- ceiling(max(data$ivp, na.rm = TRUE))
  updateSliderInput(session, "ivp_range",
                    min = ivp_min, max = ivp_max,
                    value = c(ivp_min, ivp_max))

  # Check All button
  observeEvent(input$check_all, {
    updateCheckboxGroupInput(session, "sectors",
                             selected = get_all_sectors())
  })

  # Uncheck All button
  observeEvent(input$uncheck_all, {
    updateCheckboxGroupInput(session, "sectors",
                             selected = character(0))
  })

  # Reset button - back to default state (all checked)
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "sectors",
                             selected = get_all_sectors())
    updateSliderInput(session, "ivp_range",
                      value = c(ivp_min, ivp_max))
  })

  # Trigger for filtering
  filterTrigger <- eventReactive(input$apply, {
    list(
      sectors = input$sectors,
      ivp_range = input$ivp_range
    )
  }, ignoreNULL = FALSE)

  # Filtered data
  filteredData <- reactive({


    # Reset takes precedence - return to default (all data)
    if (input$reset > 0 && input$reset >= input$apply) {
      return(data)
    }

    # Initial state - return all data
    if (input$apply == 0) {
      return(data)
    }

    # Apply current filters
    filtered <- data
    filters <- filterTrigger()

    # Filter by sectors (empty selection = no data)
    if (!is.null(filters$sectors) && length(filters$sectors) > 0) {
      filtered <- filtered[filtered$sector %in% filters$sectors, ]
    } else {
      # No sectors selected = empty result
      filtered <- filtered[FALSE, ]
    }

    # Filter by IVP range
    filtered[filtered$ivp >= filters$ivp_range[1] &
               filtered$ivp <= filters$ivp_range[2], ]
  })

  # Render the Plotly plot
  output$volPlot <- renderPlotly({
    req(filteredData())
    req(nrow(filteredData()) > 0)

    ## Get data from DB
    data = filteredData()

    # Create the plotly plot
    create_centered_plotly(
      data = data,
      symbol_col = "sym",
      x_col = "ivp",
      y_col = "iv_diff",
      title = "IVP vs IV Term Structure (IV180 - IV30)",
      show_labels = input$show_labels
    )
  })

  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste("volatility_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },

    content = function(file) {
      p <- create_centered_plotly(
        data = filteredData(),
        symbol_col = "sym",
        x_col = "ivp",
        y_col = "iv_diff",
        title = "IVP vs IV Term Structure (IV180 - IV30)",
        show_labels = input$show_labels
      )

      # Save as HTML first, then convert to PNG
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(p, temp_html)
      webshot2::webshot(temp_html, file = file, vwidth = 1200, vheight = 800)
    }
  )
}

# Run the application
runApp(shinyApp(ui = ui, server = server))
