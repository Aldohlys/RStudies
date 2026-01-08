# Historical Options Demo Application

Interactive Shiny application demonstrating historical option data retrieval and visualization.

## Overview

This demo app showcases:
- Loading position data from JSON files
- Extracting option contract specifications
- Retrieving historical option data from IBKR (via Tdata Python module)
- Visualizing price and volume history with interactive plots
- Exporting data to CSV

## Features

### Position Loading
- **File Upload**: Select any position JSON file from your filesystem
- **Example Position**: Click "Load Example Position" to use `Long_ITB_Call_110_20251128.json`

### Data Retrieval
- **Get Historical Data**: Retrieve cached data or fetch from IBKR if not available
- **Force Update**: Skip cache and fetch fresh data from IBKR
- **Auto-caching**: Retrieved data is automatically cached for future use

### Visualization
- **Price History Chart**: Interactive plotly chart showing option price over time
- **Volume History Chart**: Bar chart showing volume distribution
- **Data Table**: Sortable, filterable table with all historical data points

### Export
- **Download CSV**: Export historical data to CSV file

## Requirements

### R Packages
- `shiny`
- `jsonlite`
- `dplyr`
- `Tdata` (>= 5.7.20)
- Box modules from `Tuser/studies/view/`

### Python
- Python environment with `tdata_py` module
- IBKR TWS or Gateway running (for fresh data retrieval)

## Usage

### Running the App

**From RStudio:**
```r
# Set working directory to RStudies/historical-options-demo
setwd("C:/Users/aldoh/Documents/RApplication/RStudies/historical-options-demo")

# Run app
shiny::runApp()
```

**From Command Line:**
```bash
cd RStudies/historical-options-demo
R -e "shiny::runApp()"
```

### Loading a Position

**Option 1: Use Example Position**
1. Click "Load Example Position" button
2. The app loads `Long_ITB_Call_110_20251128.json` automatically
3. Contract details appear in the sidebar

**Option 2: Upload Your Own Position**
1. Click "Browse..." under "Select Position JSON"
2. Navigate to your position JSON file
3. Select and open the file
4. Contract details populate automatically

### Retrieving Historical Data

1. After loading a position, click **"Get Historical Data"**
2. The app will:
   - Check if cached data exists
   - If yes, display immediately
   - If no, fetch from IBKR and cache for next time
3. View results in:
   - Price history chart (line plot)
   - Volume history chart (bar plot)
   - Data table (sortable, searchable)

### Force Refresh

If you want fresh data from IBKR:
1. Click **"Force Update"**
2. Skips cache and fetches new data
3. Updates cache with fresh data

### Export Data

1. After retrieving data, click **"Download CSV"**
2. CSV file downloads with name: `historical_[SYMBOL]_[STRIKE][TYPE]_[EXPIRATION]_[DATE].csv`

## Position JSON Format

Expected structure:
```json
{
  "position": [
    {
      "strike": 110,
      "expdate": "2025-11-28",
      "type": "Call",
      "trade_price": 2.05,
      ...
    }
  ],
  "metadata": {
    "symbol": "ITB",
    "currency": "USD",
    "trade_date": "2025-10-22",
    "multiplier": 100,
    ...
  }
}
```

## Architecture

### Modules Used
- **historicalOptionUI**: Main UI/server module for data retrieval and visualization
  - Location: `Tuser/studies/view/historicalOptionUI.R`
  - Handles: contract display, data retrieval, plotting, table rendering

### Data Flow
1. User loads position JSON
2. App parses JSON → extracts contract specification
3. Contract passed to `historicalOptionUI` module
4. Module calls `Tdata::get_or_retrieve_option_historical()`
5. Python layer checks cache → fetches from IBKR if needed
6. Data returned as R tibble → rendered in plots and tables

## Troubleshooting

### "Python module 'tdata_py' not available"
- Ensure Python environment is activated
- Check `reticulate` configuration: `reticulate::py_config()`
- Verify `tdata_py` installed: `reticulate::py_module_available("tdata_py")`

### "No data available for this contract"
- IBKR TWS/Gateway may not be running
- Contract may not have historical data available
- Check expiration date (expired contracts may have no data)
- Verify symbol and trading class in database

### "Failed to parse position JSON"
- Check JSON syntax is valid
- Ensure required fields exist: `position`, `metadata`, `symbol`, `strike`, `expdate`, `type`
- Verify date format: "YYYY-MM-DD"

## Development Notes

This app demonstrates the pattern for integrating historical option data into Shiny applications. The same module can be integrated into other apps like RPreTrade.

### Key Design Patterns
- **Reactive contract specification**: Position data → contract reactive
- **Module-based UI**: Reusable `historicalOptionUI` component
- **Graceful error handling**: Validates inputs, handles missing data
- **Auto-caching**: Transparent caching layer via Python

## Next Steps

See main project documentation for:
- Integrating into RPreTrade "Historical Options" tab
- Extending with additional visualizations
- Custom analysis functions
