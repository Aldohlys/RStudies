# RStudies - Trading Research Applications

Collection of standalone research and analysis applications for trading studies.

## Available Studies

### 1. Price Changes Analysis (`price-changes-analysis/`)
- **Purpose**: Detect significant price changes in financial market data
- **Features**: Multiple detection methods, filtering options, interactive results
- **Run**: `shiny::runApp("price-changes-analysis")`

### 2. Volatility Dashboard (`volatility-dashboard/`)
- **Purpose**: Interactive volatility analysis and visualization
- **Features**: Implied volatility metrics, sector analysis, interactive charts
- **Run**: `shiny::runApp("volatility-dashboard")`

### 3. ATR & Gap Volatility (`atr-gap-volatility/`)
- **Purpose**: Distribution of true range, high − low and close-to-close moves in ATR(14) multiples; overnight-gap vs regular-session variance decomposition (Rogers-Satchell, Yang-Zhang, rolling 63-session share)
- **Run**: `Rscript atr-gap-volatility/atr_gap_study.R AAPL 2` → `output/atr_gap/AAPL/`
- **Findings**: `atr-gap-volatility/FINDINGS.md`

## Usage

Each application is self-contained and can be run independently:

```r
# From RStudies directory
shiny::runApp("price-changes-analysis")
shiny::runApp("volatility-dashboard")
```

## Dependencies

Applications use modules from `Tuser/studies/` via box imports:
- Modules are automatically available through `R_BOX_PATH`
- No additional setup required

## Adding New Studies

1. Create new directory: `RStudies/your-study-name/`
2. Add `app.R` with your Shiny application
3. Import needed modules: `box::use(studies/view/yourModule)`
4. Update this README