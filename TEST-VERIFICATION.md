# RStudies Testing Verification

## ✅ Pre-Test Checklist
- [ ] R session has access to R_BOX_PATH environment variable
- [ ] Tdata package is loaded and accessible
- [ ] Tuser/studies/view/ directory contains priceChangesUI.R
- [ ] Both app.R files exist in their respective directories

## 🧪 Application Tests

### Price Changes Analysis (`price-changes-analysis/app.R`)

**Test Command:**
```r
setwd("C:/Users/aldoh/Documents/RApplication/RStudies/price-changes-analysis")
shiny::runApp(".")
```

**Verification Points:**
- [ ] No import errors on startup
- [ ] Box import resolves: `box::use(studies/view/priceChangesUI)`
- [ ] UI renders with 3 tabs (Basic Parameters, Filtering Options, Network Settings)
- [ ] Module functions work: `priceChangesUI$ui()` and `priceChangesUI$server()`
- [ ] Can enter ticker symbols and run analysis
- [ ] Results table displays with export buttons

**Common Issues to Check:**
- Import error → Verify R_BOX_PATH points to Tuser directory
- Module not found → Verify priceChangesUI.R exists in Tuser/studies/view/
- Function errors → Check if Tdata functions are accessible

### Volatility Dashboard (`volatility-dashboard/app.R`)

**Test Command:**
```r
setwd("C:/Users/aldoh/Documents/RApplication/RStudies/volatility-dashboard")
shiny::runApp(".")
```

**Verification Points:**
- [ ] No package loading errors
- [ ] Data loads successfully: `Tdata::getAllTickers()`
- [ ] Interactive plotly chart renders
- [ ] Sector filtering controls work
- [ ] IVP range slider functions properly
- [ ] Download functionality available

**Common Issues to Check:**
- Data loading error → Verify Tdata package and database access
- Chart not rendering → Check plotly package installation
- No data → Verify database path in config.yml

## 🐛 Troubleshooting Guide

### Box Import Issues
```r
# Test R_BOX_PATH configuration
Sys.getenv("R_BOX_PATH")
# Should return: "C:/Users/aldoh/Documents/RApplication/Tuser"

# Test module existence
file.exists("C:/Users/aldoh/Documents/RApplication/Tuser/studies/view/priceChangesUI.R")
# Should return: TRUE
```

### Package Dependencies
```r
# Verify required packages
library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(Tdata)
```

### Database Access
```r
# Test database connection
Sys.getenv("R_DB_PATH")
# Should return: "C:/Users/aldoh/Documents/RApplication/data/mydb.db"

# Test Tdata functions
Tdata::getAllTickers()
```

## 📋 Test Results Log

**Date:** ___________

### Price Changes Analysis
- [ ] ✅ Successful startup
- [ ] ✅ Box imports working
- [ ] ✅ UI renders correctly
- [ ] ✅ Module functions operational
- [ ] ❌ Issues encountered: ___________

### Volatility Dashboard
- [ ] ✅ Successful startup
- [ ] ✅ Data loading working
- [ ] ✅ Chart rendering correctly
- [ ] ✅ Controls functional
- [ ] ❌ Issues encountered: ___________

## 🎯 Next Steps After Testing

If tests pass:
- [ ] Mark TODO step 3 as completed
- [ ] Proceed to step 4 (verify remaining references)
- [ ] Consider additional RStudies applications

If tests fail:
- [ ] Document specific errors
- [ ] Check environment configuration
- [ ] Verify file paths and permissions
- [ ] Review migration steps