# shared/universe.R — Wrapper around Tdata::getScannerUniverse() with caching
#
# Loads the scanner universe once per session and provides helper accessors.
# Used by both macro_context and swing_scanner reports.

.universe_cache <- new.env(parent = emptyenv())

#' Load scanner universe (cached per session)
#'
#' On first load, syncs Tickers (IV=YES, Type=STK, price <= $500) into
#' ScannerUniverse so new tickers are automatically included.
#' @param force Logical. Force reload from DB even if cached
#' @return data.frame with Symbol, Sector, Role, IsActive, Notes
get_universe <- function(force = FALSE) {
  if (force || is.null(.universe_cache$data)) {
    if (exists("syncTickersToScanner", envir = asNamespace("Tdata"))) {
      Tdata::syncTickersToScanner(max_price = 500)
    }
    .universe_cache$data <- Tdata::getScannerUniverse()
    message(sprintf("Universe loaded: %d symbols", nrow(.universe_cache$data)))
  }
  .universe_cache$data
}

#' Get macro tickers
get_macro_tickers <- function() {
  u <- get_universe()
  u$Symbol[u$Role == "macro"]
}

#' Get sector ETFs as named list (sector -> etf symbol)
get_sector_etfs <- function() {
  u <- get_universe()
  etfs <- u[u$Role == "etf", ]
  stats::setNames(etfs$Symbol, etfs$Sector)
}

#' Get scanner stocks for a sector
get_sector_stocks <- function(sector) {
  u <- get_universe()
  u$Symbol[u$Role == "scanner" & u$Sector == sector]
}

#' Get all scanner sectors (excluding Macro)
get_sectors <- function() {
  u <- get_universe()
  unique(u$Sector[u$Sector != "Macro"])
}
