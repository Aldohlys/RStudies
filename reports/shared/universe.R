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

# ── Correlation groups (ScannerUniverse.Cluster / ClusterETF) ─────────────
# Written by scripts/cluster_universe.py from return correlation (TODO 71).
# A group has at most 10 names that move together; its anchor (ClusterETF) is
# the ticker the sector gate and RS rank read. Sector stays the family label
# that keys the macro tailwind/headwind rules. Names no group correlates with
# carry Cluster = "Unclassified" and have no anchor.

UNCLASSIFIED_GROUP <- "Unclassified"

.scanner_rows <- function() {
  u <- get_universe()
  stopifnot("Cluster" %in% names(u), "ClusterETF" %in% names(u))
  u[u$Role == "scanner", , drop = FALSE]
}

#' Scanner stocks with no correlation group (Cluster NULL, empty or
#' "Unclassified") - e.g. added to Tickers after the last clustering run.
get_unclassified <- function() {
  s <- .scanner_rows()
  s$Symbol[is.na(s$Cluster) | !nzchar(s$Cluster) | s$Cluster == UNCLASSIFIED_GROUP]
}

#' All correlation groups (Unclassified excluded)
get_groups <- function() {
  s <- .scanner_rows()
  g <- unique(s$Cluster[!is.na(s$Cluster) & nzchar(s$Cluster)])
  sort(setdiff(g, UNCLASSIFIED_GROUP))
}

#' Group anchors as a named vector (group -> anchor ticker)
get_group_anchors <- function() {
  s <- .scanner_rows()
  s <- s[s$Cluster %in% get_groups() & !is.na(s$ClusterETF), , drop = FALSE]
  s <- s[!duplicated(s$Cluster), , drop = FALSE]
  stats::setNames(s$ClusterETF, s$Cluster)[get_groups()]
}

#' Scanner stocks of a correlation group
get_group_stocks <- function(group) {
  s <- .scanner_rows()
  s$Symbol[!is.na(s$Cluster) & s$Cluster == group]
}

#' Family (majority Sector) of each group, as a named vector (group -> Sector)
get_group_sectors <- function() {
  s <- .scanner_rows()
  s <- s[s$Cluster %in% get_groups(), , drop = FALSE]
  vapply(split(s$Sector, s$Cluster), function(x) names(which.max(table(x))), "")[get_groups()]
}

#' Correlation group of one symbol (NA when not a scanner name)
get_symbol_group <- function(symbol) {
  s <- .scanner_rows()
  g <- s$Cluster[s$Symbol == symbol]
  if (length(g) == 0) NA_character_ else g[1]
}
