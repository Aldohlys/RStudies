# shared/live_sources.R — per-field data resolvers for /analyze.
#
# Sourcing inversion policy (project_analyze_redesign_2026_05):
#   1. Live IBKR / yfinance (primary) when reachable and not gated by freshness.
#   2. DB cache (mydb.db: Prices, option_skew_history, option_chain_oi_history)
#      when its timestamp is within freshness$max_age_hours.
#   3. Scanner CSV (last-resort) — only for fields that have no live or DB path.
#
# Every resolver returns a uniform shape:
#   list(value = <scalar or data.frame>,
#        source = "live" | "db" | "csv" | "computed" | "unavailable",
#        retrieved_at = <POSIXct or character>,
#        reason = NULL or character — explanation if value is NA)
#
# Resolvers MUST NOT silently swallow failures: when value is NA, `reason`
# is required and gets surfaced in the report as "FETCH FAILED: <reason>".

suppressPackageStartupMessages({
  if (!requireNamespace("DBI", quietly = TRUE)) stop("DBI required")
})

# Null-coalesce — also defined in phases.R; this guard avoids ordering issues
# when live_sources.R is sourced before phases.R.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0) return(b)
    if (length(a) == 1) {
      if (is.na(a) || identical(a, "")) return(b)
    }
    a
  }
}

# ── Module-level helpers ──────────────────────────────────────────────────

#' Safe Python module accessor. Returns NULL if tdata_py not loaded.
.tdata_py <- function() {
  tryCatch(Tdata:::tdata_py, error = function(e) NULL)
}

#' Standard return-shape constructor. `status` is the neutral provenance label
#' surfaced by /analyze (see TODO #60): LIVE / CACHED / NO DATA / FETCH FAILED.
#' Defaults derive from `source` so existing call-sites need no change:
#'   source "live"                         -> LIVE
#'   source "db" / "csv" / "computed"      -> CACHED
.ok <- function(value, source, retrieved_at = Sys.time(), reason = NULL,
                status = NULL) {
  if (is.null(status))
    status <- if (identical(source, "live")) "LIVE" else "CACHED"
  list(value = value, source = source,
       retrieved_at = retrieved_at, reason = reason, status = status)
}

#' FETCH FAILED: the request never produced a response (connect refused,
#' timeout, exception, dependency unavailable). Default status.
.miss <- function(reason, source = "unavailable", status = "FETCH FAILED") {
  list(value = NA, source = source,
       retrieved_at = Sys.time(), reason = reason, status = status)
}

#' NO DATA: the request SUCCEEDED but the response was empty / all-NaN / zero
#' (illiquid strike, off-RTH, missing tick subscription). Distinct from
#' FETCH FAILED — the pipe is up, the market just had nothing to say.
.nodata <- function(reason, source = "empty") {
  .miss(reason, source = source, status = "NO DATA")
}

#' Pick the IBKR expiration whose DTE is closest to `target_dte`. Filters out
#' past expirations. Returns YYYYMMDD string or NA.
.pick_expiry_for_dte <- function(expiries, target_dte, prefer_monthly = TRUE) {
  if (is.null(expiries) || length(expiries) == 0) return(NA_character_)
  exp_dates <- as.Date(as.character(expiries), format = "%Y%m%d")
  dtes <- as.integer(exp_dates - Sys.Date())
  ok <- !is.na(dtes) & dtes > 0
  if (!any(ok)) return(NA_character_)
  expiries <- expiries[ok]; exp_dates <- exp_dates[ok]; dtes <- dtes[ok]
  # Prefer the standard monthly (3rd Friday: weekday Fri & day-of-month 15-21).
  # Monthlies carry the OI and the tight bid/ask; weeklies are often near-dead
  # (e.g. C Jul'26 weekly ATM 25% wide / OI ~1 vs monthly 7% / OI ~5k), which
  # otherwise mis-trips the vehicle rule into "stock" and prices junk spreads.
  if (isTRUE(prefer_monthly)) {
    lt <- as.POSIXlt(exp_dates)
    is_monthly <- lt$wday == 5L & lt$mday >= 15L & lt$mday <= 21L
    if (any(is_monthly))
      return(expiries[is_monthly][which.min(abs(dtes[is_monthly] - target_dte))])
  }
  expiries[which.min(abs(dtes - target_dte))]
}

# ── Spot ──────────────────────────────────────────────────────────────────

#' Live spot via Tdata::getLastSymPrice (Yahoo / IBKR depending on Tdata config).
resolve_spot <- function(ticker) {
  p <- tryCatch(Tdata::getLastSymPrice(ticker), error = function(e) NULL)
  if (is.null(p) || length(p) == 0)
    return(.miss(sprintf("getLastSymPrice returned NULL for %s", ticker)))
  if (is.data.frame(p)) {
    cand <- intersect(c("price", "Close", "close", "last", "value"), names(p))
    if (length(cand) > 0) p <- p[[cand[1]]] else {
      num_cols <- which(sapply(p, is.numeric))
      p <- if (length(num_cols) > 0) p[[tail(num_cols, 1)]] else p[[1]]
    }
  }
  v <- as.numeric(p)[1]
  if (is.na(v) || v <= 0)
    return(.miss(sprintf("getLastSymPrice returned non-numeric for %s", ticker)))
  .ok(v, source = "live")
}

# ── Sector / Sector ETF ──────────────────────────────────────────────────

#' Look up sector membership via ScannerUniverse (DB table populated from Tickers).
#' Returns NA with reason if ticker not in ScannerUniverse.
resolve_sector <- function(ticker) {
  u <- tryCatch(get_universe(), error = function(e) NULL)
  if (is.null(u)) return(.miss("ScannerUniverse not accessible"))
  hit <- u[u$Symbol == ticker, , drop = FALSE]
  if (nrow(hit) == 0)
    return(.miss(sprintf("%s not in ScannerUniverse", ticker)))
  sec <- hit$Sector[1]
  if (is.na(sec) || !nzchar(sec))
    return(.miss(sprintf("%s has no Sector field", ticker)))
  .ok(sec, source = "db")
}

#' Resolve the sector ETF symbol for a ticker via its sector membership.
resolve_sector_etf <- function(ticker) {
  sec <- resolve_sector(ticker)
  if (is.na(sec$value)) return(.miss(sec$reason))
  etfs <- tryCatch(get_sector_etfs(), error = function(e) NULL)
  if (is.null(etfs)) return(.miss("get_sector_etfs failed"))
  etf <- unname(etfs[sec$value])
  if (is.null(etf) || length(etf) == 0 || is.na(etf) || !nzchar(etf))
    return(.miss(sprintf("no ETF mapped to sector '%s'", sec$value)))
  .ok(etf, source = "db")
}

# ── Expiry ────────────────────────────────────────────────────────────────

#' Pick an IBKR expiration closest to target_dte. Live primary; no DB cache
#' (expirations are cheap to fetch). Returns YYYYMMDD string.
resolve_expiry <- function(ticker, target_dte = 45, tws_ok = TRUE) {
  if (!isTRUE(tws_ok))
    return(.miss("TWS not reachable; cannot pick live expiry"))
  py <- .tdata_py()
  if (is.null(py)) return(.miss("tdata_py unavailable; cannot pick live expiry"))
  expiries <- tryCatch(py$getExpirationDates(ticker),
                       error = function(e) conditionMessage(e))
  if (is.character(expiries) && length(expiries) == 1)
    return(.miss(paste("getExpirationDates:", expiries)))
  if (is.null(expiries) || length(expiries) == 0)
    return(.miss("no expirations from IBKR"))
  pick <- .pick_expiry_for_dte(expiries, target_dte)
  if (is.na(pick))
    return(.miss(sprintf("no future expiry near %dd DTE", target_dte)))
  .ok(pick, source = "live")
}

# ── ATM IV at target DTE (IV30 / IV90 paths) ─────────────────────────────

#' Live ATM IV via IBKR. Averages call + put IV at the strike closest to spot.
#' Returns IV as a fraction (e.g. 0.32 for 32%) to match DB Prices.iv30 scale.
.live_atm_iv <- function(ticker, spot, target_dte, tws_ok = TRUE) {
  if (!isTRUE(tws_ok)) return(.miss("TWS not reachable"))
  py <- .tdata_py()
  if (is.null(py)) return(.miss("tdata_py module unavailable"))
  if (is.na(spot)) return(.miss("spot price unavailable"))
  expiries <- tryCatch(py$getExpirationDates(ticker),
                       error = function(e) conditionMessage(e))
  if (is.character(expiries) && length(expiries) == 1)
    return(.miss(paste("getExpirationDates:", expiries)))
  if (is.null(expiries) || length(expiries) == 0)
    return(.miss("no expirations from IBKR"))
  expiration <- .pick_expiry_for_dte(expiries, target_dte)
  if (is.na(expiration)) return(.miss(sprintf("no expiry near %dd DTE", target_dte)))

  # ±4% is enough to bracket the ATM strike on any grid while qualifying far
  # fewer strikes than the old ±10% (only the ATM strike is priced below).
  strikes <- tryCatch(py$getStrikesAuto(
    sym = ticker, expiration = expiration,
    center_strike = spot, range_pct = 0.04),
    error = function(e) conditionMessage(e))
  if (is.character(strikes) && length(strikes) == 1)
    return(.miss(paste("getStrikesAuto:", strikes)))
  if (is.null(strikes) || length(strikes) == 0)
    return(.miss("no strikes near spot from IBKR"))
  strikes <- as.numeric(unlist(strikes))
  strikes <- strikes[!is.na(strikes)]
  if (length(strikes) == 0) return(.miss("all strikes from IBKR were NaN"))

  atm_strike <- strikes[which.min(abs(strikes - spot))]
  fetch <- function(right) {
    df <- tryCatch(py$getOptValue(
      sym = ticker, expiration = expiration,
      strikes = list(atm_strike), right = right,
      force_refresh = TRUE),
      error = function(e) NULL)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
    iv <- df$impliedvol[1]
    if (is.null(iv) || is.na(iv) || iv <= 0) return(NA_real_)
    as.numeric(iv)
  }
  iv_c <- fetch("C"); iv_p <- fetch("P")
  ivs <- c(iv_c, iv_p); ivs <- ivs[!is.na(ivs)]
  if (length(ivs) == 0)
    return(.nodata(sprintf("getOptValue returned 0/NaN IV for %s @ %s",
                           atm_strike, expiration)))
  .ok(mean(ivs), source = "live",
      reason = NULL)
}

#' Resolve IV30 (~30d ATM IV). DB Prices first if fresh; else live IBKR.
resolve_iv30 <- function(ticker, spot, freshness, tws_ok = TRUE, conn = NULL) {
  own_conn <- is.null(conn)
  if (own_conn) conn <- tryCatch(Tdata::safe_db_connect(),
                                  error = function(e) NULL)
  on.exit(if (own_conn && !is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  if (!is.null(conn)) {
    row <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT datetime, iv30 FROM Prices WHERE sym = ?
       ORDER BY ROWID DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
    if (!is.null(row) && nrow(row) > 0 && !is.na(row$iv30) &&
        is_fresh(row$datetime, freshness)) {
      return(.ok(as.numeric(row$iv30), source = "db",
                 retrieved_at = row$datetime))
    }
  }
  .live_atm_iv(ticker, spot, target_dte = 30, tws_ok = tws_ok)
}

#' Resolve IV90 (~90d ATM IV). DB Prices first if fresh; else live IBKR.
resolve_iv90 <- function(ticker, spot, freshness, tws_ok = TRUE, conn = NULL) {
  own_conn <- is.null(conn)
  if (own_conn) conn <- tryCatch(Tdata::safe_db_connect(),
                                  error = function(e) NULL)
  on.exit(if (own_conn && !is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  if (!is.null(conn)) {
    row <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT datetime, iv90 FROM Prices WHERE sym = ?
       ORDER BY ROWID DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
    if (!is.null(row) && nrow(row) > 0 && !is.na(row$iv90) &&
        is_fresh(row$datetime, freshness)) {
      return(.ok(as.numeric(row$iv90), source = "db",
                 retrieved_at = row$datetime))
    }
  }
  .live_atm_iv(ticker, spot, target_dte = 90, tws_ok = tws_ok)
}

# ── RV30 ──────────────────────────────────────────────────────────────────

#' Live RV30 via yfinance: 30-session sample of daily log-returns, annualized.
.live_rv30 <- function(ticker) {
  if (!requireNamespace("reticulate", quietly = TRUE))
    return(.miss("reticulate unavailable"))
  tryCatch({
    yf <- reticulate::import("yfinance", delay_load = TRUE)
    # Yahoo spells class shares with a hyphen ("BRK-B") where IBKR uses a
    # space ("BRK B"); the raw symbol returns an empty history.
    yn <- tryCatch(unname(Tdata::getYahooName(ticker)[1]),
                   error = function(e) NA_character_)
    sym <- if (is.na(yn) || !nzchar(yn)) ticker else yn
    hist <- yf$Ticker(sym)$history(period = "60d", interval = "1d")
    closes <- as.numeric(hist$Close)
    closes <- closes[!is.na(closes) & closes > 0]
    if (length(closes) < 21) stop("yfinance returned <21 closes")
    rets <- diff(log(tail(closes, 31)))
    .ok(sd(rets) * sqrt(252), source = "live")
  }, error = function(e) .miss(paste("yfinance:", conditionMessage(e))))
}

#' Resolve RV30. DB Prices first if fresh; else live yfinance.
resolve_rv30 <- function(ticker, freshness, conn = NULL) {
  own_conn <- is.null(conn)
  if (own_conn) conn <- tryCatch(Tdata::safe_db_connect(),
                                  error = function(e) NULL)
  on.exit(if (own_conn && !is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  if (!is.null(conn)) {
    row <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT datetime, rv30 FROM Prices WHERE sym = ?
       ORDER BY ROWID DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
    if (!is.null(row) && nrow(row) > 0 && !is.na(row$rv30) &&
        is_fresh(row$datetime, freshness)) {
      return(.ok(as.numeric(row$rv30), source = "db",
                 retrieved_at = row$datetime))
    }
  }
  .live_rv30(ticker)
}

# ── IVP ──────────────────────────────────────────────────────────────────

#' Interpolate IVP (0-100) from percentile breakpoints via linear interp.
#' Inputs come from Tdata::getIVPercentileLevels: current_iv + p10/p25/p50/p75/p90.
.interp_ivp <- function(current, p10, p25, p50, p75, p90) {
  breaks <- c(p10, p25, p50, p75, p90)
  pcts   <- c(10,  25,  50,  75,  90)
  ok <- !is.na(breaks)
  if (sum(ok) < 2 || is.na(current)) return(NA_real_)
  breaks <- breaks[ok]; pcts <- pcts[ok]
  # Sort ascending — guard against non-monotone IBKR returns
  o <- order(breaks); breaks <- breaks[o]; pcts <- pcts[o]
  if (current <= breaks[1]) return(pcts[1] *
                                    max(current / breaks[1], 0))
  if (current >= tail(breaks, 1)) {
    overshoot <- (current - tail(breaks, 1)) / tail(breaks, 1)
    return(min(100, tail(pcts, 1) + overshoot * (100 - tail(pcts, 1))))
  }
  # Linear interp between adjacent breakpoints
  i <- max(which(breaks <= current))
  frac <- (current - breaks[i]) / (breaks[i + 1] - breaks[i])
  pcts[i] + frac * (pcts[i + 1] - pcts[i])
}

#' Resolve IVP (Implied Volatility Percentile, 0-100).
#' Priority:
#'   1. DB Prices.ivp if fresh
#'   2. Live: Tdata::getIVPercentileLevels (252d OPTION_IMPLIED_VOLATILITY
#'      history from IBKR), then linear-interp current_iv into the percentile
#'      breakpoints.
#' Closes the historical gap exposed by the UPS test (IVP n/a → FETCH FAILED).
resolve_ivp <- function(ticker, freshness, tws_ok = TRUE, conn = NULL) {
  own_conn <- is.null(conn)
  if (own_conn) conn <- tryCatch(Tdata::safe_db_connect(),
                                  error = function(e) NULL)
  on.exit(if (own_conn && !is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  if (!is.null(conn)) {
    row <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT datetime, ivp, ivp_2y FROM Prices WHERE sym = ?
       ORDER BY ROWID DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
    if (!is.null(row) && nrow(row) > 0 && !is.na(row$ivp) &&
        is_fresh(row$datetime, freshness)) {
      return(.ok(as.numeric(row$ivp), source = "db",
                 retrieved_at = row$datetime))
    }
  }
  if (!isTRUE(tws_ok))
    return(.miss("DB Prices.ivp NA/stale; TWS not reachable for live rank"))
  res <- tryCatch(Tdata::getIVPercentileLevels(ticker),
                  error = function(e) NULL)
  if (is.null(res))
    return(.miss("getIVPercentileLevels returned NULL"))
  if (is.null(res$current) || is.na(res$current))
    return(.nodata("getIVPercentileLevels OK but no current_iv"))
  ivp <- .interp_ivp(res$current, res$p10, res$p25, res$p50, res$p75, res$p90)
  if (is.na(ivp))
    return(.miss(sprintf("interp failed: current=%.3f p10..p90=%s",
                          res$current,
                          paste(sprintf("%.3f", c(res$p10, res$p25, res$p50,
                                                  res$p75, res$p90)),
                                collapse = "/"))))
  .ok(round(ivp, 1), source = "computed",
      reason = sprintf("interp from %dd IV history (current=%.1f%%, p50=%.1f%%)",
                       res$days_covered %||% 252L,
                       res$current * 100, res$p50 * 100))
}

# ── RVP (Realized Vol Percentile) ───────────────────────────────────────

#' Resolve RVP — where current RV30 sits in its 1y history (0-100).
#' Priority: DB Prices.rvp if fresh → live via Tdata::getVolMetrics.
#' Mirrors resolve_ivp() but for realized vol.
resolve_rvp <- function(ticker, freshness, tws_ok = TRUE, conn = NULL) {
  own_conn <- is.null(conn)
  if (own_conn) conn <- tryCatch(Tdata::safe_db_connect(),
                                  error = function(e) NULL)
  on.exit(if (own_conn && !is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  if (!is.null(conn)) {
    row <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT datetime, rvp FROM Prices WHERE sym = ?
       ORDER BY ROWID DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
    if (!is.null(row) && nrow(row) > 0 && !is.na(row$rvp) &&
        is_fresh(row$datetime, freshness)) {
      return(.ok(as.numeric(row$rvp), source = "db",
                 retrieved_at = row$datetime))
    }
  }
  if (!isTRUE(tws_ok))
    return(.miss("DB Prices.rvp NA/stale; TWS not reachable for live"))
  # rvp comes from the 252d historical-vol bars. Fetch ONLY that via the python
  # helper directly, NOT Tdata::getVolMetrics — the latter also computes the
  # iv15/30/90/180 term structure (8 option-chain fetches) and writes the Prices
  # DB, all wasted here. /analyze is a read tool; the scanner keeps Prices warm.
  py <- .tdata_py()
  if (is.null(py))
    return(.miss("tdata_py unavailable for live RV percentile"))
  res <- tryCatch(as.data.frame(py$get_volatility_metrics(
                    sym = ticker, lookback_days = 252L, hist = TRUE, price = FALSE)),
                  error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0)
    return(.nodata("get_volatility_metrics returned empty"))
  rvp <- suppressWarnings(as.numeric(res$hv_percentile[1]))
  if (is.na(rvp)) return(.nodata("get_volatility_metrics returned NaN hv_percentile"))
  rv30 <- suppressWarnings(as.numeric(res$current_hv[1]))
  .ok(round(rvp, 1), source = "live",
      reason = sprintf("from hist-vol bars (RV30=%.1f%%, no IV term structure)",
                       rv30 * 100))
}

# ── 25-delta skew (RR_25) ────────────────────────────────────────────────

#' Live 25-delta call/put IV, returning the RR in vol-points: (call25 - put25)*100.
.live_25d_skew <- function(ticker, spot, target_dte = 30, tws_ok = TRUE) {
  if (!isTRUE(tws_ok)) return(.miss("TWS not reachable"))
  py <- .tdata_py()
  if (is.null(py)) return(.miss("tdata_py module unavailable"))
  if (is.na(spot)) return(.miss("spot price unavailable"))
  expiries <- tryCatch(py$getExpirationDates(ticker),
                       error = function(e) conditionMessage(e))
  if (is.character(expiries) && length(expiries) == 1)
    return(.miss(paste("getExpirationDates:", expiries)))
  if (is.null(expiries) || length(expiries) == 0)
    return(.miss("no expirations from IBKR"))
  expiration <- .pick_expiry_for_dte(expiries, target_dte)
  if (is.na(expiration))
    return(.miss(sprintf("no expiry near %dd DTE", target_dte)))

  # Locate the 25Δ wings analytically (rough IV from DB, 0.30 fallback) and
  # price only their neighborhoods — not the whole ±25% chain. Final pick is by
  # actual delta (pick_25d below).
  sigma <- .rough_iv30(ticker)
  dte   <- tryCatch(as.integer(as.Date(expiration, "%Y%m%d") - Sys.Date()),
                    error = function(e) NA_integer_)
  Tyr   <- (if (is.na(dte)) target_dte else dte) / 365
  ssT   <- sigma * sqrt(Tyr)
  drift <- 0.5 * sigma^2 * Tyr
  k_c25 <- spot * exp( 0.6745 * ssT + drift)   # 25Δ call (OTM, above spot)
  k_p25 <- spot * exp(-0.6745 * ssT + drift)   # 25Δ put  (OTM, below spot)

  span <- max(abs(k_c25 / spot - 1), abs(1 - k_p25 / spot)) + 0.02
  all_strikes <- tryCatch(py$getStrikesInRange(
    sym = ticker, expiration = expiration,
    center_strike = spot, range_pct = span),
    error = function(e) conditionMessage(e))
  if (is.character(all_strikes) && length(all_strikes) == 1)
    return(.miss(paste("getStrikesInRange:", all_strikes)))
  all_strikes <- sort(as.numeric(unlist(all_strikes)))
  all_strikes <- all_strikes[!is.na(all_strikes)]
  if (length(all_strikes) == 0) return(.miss("no strikes from IBKR"))

  .near <- function(center)
    Tbasics::get_nearest_values(all_strikes, center, n_below = 1, n_above = 1)
  strikes <- sort(unique(c(.near(k_c25), .near(k_p25))))

  fetch_df <- function(right) {
    tryCatch(py$getOptValue(
      sym = ticker, expiration = expiration,
      strikes = as.list(strikes), right = right,
      force_refresh = TRUE),
      error = function(e) NULL)
  }
  df_c <- fetch_df("C"); df_p <- fetch_df("P")
  if ((is.null(df_c) || nrow(df_c) == 0) &&
      (is.null(df_p) || nrow(df_p) == 0))
    return(.nodata(sprintf("getOptValue OK but empty for both wings on %s",
                           expiration)))

  pick_25d <- function(df, want_sign) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
    delt <- as.numeric(df$delta); iv <- as.numeric(df$impliedvol)
    ok <- !is.na(delt) & !is.na(iv) & iv > 0
    if (sum(ok) == 0) return(NA_real_)
    delt <- delt[ok]; iv <- iv[ok]
    iv[which.min(abs(delt - 0.25 * want_sign))]
  }
  c25 <- pick_25d(df_c,  1)
  p25 <- pick_25d(df_p, -1)
  if (is.na(c25) && is.na(p25))
    return(.nodata("getOptValue OK but no 25Δ strike carried IV/delta"))
  if (is.na(c25) || is.na(p25))
    return(.nodata(sprintf("only one wing available (call25=%s, put25=%s)",
                          ifelse(is.na(c25), "n/a", sprintf("%.4f", c25)),
                          ifelse(is.na(p25), "n/a", sprintf("%.4f", p25)))))
  .ok(list(rr_vp = (c25 - p25) * 100, call25_iv = c25, put25_iv = p25,
            expiration = expiration), source = "live")
}

#' Resolve 25Δ RR. DB option_skew_history first if fresh; else live IBKR.
resolve_skew_25d <- function(ticker, spot, freshness, tws_ok = TRUE,
                              target_dte = 30, conn = NULL) {
  own_conn <- is.null(conn)
  if (own_conn) conn <- tryCatch(Tdata::safe_db_connect(),
                                  error = function(e) NULL)
  on.exit(if (own_conn && !is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  if (!is.null(conn)) {
    row <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT cache_date, call25_iv, put25_iv, skew_25d FROM option_skew_history
       WHERE sym = ? ORDER BY cache_date DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
    if (!is.null(row) && nrow(row) > 0 &&
        !is.na(row$call25_iv) && !is.na(row$put25_iv) &&
        is_fresh(row$cache_date, freshness)) {
      return(.ok(list(
        rr_vp = (row$call25_iv - row$put25_iv) * 100,
        call25_iv = as.numeric(row$call25_iv),
        put25_iv = as.numeric(row$put25_iv),
        expiration = NA_character_),
        source = "db", retrieved_at = row$cache_date))
    }
  }
  .live_25d_skew(ticker, spot, target_dte = target_dte, tws_ok = tws_ok)
}

# ── Option bid/ask spread (liquidity) ─────────────────────────────────────

#' Pick the row whose delta is closest to `target` (signed: +0.30 for a 30Δ
#' call, -0.30 for a 30Δ put). Returns a 1-row data.frame or NULL.
.pick_delta_row <- function(df, target) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  d <- suppressWarnings(as.numeric(df$delta))
  ok <- !is.na(d)
  if (!any(ok)) return(NULL)
  cand <- which(ok)
  cand[which.min(abs(d[ok] - target))] |> (\(i) df[i, , drop = FALSE])()
}

#' Pick the row whose strike is closest to `spot`. Returns 1-row df or NULL.
.pick_atm_row <- function(df, spot) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  k <- suppressWarnings(as.numeric(df$strike))
  ok <- !is.na(k)
  if (!any(ok)) return(NULL)
  which(ok)[which.min(abs(k[ok] - spot))] |> (\(i) df[i, , drop = FALSE])()
}

#' Normalized bid/ask spread for one option row = (ask-bid)/mid. Prefers the
#' Python-side `spread` column (= 2*(ask-bid)/(ask+bid), identical to (ask-bid)
#' /mid); falls back to a local recompute from bid/ask. Returns a fraction
#' (e.g. 0.27 for a 27% spread) or NA.
.norm_spread <- function(row) {
  if (is.null(row) || nrow(row) == 0) return(NA_real_)
  sp <- suppressWarnings(as.numeric(row$spread[1]))
  if (!is.na(sp) && sp >= 0) return(sp)
  bid <- suppressWarnings(as.numeric(row$bid[1]))
  ask <- suppressWarnings(as.numeric(row$ask[1]))
  if (is.na(bid) || is.na(ask) || (bid + ask) <= 0) return(NA_real_)
  2 * (ask - bid) / (ask + bid)
}

#' Flatten one option row into the fields the report needs.
.spread_grab <- function(row) {
  list(
    strike = if (is.null(row)) NA_real_ else suppressWarnings(as.numeric(row$strike[1])),
    spread = .norm_spread(row),
    bid    = if (is.null(row)) NA_real_ else suppressWarnings(as.numeric(row$bid[1])),
    ask    = if (is.null(row)) NA_real_ else suppressWarnings(as.numeric(row$ask[1])),
    delta  = if (is.null(row)) NA_real_ else suppressWarnings(as.numeric(row$delta[1])))
}

#' Rough current IV30 for locating the 30Δ wings — cheap DB read of the latest
#' Prices.iv30 (a fraction, e.g. 0.18). Falls back to 0.30 when absent. Only
#' used to *place* the strike search; the final 30Δ pick is by actual delta.
.rough_iv30 <- function(ticker, default = 0.30) {
  conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
  if (is.null(conn)) return(default)
  on.exit(DBI::dbDisconnect(conn))
  v <- tryCatch(DBI::dbGetQuery(conn,
    "SELECT iv30 FROM Prices WHERE sym = ? AND iv30 IS NOT NULL ORDER BY ROWID DESC LIMIT 1",
    params = list(ticker))$iv30, error = function(e) NULL)
  if (is.null(v) || length(v) == 0 || is.na(v[1]) || v[1] <= 0) return(default)
  as.numeric(v[1])
}

#' Resolve option bid/ask-spread liquidity at one expiry near `target_dte`.
#' Probes the ATM strike (call + put) and the ~30Δ call/put wings, computing
#' the normalized spread (ask-bid)/mid for each. Live IBKR only — option quotes
#' have no DB-cache path here. force_refresh=TRUE (like .live_atm_iv /
#' .live_25d_skew): the parquet quote cache can hold rows fetched by paths that
#' left bid/ask NaN (e.g. chain-OI scans), which would yield a spurious "no
#' bid/ask spread" NO DATA — a spread probe must pull the live quote.
#'
#' Returns .ok(list(expiration, dte, atm_strike, atm_call, atm_put, c30, p30,
#'   atm_bid_ask_pct)) where atm_*/c30/p30 are .spread_grab() lists (spreads are
#'   fractions) and atm_bid_ask_pct is the mean ATM call/put spread in PERCENT
#'   (for the vehicle rule). .miss / .nodata on failure.
resolve_option_spread <- function(ticker, spot, target_dte = 45, tws_ok = TRUE) {
  if (!isTRUE(tws_ok))
    return(.miss("TWS not reachable; cannot probe option bid/ask spreads"))
  py <- .tdata_py()
  if (is.null(py))
    return(.miss("tdata_py unavailable; cannot probe option bid/ask spreads"))
  if (is.na(spot) || spot <= 0) return(.miss("spot price unavailable"))

  expiries <- tryCatch(py$getExpirationDates(ticker),
                       error = function(e) conditionMessage(e))
  if (is.character(expiries) && length(expiries) == 1)
    return(.miss(paste("getExpirationDates:", expiries)))
  if (is.null(expiries) || length(expiries) == 0)
    return(.miss("no expirations from IBKR"))
  expiration <- .pick_expiry_for_dte(expiries, target_dte)
  if (is.na(expiration))
    return(.miss(sprintf("no expiry near %dd DTE", target_dte)))
  dte <- tryCatch(as.integer(as.Date(expiration, "%Y%m%d") - Sys.Date()),
                  error = function(e) NA_integer_)

  # Locate the ATM + ~30Δ wings analytically (rough IV from DB, 0.30 fallback)
  # instead of fetching the whole chain. The final 30Δ pick is still by actual
  # delta from the fetched rows; the estimate only places the strike search.
  sigma <- .rough_iv30(ticker)
  Tyr   <- (if (is.na(dte)) 45L else dte) / 365
  ssT   <- sigma * sqrt(Tyr)
  drift <- 0.5 * sigma^2 * Tyr
  k_c30 <- spot * exp( 0.524 * ssT + drift)   # 30Δ call (OTM, above spot)
  k_p30 <- spot * exp(-0.524 * ssT + drift)   # 30Δ put  (OTM, below spot)

  # Qualify a band just wide enough to span both wings (+2% buffer); cached per
  # expiry after the first run. Far tighter than the old ±35%.
  span <- max(abs(k_c30 / spot - 1), abs(1 - k_p30 / spot)) + 0.02
  all_strikes <- tryCatch(py$getStrikesInRange(
    sym = ticker, expiration = expiration,
    center_strike = spot, range_pct = span),
    error = function(e) conditionMessage(e))
  if (is.character(all_strikes) && length(all_strikes) == 1)
    return(.miss(paste("getStrikesInRange:", all_strikes)))
  all_strikes <- sort(as.numeric(unlist(all_strikes)))
  all_strikes <- all_strikes[!is.na(all_strikes)]
  if (length(all_strikes) == 0) return(.miss("no strikes from IBKR"))

  # Price ONLY the ATM + the two wing neighborhoods (±1 strike each). Quotes are
  # force-refreshed, so fetching ~6-9 strikes instead of the whole chain is the
  # main recurring saving.
  .near <- function(center)
    Tbasics::get_nearest_values(all_strikes, center, n_below = 1, n_above = 1)
  strikes <- sort(unique(c(.near(spot), .near(k_c30), .near(k_p30))))

  fetch_df <- function(right) {
    tryCatch(py$getOptValue(
      sym = ticker, expiration = expiration,
      strikes = as.list(strikes), right = right,
      force_refresh = TRUE),
      error = function(e) NULL)
  }
  df_c <- fetch_df("C"); df_p <- fetch_df("P")
  if ((is.null(df_c) || nrow(df_c) == 0) && (is.null(df_p) || nrow(df_p) == 0))
    return(.nodata(sprintf("getOptValue empty for both wings on %s", expiration)))

  atm_call <- .spread_grab(.pick_atm_row(df_c, spot))
  atm_put  <- .spread_grab(.pick_atm_row(df_p, spot))
  c30      <- .spread_grab(.pick_delta_row(df_c,  0.30))
  p30      <- .spread_grab(.pick_delta_row(df_p, -0.30))

  all_spreads <- c(atm_call$spread, atm_put$spread, c30$spread, p30$spread)
  if (all(is.na(all_spreads)))
    return(.nodata(sprintf("quotes returned but no bid/ask spread on %s",
                           expiration)))

  atm_spreads <- c(atm_call$spread, atm_put$spread)
  atm_spreads <- atm_spreads[!is.na(atm_spreads)]
  atm_bid_ask_pct <- if (length(atm_spreads) > 0)
    round(mean(atm_spreads) * 100, 1) else NA_real_
  atm_strike <- if (!is.na(atm_call$strike)) atm_call$strike else atm_put$strike

  .ok(list(expiration = expiration, dte = dte, atm_strike = atm_strike,
           atm_call = atm_call, atm_put = atm_put, c30 = c30, p30 = p30,
           atm_bid_ask_pct = atm_bid_ask_pct),
      source = "live")
}

# ── Chain OI ──────────────────────────────────────────────────────────────

#' Reduce a per-strike/per-right OI table into oi_cap_call, oi_cap_put,
#' and a chain_state label.
#'
#' OTM-only filter: oi_cap_call is the max-OI strike *above* spot (real
#' resistance from dealer hedging); ITM call OI is stock-replacement /
#' covered-call cover with no pin dynamic. Mirror for puts below spot.
#'
#' Thin-chain bypass: if the max OTM OI on a side falls below
#' thin_oi_threshold, that side's cap is NA — too sparse to read as
#' resistance/support. chain_state = "thin" when both sides bypass.
.summarize_oi <- function(oi_rows, spot, source = "db",
                           thin_oi_threshold = 100L) {
  oi_rows$open_interest <- suppressWarnings(as.numeric(oi_rows$open_interest))
  oi_rows <- oi_rows[!is.na(oi_rows$open_interest) & oi_rows$open_interest > 0, ]
  if (nrow(oi_rows) == 0)
    return(.nodata(sprintf("%s: OI rows arrived but all 0/NaN", source)))
  if (is.na(spot) || !is.finite(spot)) {
    calls <- oi_rows[FALSE, , drop = FALSE]
    puts  <- oi_rows[FALSE, , drop = FALSE]
  } else {
    calls <- oi_rows[oi_rows$right == "C" & oi_rows$strike > spot, , drop = FALSE]
    puts  <- oi_rows[oi_rows$right == "P" & oi_rows$strike < spot, , drop = FALSE]
  }
  oi_cap_call <- if (nrow(calls) > 0 &&
                     max(calls$open_interest, na.rm = TRUE) >= thin_oi_threshold)
    calls$strike[which.max(calls$open_interest)] else NA_real_
  oi_cap_put  <- if (nrow(puts) > 0 &&
                     max(puts$open_interest, na.rm = TRUE) >= thin_oi_threshold)
    puts$strike[which.max(puts$open_interest)] else NA_real_
  otm_oi <- rbind(calls, puts)
  total_oi <- if (nrow(otm_oi) > 0) sum(otm_oi$open_interest) else 0
  top_n <- min(3L, nrow(otm_oi))
  top3 <- if (top_n > 0)
    sum(sort(otm_oi$open_interest, decreasing = TRUE)[1:top_n], na.rm = TRUE)
    else 0
  conc <- if (total_oi > 0) top3 / total_oi else NA_real_
  state <- if (is.na(oi_cap_call) && is.na(oi_cap_put)) "thin"
           else if (is.na(conc)) "open"
           else if (conc >= 0.6) "chain-capped"
           else if (conc >= 0.4) "crowded"
           else "open"
  .ok(list(oi_cap_call = oi_cap_call, oi_cap_put = oi_cap_put,
            chain_state = state), source = source)
}

#' Resolve chain OI cap + state for one expiry. DB option_chain_oi_history
#' first if fresh; else live get_chain_oi.
resolve_chain_oi <- function(ticker, expiry, spot, freshness, tws_ok = TRUE,
                              conn = NULL, thin_oi_threshold = 100L) {
  own_conn <- is.null(conn)
  if (own_conn) conn <- tryCatch(Tdata::safe_db_connect(),
                                  error = function(e) NULL)
  on.exit(if (own_conn && !is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  if (is.na(expiry) || !nzchar(expiry))
    return(.miss("expiry unavailable — cannot resolve chain"))
  if (!is.null(conn)) {
    oi_rows <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT strike, right, open_interest, cache_date FROM option_chain_oi_history
       WHERE sym = ? AND expiry = ?
       ORDER BY cache_date DESC", params = list(ticker, expiry)),
      error = function(e) NULL)
    if (!is.null(oi_rows) && nrow(oi_rows) > 0) {
      latest <- max(oi_rows$cache_date, na.rm = TRUE)
      if (is_fresh(latest, freshness)) {
        return(.summarize_oi(oi_rows, spot = spot, source = "db",
                              thin_oi_threshold = thin_oi_threshold))
      }
    }
  }
  if (!isTRUE(tws_ok))
    return(.miss("DB option_chain_oi_history NA/stale; TWS not reachable"))
  py <- .tdata_py()
  if (is.null(py)) return(.miss("DB option_chain_oi_history NA/stale; tdata_py unavailable"))
  if (is.na(spot)) return(.miss("missing spot — cannot pull live OI"))
  # ±12% OI band. OI walls that can cap a directional target sit within ~±10%
  # for a 30-60 DTE swing; a far-OTM lottery wall beyond that isn't a relevant
  # cap. ±25% just qualified/priced ~212 strikes (genericTickList=101 per strike).
  smin <- spot * 0.88; smax <- spot * 1.12
  live_oi <- tryCatch(py$get_chain_oi(
    sym = ticker, expiration = expiry,
    strike_min = smin, strike_max = smax),
    error = function(e) conditionMessage(e))
  if (is.character(live_oi) && length(live_oi) == 1)
    return(.miss(paste("get_chain_oi:", live_oi)))
  if (is.null(live_oi) || !is.data.frame(live_oi) || nrow(live_oi) == 0)
    return(.nodata(sprintf("get_chain_oi: request OK but no rows for %s @ %s",
                           ticker, expiry)))
  .summarize_oi(live_oi, spot = spot, source = "live",
                 thin_oi_threshold = thin_oi_threshold)
}

# ── Earnings ─────────────────────────────────────────────────────────────

# ── Sector RS context ────────────────────────────────────────────────────

#' Live ret20/ret60 for any ticker via fetch_single_ohlcv + calc_ind. Used
#' for sector ETFs and SPY in the Phase B sector-RS context. Returns
#' list(ret20, ret60, source, reason) with NA values + reason on failure.
resolve_returns <- function(ticker) {
  raw <- tryCatch(fetch_single_ohlcv(ticker), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) < 70)
    return(.miss(sprintf("fetch_single_ohlcv: <70 rows for %s", ticker)))
  ind <- tryCatch(calc_ind(raw), error = function(e) NULL)
  if (is.null(ind) || nrow(ind) == 0)
    return(.miss(sprintf("calc_ind returned empty for %s", ticker)))
  last <- tail(ind, 1)
  ret20 <- as.numeric(last$ret20)
  ret60 <- as.numeric(last$ret60)
  if (is.na(ret20) && is.na(ret60))
    return(.miss(sprintf("ret20/ret60 both NA for %s", ticker)))
  .ok(list(ret20 = ret20, ret60 = ret60), source = "live")
}

#' Compute Phase B sector-RS context for a single ticker.
#'
#' Returns:
#'   - sector / etf_sym: ticker's sector + sector ETF
#'   - stock_ret20 / stock_ret60: ticker's own returns
#'   - etf_ret20  / etf_ret60:    sector ETF returns
#'   - spy_ret20  / spy_ret60:    SPY returns
#'   - rs_vs_sector_20d / 60d:    stock_ret - etf_ret (leader-vs-laggard)
#'   - sector_rs_vs_spy_20d / 60d: etf_ret - spy_ret  (strong-vs-weak sector)
#'   - sector_rank: rank among all sectors of (etf_ret20 - spy_ret20),
#'     direction-aware: descending for long (rank 1 = strongest), ascending
#'     for short (rank 1 = weakest).
#'   - n_sectors: total sector count walked.
#'
#' Walks every sector ETF (~11 yfinance fetches; ~5-15s on first run).
compute_sector_rs_context <- function(ticker, direction,
                                       stock_ret20 = NA_real_,
                                       stock_ret60 = NA_real_) {
  sec_r <- resolve_sector(ticker)
  if (is.na(sec_r$value))
    return(list(sector = NA_character_, etf_sym = NA_character_,
                stock_ret20 = stock_ret20, stock_ret60 = stock_ret60,
                etf_ret20 = NA_real_, etf_ret60 = NA_real_,
                spy_ret20 = NA_real_, spy_ret60 = NA_real_,
                rs_vs_sector_20d = NA_real_, rs_vs_sector_60d = NA_real_,
                sector_rs_vs_spy_20d = NA_real_, sector_rs_vs_spy_60d = NA_real_,
                sector_rank = NA_integer_, n_sectors = NA_integer_,
                source = "unavailable", reason = sec_r$reason))
  sector <- sec_r$value

  etf_r <- resolve_sector_etf(ticker)
  etf_sym <- if (is.na(etf_r$value)) NA_character_ else etf_r$value

  # If we don't have a stock return passed in, fetch it
  if (is.na(stock_ret20) || is.na(stock_ret60)) {
    sr <- resolve_returns(ticker)
    if (is.list(sr$value)) {
      stock_ret20 <- sr$value$ret20
      stock_ret60 <- sr$value$ret60
    }
  }

  # Ticker's own sector ETF
  this_etf_ret20 <- NA_real_; this_etf_ret60 <- NA_real_
  if (!is.na(etf_sym)) {
    er <- resolve_returns(etf_sym)
    if (is.list(er$value)) {
      this_etf_ret20 <- er$value$ret20
      this_etf_ret60 <- er$value$ret60
    }
  }

  # SPY (benchmark)
  spy_r <- resolve_returns("SPY")
  spy_ret20 <- if (is.list(spy_r$value)) spy_r$value$ret20 else NA_real_
  spy_ret60 <- if (is.list(spy_r$value)) spy_r$value$ret60 else NA_real_

  # Walk all sectors for rank
  all_etfs <- tryCatch(get_sector_etfs(), error = function(e) NULL)
  sector_rank <- NA_integer_; n_sectors <- NA_integer_
  if (!is.null(all_etfs) && length(all_etfs) > 0 && !is.na(spy_ret20)) {
    rs_per_sector <- vapply(unname(all_etfs), function(e) {
      r <- resolve_returns(e)
      if (is.list(r$value) && !is.na(r$value$ret20)) {
        r$value$ret20 - spy_ret20
      } else NA_real_
    }, numeric(1))
    names(rs_per_sector) <- names(all_etfs)
    rs_per_sector <- rs_per_sector[!is.na(rs_per_sector)]
    n_sectors <- length(rs_per_sector)
    if (n_sectors > 0 && sector %in% names(rs_per_sector)) {
      if (direction == "long") {
        ranks <- rank(-rs_per_sector, ties.method = "first")  # descending
      } else {
        ranks <- rank(rs_per_sector, ties.method = "first")   # ascending
      }
      sector_rank <- as.integer(ranks[sector])
    }
  }

  rs_vs_sector_20d <- if (!is.na(stock_ret20) && !is.na(this_etf_ret20))
                        round(stock_ret20 - this_etf_ret20, 2) else NA_real_
  rs_vs_sector_60d <- if (!is.na(stock_ret60) && !is.na(this_etf_ret60))
                        round(stock_ret60 - this_etf_ret60, 2) else NA_real_
  sector_rs_vs_spy_20d <- if (!is.na(this_etf_ret20) && !is.na(spy_ret20))
                            round(this_etf_ret20 - spy_ret20, 2) else NA_real_
  sector_rs_vs_spy_60d <- if (!is.na(this_etf_ret60) && !is.na(spy_ret60))
                            round(this_etf_ret60 - spy_ret60, 2) else NA_real_

  list(
    sector = sector, etf_sym = etf_sym,
    stock_ret20 = stock_ret20, stock_ret60 = stock_ret60,
    etf_ret20 = this_etf_ret20, etf_ret60 = this_etf_ret60,
    spy_ret20 = spy_ret20, spy_ret60 = spy_ret60,
    rs_vs_sector_20d = rs_vs_sector_20d,
    rs_vs_sector_60d = rs_vs_sector_60d,
    sector_rs_vs_spy_20d = sector_rs_vs_spy_20d,
    sector_rs_vs_spy_60d = sector_rs_vs_spy_60d,
    sector_rank = sector_rank, n_sectors = n_sectors,
    source = "live", reason = NULL
  )
}

# ── Earnings ─────────────────────────────────────────────────────────────

#' Resolve next earnings date and DTE via Tdata::getNextEarningsDate.
resolve_earnings <- function(ticker) {
  ed_raw <- tryCatch(Tdata::getNextEarningsDate(ticker),
                     error = function(e) NA)
  ed <- if (inherits(ed_raw, "Date")) ed_raw
        else if (is.character(ed_raw) && nzchar(ed_raw) && !is.na(ed_raw)) {
          tryCatch(as.Date(ed_raw, format = "%Y%m%d"),
                   error = function(e) NA)
        } else NA
  if (!inherits(ed, "Date") || is.na(ed))
    return(.miss("getNextEarningsDate returned no date (likely yfinance unreachable)"))
  dte <- as.integer(ed - Sys.Date())
  .ok(list(date = ed, dte = dte), source = "live")
}
