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

#' Standard return-shape constructor.
.ok <- function(value, source, retrieved_at = Sys.time(), reason = NULL) {
  list(value = value, source = source,
       retrieved_at = retrieved_at, reason = reason)
}
.miss <- function(reason, source = "unavailable") {
  list(value = NA, source = source,
       retrieved_at = Sys.time(), reason = reason)
}

#' Pick the IBKR expiration whose DTE is closest to `target_dte`. Filters out
#' past expirations. Returns YYYYMMDD string or NA.
.pick_expiry_for_dte <- function(expiries, target_dte) {
  if (is.null(expiries) || length(expiries) == 0) return(NA_character_)
  exp_dates <- as.Date(as.character(expiries), format = "%Y%m%d")
  dtes <- as.integer(exp_dates - Sys.Date())
  ok <- !is.na(dtes) & dtes > 0
  if (!any(ok)) return(NA_character_)
  expiries <- expiries[ok]; dtes <- dtes[ok]
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

  strikes <- tryCatch(py$getStrikesAuto(
    sym = ticker, expiration = expiration,
    center_strike = spot, range_pct = 0.1),
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
    return(.miss(sprintf("getOptValue returned no IV for %s @ %s",
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
    hist <- yf$Ticker(ticker)$history(period = "60d", interval = "1d")
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
    return(.miss("getIVPercentileLevels: no current_iv"))
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
  res <- tryCatch(Tdata::getVolMetrics(ticker),
                  error = function(e) NULL)
  if (is.null(res) || !is.data.frame(res) || nrow(res) == 0)
    return(.miss("getVolMetrics returned empty"))
  rvp <- suppressWarnings(as.numeric(res$rvp[1]))
  if (is.na(rvp)) return(.miss("getVolMetrics: rvp is NA"))
  .ok(round(rvp, 1), source = "live",
      reason = sprintf("from getVolMetrics (RV30=%.1f%%)",
                       suppressWarnings(as.numeric(res$rv30[1])) * 100))
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

  strikes <- tryCatch(py$getStrikesAuto(
    sym = ticker, expiration = expiration,
    center_strike = spot, range_pct = 0.25),
    error = function(e) conditionMessage(e))
  if (is.character(strikes) && length(strikes) == 1)
    return(.miss(paste("getStrikesAuto:", strikes)))
  if (is.null(strikes) || length(strikes) == 0)
    return(.miss("no strikes from IBKR"))
  strikes <- as.numeric(unlist(strikes))
  strikes <- strikes[!is.na(strikes)]
  if (length(strikes) == 0) return(.miss("all strikes were NaN"))

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
    return(.miss(sprintf("getOptValue empty for both wings on %s", expiration)))

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
    return(.miss("no 25Δ strike with IV/delta from IBKR"))
  if (is.na(c25) || is.na(p25))
    return(.miss(sprintf("only one wing available (call25=%s, put25=%s)",
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

# ── Chain OI ──────────────────────────────────────────────────────────────

#' Reduce a per-strike/per-right OI table into oi_cap_call, oi_cap_put,
#' and a chain_state label.
.summarize_oi <- function(oi_rows, source = "db") {
  oi_rows$open_interest <- suppressWarnings(as.numeric(oi_rows$open_interest))
  oi_rows <- oi_rows[!is.na(oi_rows$open_interest) & oi_rows$open_interest > 0, ]
  if (nrow(oi_rows) == 0)
    return(.miss(sprintf("%s: all OI rows empty/zero", source)))
  calls <- oi_rows[oi_rows$right == "C", , drop = FALSE]
  puts  <- oi_rows[oi_rows$right == "P", , drop = FALSE]
  oi_cap_call <- if (nrow(calls) > 0)
    calls$strike[which.max(calls$open_interest)] else NA_real_
  oi_cap_put  <- if (nrow(puts)  > 0)
    puts$strike[which.max(puts$open_interest)]   else NA_real_
  total_oi <- sum(oi_rows$open_interest)
  top3 <- sum(sort(oi_rows$open_interest, decreasing = TRUE)[1:3], na.rm = TRUE)
  conc <- if (total_oi > 0) top3 / total_oi else NA_real_
  state <- if (is.na(conc)) "open"
           else if (conc >= 0.6) "chain-capped"
           else if (conc >= 0.4) "crowded"
           else "open"
  .ok(list(oi_cap_call = oi_cap_call, oi_cap_put = oi_cap_put,
            chain_state = state), source = source)
}

#' Resolve chain OI cap + state for one expiry. DB option_chain_oi_history
#' first if fresh; else live get_chain_oi.
resolve_chain_oi <- function(ticker, expiry, spot, freshness, tws_ok = TRUE,
                              conn = NULL) {
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
        return(.summarize_oi(oi_rows, source = "db"))
      }
    }
  }
  if (!isTRUE(tws_ok))
    return(.miss("DB option_chain_oi_history NA/stale; TWS not reachable"))
  py <- .tdata_py()
  if (is.null(py)) return(.miss("DB option_chain_oi_history NA/stale; tdata_py unavailable"))
  if (is.na(spot)) return(.miss("missing spot — cannot pull live OI"))
  smin <- spot * 0.75; smax <- spot * 1.25
  live_oi <- tryCatch(py$get_chain_oi(
    sym = ticker, expiration = expiry,
    strike_min = smin, strike_max = smax),
    error = function(e) conditionMessage(e))
  if (is.character(live_oi) && length(live_oi) == 1)
    return(.miss(paste("get_chain_oi:", live_oi)))
  if (is.null(live_oi) || !is.data.frame(live_oi) || nrow(live_oi) == 0)
    return(.miss(sprintf("get_chain_oi: no rows for %s @ %s", ticker, expiry)))
  .summarize_oi(live_oi, source = "live")
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
