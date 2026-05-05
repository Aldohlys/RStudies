# reports/analyze/funnel.R — Phase C.2 Directional Vol Funnel.
#
# Pure measurement: tabulates IV landscape, VRP (both forms), term shape,
# skew/RR, earnings. Produces a 6-row grid + signal tally for the user's
# direction. Does NOT modify any conviction (there is no conviction output).
#
# Live-fetch policy (see feedback_analyze_live_data_fallback.md):
#   DB cache first. If NA / stale, live-pull from IBKR via tdata_py
#   (getExpirationDates / getStrikesAuto / getOptValue, force_refresh=TRUE).
#   If the live fetch also fails, surface "FETCH FAILED: <cause>" in the
#   cell — never an opaque n/a.

# ── Mechanical labels (no prescriptive language) ──────────────────────────
.regime_label <- function(ivp, cfg) {
  if (is.na(ivp)) return("FETCH FAILED")
  if (ivp < cfg$ivp_regime$cheap_max) "cheap"
  else if (ivp > cfg$ivp_regime$rich_min) "rich"
  else "mid"
}

.vrp_log_band <- function(vrp_log, cfg) {
  if (is.na(vrp_log)) return("FETCH FAILED")
  b <- cfg$vrp_log_bands
  if (vrp_log <= b$neg_max) "negative (IV<RV)"
  else if (vrp_log <= b$mild_max) "mildly positive"
  else if (vrp_log <= b$moderate_max) "moderately positive"
  else if (vrp_log <= b$strong_max) "strongly positive"
  else "very strongly positive"
}

.term_shape <- function(iv_by_dte) {
  ok <- !is.na(iv_by_dte) & iv_by_dte > 0
  if (sum(ok) < 2) return("FETCH FAILED")
  v <- iv_by_dte[ok]
  d <- diff(v)
  if (all(d >= -0.5) && (tail(v, 1) - v[1]) > 1) "contango"
  else if (all(d <= 0.5) && (v[1] - tail(v, 1)) > 1) "backwardation"
  else if (length(v) >= 3 && which.max(v) %in% c(1, length(v)) &&
           min(v) < min(v[c(1, length(v))])) "U-shape"
  else if (length(v) >= 3 && which.max(v) %in% 2:(length(v)-1)) "hump"
  else "flat"
}

.rr_label <- function(rr) {
  if (is.na(rr)) "FETCH FAILED"
  else if (abs(rr) < 1) "flat"
  else if (rr > 0) "calls bid (positive RR)"
  else "puts bid (negative RR)"
}

# ── Live IBKR helpers ─────────────────────────────────────────────────────
# Each helper returns list(value=..., reason=NULL) on success, or
# list(value=NA_real_, reason="<cause>") on failure. Reasons get propagated
# to the report as "FETCH FAILED: <cause>" instead of n/a.

.tdata_py <- function() {
  tryCatch(Tdata:::tdata_py, error = function(e) NULL)
}

.live_spot <- function(ticker) {
  p <- tryCatch(Tdata::getLastSymPrice(ticker), error = function(e) NULL)
  if (is.null(p) || length(p) == 0) return(NA_real_)
  if (is.data.frame(p)) {
    cand <- intersect(c("price", "Close", "close", "last", "value"), names(p))
    if (length(cand) > 0) p <- p[[cand[1]]]
    else {
      num_cols <- which(sapply(p, is.numeric))
      p <- if (length(num_cols) > 0) p[[tail(num_cols, 1)]] else p[[1]]
    }
  }
  as.numeric(p)[1]
}

#' Pick the IBKR expiration whose DTE is closest to `target_dte`.
#' Returns YYYYMMDD string or NA. `expiries` is the list returned by
#' getExpirationDates().
.pick_expiry_for_dte <- function(expiries, target_dte) {
  if (is.null(expiries) || length(expiries) == 0) return(NA_character_)
  exp_dates <- as.Date(as.character(expiries), format = "%Y%m%d")
  dtes <- as.integer(exp_dates - Sys.Date())
  dtes <- dtes[!is.na(dtes) & dtes > 0]
  if (length(dtes) == 0) return(NA_character_)
  expiries <- expiries[match(dtes, as.integer(
    as.Date(as.character(expiries), format = "%Y%m%d") - Sys.Date()))]
  expiries[which.min(abs(dtes - target_dte))]
}

#' Fetch ATM IV at a given target DTE, live from IBKR.
#' Returns list(iv = % volatility (e.g. 32.5 for 32.5%), expiration = YYYYMMDD,
#' reason = NULL on success / "<cause>" on failure).
.live_atm_iv <- function(ticker, spot, target_dte) {
  py <- .tdata_py()
  if (is.null(py)) return(list(iv = NA_real_, expiration = NA_character_,
                               reason = "tdata_py module unavailable"))
  if (is.na(spot)) return(list(iv = NA_real_, expiration = NA_character_,
                               reason = "spot price unavailable"))
  expiries <- tryCatch(py$getExpirationDates(ticker),
                       error = function(e) conditionMessage(e))
  if (is.character(expiries) && length(expiries) == 1) {
    return(list(iv = NA_real_, expiration = NA_character_,
                reason = paste("getExpirationDates:", expiries)))
  }
  if (is.null(expiries) || length(expiries) == 0) {
    return(list(iv = NA_real_, expiration = NA_character_,
                reason = "no expirations from IBKR"))
  }
  expiration <- .pick_expiry_for_dte(expiries, target_dte)
  if (is.na(expiration)) return(list(
    iv = NA_real_, expiration = NA_character_,
    reason = sprintf("no expiry close to %dd DTE", target_dte)))

  # Get strikes within ±10% of spot (covers ATM) using auto trading class
  strikes <- tryCatch(py$getStrikesAuto(
    sym = ticker, expiration = expiration,
    center_strike = spot, range_pct = 0.1),
    error = function(e) conditionMessage(e))
  if (is.character(strikes) && length(strikes) == 1) {
    return(list(iv = NA_real_, expiration = expiration,
                reason = paste("getStrikesAuto:", strikes)))
  }
  if (is.null(strikes) || length(strikes) == 0) {
    return(list(iv = NA_real_, expiration = expiration,
                reason = "no strikes near spot from IBKR"))
  }
  strikes <- as.numeric(unlist(strikes))
  strikes <- strikes[!is.na(strikes)]
  if (length(strikes) == 0) return(list(
    iv = NA_real_, expiration = expiration,
    reason = "all strikes from IBKR were NaN"))

  atm_strike <- strikes[which.min(abs(strikes - spot))]

  # Average call + put IV at ATM strike (force_refresh per analyze policy)
  fetch <- function(right) {
    df <- tryCatch(py$getOptValue(
      sym = ticker, expiration = expiration,
      strikes = list(atm_strike), right = right,
      force_refresh = TRUE),
      error = function(e) NULL)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
    iv <- df$impliedvol[1]
    if (is.null(iv) || is.na(iv) || iv <= 0) return(NA_real_)
    as.numeric(iv)  # fraction (e.g. 0.36)
  }
  iv_c <- fetch("C"); iv_p <- fetch("P")
  ivs <- c(iv_c, iv_p); ivs <- ivs[!is.na(ivs)]
  if (length(ivs) == 0) return(list(
    iv = NA_real_, expiration = expiration,
    reason = sprintf("getOptValue returned no IV for %s @ %s", atm_strike, expiration)))
  # IV returned as fraction (e.g. 0.36) to match DB Prices.iv30 scale.
  list(iv = mean(ivs), expiration = expiration, reason = NULL)
}

#' Fetch 25-delta call/put IV (and RR in vol-points) live from IBKR.
#' Returns list(rr_vp, call25_iv, put25_iv, expiration, reason).
.live_25d_skew <- function(ticker, spot, target_dte = 30) {
  py <- .tdata_py()
  if (is.null(py)) return(list(rr_vp = NA_real_, call25_iv = NA_real_,
                               put25_iv = NA_real_, expiration = NA_character_,
                               reason = "tdata_py module unavailable"))
  if (is.na(spot)) return(list(rr_vp = NA_real_, call25_iv = NA_real_,
                               put25_iv = NA_real_, expiration = NA_character_,
                               reason = "spot price unavailable"))

  expiries <- tryCatch(py$getExpirationDates(ticker),
                       error = function(e) conditionMessage(e))
  if (is.character(expiries) && length(expiries) == 1) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = NA_character_, reason = paste("getExpirationDates:", expiries)))
  if (is.null(expiries) || length(expiries) == 0) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = NA_character_, reason = "no expirations from IBKR"))

  expiration <- .pick_expiry_for_dte(expiries, target_dte)
  if (is.na(expiration)) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = NA_character_,
    reason = sprintf("no expiry near %dd DTE", target_dte)))

  # Need wider strike range for 25Δ wings — use ±25%
  strikes <- tryCatch(py$getStrikesAuto(
    sym = ticker, expiration = expiration,
    center_strike = spot, range_pct = 0.25),
    error = function(e) conditionMessage(e))
  if (is.character(strikes) && length(strikes) == 1) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = expiration, reason = paste("getStrikesAuto:", strikes)))
  if (is.null(strikes) || length(strikes) == 0) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = expiration, reason = "no strikes from IBKR"))

  strikes <- as.numeric(unlist(strikes))
  strikes <- strikes[!is.na(strikes)]
  if (length(strikes) == 0) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = expiration, reason = "all strikes were NaN"))

  fetch_df <- function(right) {
    tryCatch(py$getOptValue(
      sym = ticker, expiration = expiration,
      strikes = as.list(strikes), right = right,
      force_refresh = TRUE),
      error = function(e) NULL)
  }
  df_c <- fetch_df("C"); df_p <- fetch_df("P")
  if ((is.null(df_c) || nrow(df_c) == 0) &&
      (is.null(df_p) || nrow(df_p) == 0)) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = expiration,
    reason = sprintf("getOptValue returned empty for both wings on %s", expiration)))

  pick_25d <- function(df, want_sign) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
    delt <- as.numeric(df$delta); iv <- as.numeric(df$impliedvol)
    ok <- !is.na(delt) & !is.na(iv) & iv > 0
    if (sum(ok) == 0) return(NA_real_)
    delt <- delt[ok]; iv <- iv[ok]
    target <- 0.25 * want_sign  # +0.25 for calls, -0.25 for puts
    iv[which.min(abs(delt - target))]  # fraction
  }
  c25 <- pick_25d(df_c,  1)
  p25 <- pick_25d(df_p, -1)
  if (is.na(c25) && is.na(p25)) return(list(
    rr_vp = NA_real_, call25_iv = NA_real_, put25_iv = NA_real_,
    expiration = expiration,
    reason = "no 25Δ strike with IV/delta from IBKR"))
  if (is.na(c25) || is.na(p25)) return(list(
    rr_vp = NA_real_, call25_iv = c25, put25_iv = p25, expiration = expiration,
    reason = sprintf("only one wing available (call25=%s, put25=%s)",
                     ifelse(is.na(c25), "n/a", sprintf("%.4f", c25)),
                     ifelse(is.na(p25), "n/a", sprintf("%.4f", p25)))))
  # Returned in vol-points (consistent with DB-path: (call25_iv - put25_iv) * 100).
  list(rr_vp = (c25 - p25) * 100, call25_iv = c25, put25_iv = p25,
       expiration = expiration, reason = NULL)
}

#' Solve realized vol from yfinance daily closes (last 30 sessions).
.live_rv30 <- function(ticker) {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(list(
    rv = NA_real_, reason = "reticulate unavailable"))
  res <- tryCatch({
    yf <- reticulate::import("yfinance", delay_load = TRUE)
    hist <- yf$Ticker(ticker)$history(period = "60d", interval = "1d")
    closes <- as.numeric(hist$Close)
    closes <- closes[!is.na(closes) & closes > 0]
    if (length(closes) < 21) stop("yfinance returned <21 closes")
    rets <- diff(log(tail(closes, 31)))
    # RV returned as fraction (e.g. 0.32) to match DB Prices.rv30 scale.
    list(rv = sd(rets) * sqrt(252), reason = NULL)
  }, error = function(e) list(rv = NA_real_, reason = paste("yfinance:", conditionMessage(e))))
  res
}

# ── Funnel data pull ──────────────────────────────────────────────────────
run_funnel_deep_dive <- function(ticker, direction, scanner_row, config,
                                 spot = NA_real_, freshness = NULL) {
  conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
  on.exit(if (!is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)

  # Pull most-recent Prices snapshot for IV/RV
  prices_row <- if (!is.null(conn)) {
    tryCatch(DBI::dbGetQuery(conn,
      "SELECT datetime, price, iv30, iv90, rv30, vrp, ivr, ivp, ivp_2y
       FROM Prices WHERE sym = ? ORDER BY ROWID DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
  } else NULL
  has_prices <- !is.null(prices_row) && nrow(prices_row) > 0

  # Apply freshness gate: if Prices.datetime older than policy, treat as NA
  prices_fresh <- has_prices && (is.null(freshness) ||
                                 is_fresh(prices_row$datetime, freshness))
  prices_age_h <- if (has_prices) round(hours_since(prices_row$datetime), 1) else NA_real_
  stale_reason <- if (has_prices && !prices_fresh)
                    sprintf("DB Prices stale (%.1fh old)", prices_age_h) else NULL

  iv30 <- if (prices_fresh) as.numeric(prices_row$iv30) else NA_real_
  iv90 <- if (prices_fresh) as.numeric(prices_row$iv90) else NA_real_
  rv30 <- if (prices_fresh) as.numeric(prices_row$rv30) else NA_real_
  ivp_used <- if (!is.null(scanner_row) && !is.na(scanner_row$ivp_used)) as.numeric(scanner_row$ivp_used)
              else if (prices_fresh) as.numeric(prices_row$ivp)
              else NA_real_

  iv30_reason <- if (!is.na(iv30)) NULL else (stale_reason %||% "DB Prices.iv30 NA")
  iv90_reason <- if (!is.na(iv90)) NULL else (stale_reason %||% "DB Prices.iv90 NA")
  rv30_reason <- if (!is.na(rv30)) NULL else (stale_reason %||% "DB Prices.rv30 NA")
  ivp_reason  <- if (!is.na(ivp_used)) NULL
                 else (stale_reason %||% "DB Prices.ivp / scanner ivp_used NA")

  # Live fallback: IV30, IV90 via IBKR
  tws_ok <- isTRUE(config$tws_reachable)
  tws_down_reason <- "TWS not reachable"
  if (is.na(spot)) spot <- if (tws_ok) .live_spot(ticker) else NA_real_
  if (is.na(iv30)) {
    if (tws_ok) {
      live <- .live_atm_iv(ticker, spot, target_dte = 30)
      iv30 <- live$iv; iv30_reason <- live$reason
    } else iv30_reason <- tws_down_reason
  }
  if (is.na(iv90)) {
    if (tws_ok) {
      live <- .live_atm_iv(ticker, spot, target_dte = 90)
      iv90 <- live$iv; iv90_reason <- live$reason
    } else iv90_reason <- tws_down_reason
  }
  # Live fallback: RV30 via yfinance
  if (is.na(rv30)) {
    live <- .live_rv30(ticker)
    rv30 <- live$rv
    rv30_reason <- live$reason
  }

  # VRP — both forms
  vrp_log <- if (!is.na(iv30) && !is.na(rv30) && rv30 > 0)
               log(iv30 / rv30) * 100 else NA_real_
  vrp_vp  <- if (!is.na(iv30) && !is.na(rv30))
               (iv30 - rv30) * 100 else NA_real_
  .join_reasons <- function(...) {
    parts <- Filter(function(x) !is.null(x) && !is.na(x) && nzchar(x),
                    list(...))
    if (length(parts) == 0) NULL else paste(unlist(parts), collapse = "; ")
  }
  vrp_reason <- if (!is.na(vrp_log)) NULL
                else .join_reasons(iv30_reason, rv30_reason)

  # Term ratio (front vs back)
  term_pct <- if (!is.na(iv30) && !is.na(iv90) && iv90 > 0)
                round((iv30 - iv90) / iv90 * 100, 1) else NA_real_
  term_reason <- if (!is.na(term_pct)) NULL
                 else .join_reasons(iv30_reason, iv90_reason)

  # Term shape from Prices alone is binary (front vs back). Mark accordingly.
  term_shape_label <- if (is.na(term_pct)) "FETCH FAILED"
                      else if (term_pct < -2) "contango"
                      else if (term_pct >  2) "backwardation"
                      else "flat"

  # Skew history → RR mechanical read
  skew_row <- if (!is.null(conn)) {
    tryCatch(DBI::dbGetQuery(conn,
      "SELECT cache_date, iv30, iv90, rv30, call25_iv, put25_iv, skew_25d
       FROM option_skew_history WHERE sym = ?
       ORDER BY cache_date DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
  } else NULL
  skew_fresh <- !is.null(skew_row) && nrow(skew_row) > 0 &&
                (is.null(freshness) || is_fresh(skew_row$cache_date, freshness))
  has_skew <- skew_fresh && !is.na(skew_row$call25_iv) && !is.na(skew_row$put25_iv)
  rr_vp <- if (has_skew) (skew_row$call25_iv - skew_row$put25_iv) * 100 else NA_real_
  rr_reason <- if (!is.na(rr_vp)) NULL
               else if (!is.null(skew_row) && nrow(skew_row) > 0 && !skew_fresh)
                 sprintf("DB option_skew_history stale (%.1fh old)",
                         hours_since(skew_row$cache_date))
               else "DB option_skew_history NA"

  # Live fallback: 25Δ skew via IBKR
  if (is.na(rr_vp)) {
    if (tws_ok) {
      live <- .live_25d_skew(ticker, spot, target_dte = 30)
      rr_vp <- live$rr_vp; rr_reason <- live$reason
    } else rr_reason <- tws_down_reason
  }

  # Earnings — getNextEarningsDate may return a Date or a YYYYMMDD string
  ed_raw <- tryCatch(Tdata::getNextEarningsDate(ticker), error = function(e) NA)
  ed <- if (inherits(ed_raw, "Date")) ed_raw
        else if (is.character(ed_raw) && nzchar(ed_raw) && !is.na(ed_raw)) {
          tryCatch(as.Date(ed_raw, format = "%Y%m%d"), error = function(e) NA)
        } else NA
  ed_dte <- if (inherits(ed, "Date") && !is.na(ed))
              as.integer(ed - Sys.Date()) else NA_integer_
  ed_reason <- if (!is.na(ed_dte)) NULL
               else "getNextEarningsDate returned no date (likely yfinance unreachable)"

  # ── Build funnel grid (6 rows, mechanical) ──────────────────────────────
  regime <- .regime_label(ivp_used, config)
  vrp_band <- .vrp_log_band(vrp_log, config)
  rr_lab <- .rr_label(rr_vp)

  .with_reason <- function(value, reason, fmt) {
    if (!is.na(value)) sprintf(fmt, value)
    else if (!is.null(reason) && nzchar(reason)) paste0("FETCH FAILED: ", reason)
    else "FETCH FAILED"
  }

  rows <- list(
    list(signal = "IV Rank 1Y",
         reading = .with_reason(ivp_used, ivp_reason, "%.1f%%"),
         label   = regime),
    list(signal = "VRP",
         reading = if (!is.na(vrp_log))
                     sprintf("log %+.1f / %+.1fvp", vrp_log, vrp_vp)
                   else .with_reason(vrp_log, vrp_reason, "%+.1f"),
         label   = vrp_band),
    list(signal = "Term IV30/IV90",
         reading = .with_reason(term_pct, term_reason, "%+.1f%%"),
         label   = term_shape_label),
    list(signal = "Skew (RR 25Δ)",
         reading = .with_reason(rr_vp, rr_reason, "%+.1f vp"),
         label   = rr_lab),
    list(signal = "Earnings",
         reading = if (!is.na(ed_dte)) sprintf("%s (%dd)", as.character(ed), ed_dte)
                   else paste0("FETCH FAILED: ", ed_reason %||% "no earnings date"),
         label   = if (is.na(ed_dte)) "FETCH FAILED"
                   else if (ed_dte <= 0) "today/past"
                   else if (ed_dte <= config$earnings_window_days) "within event window"
                   else "outside event window"),
    list(signal = "Sector x-rank",
         reading = "see Phase B sector_rs_rank",
         label   = "passthrough")
  )

  # ── Funnel tally for the user's direction (buy-premium read) ────────────
  fav <- 0L; unfav <- 0L; unav <- 0L

  if (is.na(ivp_used)) unav <- unav + 1L
  else if (regime == "cheap") fav <- fav + 1L
  else unfav <- unfav + 1L

  if (is.na(vrp_log)) unav <- unav + 1L
  else if (vrp_log <= 0) fav <- fav + 1L
  else unfav <- unfav + 1L

  if (is.na(term_pct)) unav <- unav + 1L
  else if (term_pct < 0) fav <- fav + 1L
  else unfav <- unfav + 1L

  if (is.na(rr_vp)) unav <- unav + 1L
  else {
    if (direction == "long"  && rr_vp > 0) fav <- fav + 1L
    else if (direction == "short" && rr_vp < 0) fav <- fav + 1L
    else unfav <- unfav + 1L
  }

  if (is.na(ed_dte)) unav <- unav + 1L
  else if (ed_dte <= config$earnings_window_days && ed_dte > -7) unfav <- unfav + 1L
  else fav <- fav + 1L

  unav <- unav + 1L  # Sector — passthrough

  # Retrieval timestamps per source — fed into HTML tooltips
  retrieved <- list(
    prices_db = if (has_prices) as.character(prices_row$datetime) else NA_character_,
    skew_db   = if (!is.null(skew_row) && nrow(skew_row) > 0) as.character(skew_row$cache_date) else NA_character_,
    live_now  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  list(
    grid = rows,
    iv30 = iv30, iv90 = iv90, rv30 = rv30, ivp_used = ivp_used,
    vrp_log = vrp_log, vrp_vp = vrp_vp,
    term_pct = term_pct, term_shape = term_shape_label,
    rr_vp = rr_vp,
    earnings_date = ed, earnings_dte = ed_dte,
    regime = regime,
    spot = spot,
    reasons = list(
      iv30 = iv30_reason, iv90 = iv90_reason, rv30 = rv30_reason,
      ivp = ivp_reason, vrp = vrp_reason, term = term_reason,
      rr = rr_reason, earnings = ed_reason),
    retrieved = retrieved,
    tally = list(favorable = fav, unfavorable = unfav, unavailable = unav)
  )
}
