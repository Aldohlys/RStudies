# reports/analyze/structures.R — Phase D: setup, chain, R:R + structures table.
#
# Mechanical: enumerate vertical spreads via Tdata Python compute_spread_risk_reward
# when reachable; fall back to v5 CSV row + DB option_chain_oi_history when not.
# Apply $300/lot filter (config). NO Best/Alternative/Aggressive/Avoid framing.

run_phase_d <- function(ticker, direction, phase_b, phase_c, config) {
  v5 <- .read_v5_row(ticker)
  empty_targets <- list(
    spot_target_low = NA_real_, spot_target_high = NA_real_,
    targets_agreeing = NA_integer_, fib_confirms = NA)
  empty <- list(
    result = "NO SIGNAL", vehicle = NA_character_, strike = NA_real_,
    expiry = NA_character_, targets = empty_targets,
    chain_state = NA_character_, oi_cap_call = NA_real_, oi_cap_put = NA_real_,
    effective_target = NA_real_, rr = NA_real_, entry_floor = NA_real_,
    entry_ceiling = NA_real_, headroom_band = NA_character_,
    entry_state = "NO SIGNAL",
    structures = data.frame(),
    n_structures_within_cap = 0L, any_within_cap = FALSE,
    targets_agreeing = NA_integer_)

  if (is.null(v5$row)) return(empty)
  r <- v5$row

  # If Phase B emitted no pull signal, the entry framework has nothing to bracket
  if (is.na(as.integer(r$pull_score)) || as.integer(r$pull_score) == 0 ||
      r$pull_direction == "neutral") {
    out <- empty
    out$vehicle <- r$vehicle
    out$expiry <- r$expiry
    out$targets <- list(
      spot_target_low = as.numeric(r$spot_target_low),
      spot_target_high = as.numeric(r$spot_target_high),
      targets_agreeing = as.integer(r$targets_agreeing),
      fib_confirms = as.logical(r$fib_confirms))
    out$targets_agreeing <- as.integer(r$targets_agreeing)
    out$chain_state <- r$chain_state
    out$oi_cap_call <- as.numeric(r$oi_cap_call)
    out$oi_cap_put  <- as.numeric(r$oi_cap_put)
    out$effective_target <- as.numeric(r$effective_target)
    out$entry_state <- "NO SIGNAL"
    out$structures <- enumerate_structures(ticker, direction, phase_b$price,
                                            r, config, signal_present = FALSE)
    out$n_structures_within_cap <- sum(out$structures$within_cap %||% logical(0),
                                        na.rm = TRUE)
    out$any_within_cap <- any(out$structures$within_cap %||% logical(0), na.rm = TRUE)
    return(out)
  }

  # Standard path: pull from v5 row
  vehicle <- r$vehicle
  expiry  <- r$expiry
  rr      <- as.numeric(r$rr)
  entry_state <- r$entry_state
  any_within <- isTRUE(rr >= config$rr_min)

  structures <- enumerate_structures(ticker, direction, phase_b$price,
                                      r, config, signal_present = TRUE)
  within <- if ("within_cap" %in% names(structures))
              structures$within_cap else logical(0)
  n_within <- sum(within, na.rm = TRUE)

  d_pass <- (as.integer(r$targets_agreeing) %||% 0L) >= 2L &&
            isTRUE(rr >= config$rr_min) &&
            (entry_state == "IN BAND") &&
            (r$chain_state %||% "") != "chain-capped" &&
            n_within > 0

  list(
    result = if (d_pass) "PASS" else "SKIP",
    vehicle = vehicle, strike = as.numeric(r$strike), expiry = expiry,
    targets = list(
      spot_target_low = as.numeric(r$spot_target_low),
      spot_target_high = as.numeric(r$spot_target_high),
      targets_agreeing = as.integer(r$targets_agreeing),
      fib_confirms = as.logical(r$fib_confirms)),
    targets_agreeing = as.integer(r$targets_agreeing),
    chain_state = r$chain_state,
    oi_cap_call = as.numeric(r$oi_cap_call),
    oi_cap_put  = as.numeric(r$oi_cap_put),
    effective_target = as.numeric(r$effective_target),
    rr = rr,
    entry_floor = as.numeric(r$entry_floor),
    entry_ceiling = as.numeric(r$entry_ceiling),
    headroom_band = r$headroom_band,
    entry_state = entry_state,
    structures = structures,
    n_structures_within_cap = n_within,
    any_within_cap = n_within > 0
  )
}

# ── Structure enumeration (live spread pricer if reachable, else DB cache) ──
enumerate_structures <- function(ticker, direction, spot, v5_row, config,
                                  signal_present = TRUE) {
  cap <- config$risk_cap_lot_usd
  expiry <- v5_row$expiry
  vehicle <- v5_row$vehicle %||% "spread"
  right <- if (direction == "long") "C" else "P"

  # Try Python compute_spread_risk_reward via reticulate; bail if unavailable.
  spreads_df <- tryCatch({
    if (!requireNamespace("reticulate", quietly = TRUE)) stop("no reticulate")
    if (is.na(expiry) || is.na(spot)) stop("no expiry/spot")
    spread_mod <- reticulate::import("tdata_py.spread", delay_load = TRUE)
    rows <- list()
    for (w in config$spread_widths) {
      df <- tryCatch(spread_mod$compute_spread_risk_reward(
        sym = ticker, trading_class = ticker, expiration = expiry,
        current_price = spot, moneyness_pct = config$moneyness_pct,
        spread_width = as.integer(w), right = right,
        multiplier = 100L, currency = "USD",
        exchangeSec = "SMART", exchangeOpt = "SMART",
        force_refresh = TRUE), error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) {
        df$source <- "live"
        rows[[length(rows) + 1]] <- df
      }
    }
    if (length(rows) == 0) NULL else do.call(rbind, rows)
  }, error = function(e) NULL)

  if (is.null(spreads_df) || nrow(spreads_df) == 0) {
    # DB fallback — return placeholder rows for the structure-table; the user
    # sees that no live enumeration happened.
    return(.placeholder_structures(ticker, direction, spot, v5_row, config))
  }

  # Filter: DEBIT (or both, neutral display) and within cap
  spreads_df$within_cap <- spreads_df$max_risk <= cap
  spreads_df <- spreads_df[order(-spreads_df$reward_risk_ratio), ]
  spreads_df
}

.placeholder_structures <- function(ticker, direction, spot, v5_row, config) {
  data.frame(
    structure = c("Stock direct",
                  sprintf("%s vertical debit (live pricer unavailable)",
                          if (direction == "long") "Bull call" else "Bear put")),
    expiry = c(NA, v5_row$expiry %||% NA_character_),
    debit  = c(NA_real_, NA_real_),
    max_risk = c(NA_real_, NA_real_),
    max_reward = c(NA_real_, NA_real_),
    reward_risk_ratio = c(NA_real_, NA_real_),
    prob_success_delta = c(NA_real_, NA_real_),
    within_cap = c(NA, NA),
    surface_fact = c("n/a — IV exposure does not apply",
                     sprintf("VRP %s, RR %s",
                             v5_row$vrp %||% "n/a",
                             "see Phase C")),
    source = c("placeholder", "placeholder"),
    stringsAsFactors = FALSE
  )
}
