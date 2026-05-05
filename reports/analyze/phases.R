# reports/analyze/phases.R — Phase A, B, C.1, E orchestration for /analyze.
#
# Loads the latest swing_scanner CSV row for the ticker and exposes its
# Phase A/B/C/D fields. Falls back to live Tdata helpers when CSV emits NA.
# Phase outputs are mechanical — PASS / SKIP / NO SIGNAL / STALE — no advice.

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1) {
    if (is.na(a) || identical(a, "")) return(b)
  }
  a
}

# ── Scanner CSV loader ────────────────────────────────────────────────────
.find_latest_scanner_csv <- function(out_dir = "C:/Users/aldoh/Documents/NewTrading/reports") {
  files <- list.files(out_dir, pattern = "^swing_scanner_\\d{8}\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files <- files[order(files, decreasing = TRUE)]
  files[1]
}

#' Read the latest scanner CSV row for a ticker.
#' When `freshness` is supplied and the CSV mtime exceeds the policy cutoff,
#' returns row=NULL with stale=TRUE so callers fall back to live fetches.
.read_scanner_row <- function(ticker, freshness = NULL) {
  csv <- .find_latest_scanner_csv()
  if (is.null(csv)) return(list(row = NULL, csv_path = NULL, stale = TRUE))
  mtime <- file.info(csv)$mtime
  stale <- !is.null(freshness) && !is_fresh(mtime, freshness)
  if (stale) return(list(row = NULL, csv_path = csv, stale = TRUE, mtime = mtime))
  df <- tryCatch(
    read.csv2(csv, stringsAsFactors = FALSE, na.strings = c("NA", "")),
    error = function(e) NULL)
  if (is.null(df) || !"sym" %in% names(df))
    return(list(row = NULL, csv_path = csv, stale = FALSE, mtime = mtime))
  hit <- df[df$sym == ticker, , drop = FALSE]
  if (nrow(hit) == 0)
    return(list(row = NULL, csv_path = csv, stale = FALSE, mtime = mtime))
  list(row = hit[1, , drop = FALSE], csv_path = csv, stale = FALSE, mtime = mtime)
}

# ── PHASE A ──────────────────────────────────────────────────────────────
run_phase_a <- function(ticker, freshness = NULL) {
  scan <- .read_scanner_row(ticker, freshness)
  if (!is.null(scan$row)) {
    rich <- as.logical(scan$row$rich_pass)
    # Scanner emits TRUE permissively when end-of-day chain wasn't fetched. Confirm
    # against scanner_rich_universe.reason if available.
    permissive <- FALSE
    conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
    if (!is.null(conn)) {
      reason <- tryCatch(DBI::dbGetQuery(conn,
        "SELECT reason FROM scanner_rich_universe WHERE sym = ?
         ORDER BY cache_date DESC LIMIT 1",
        params = list(ticker))$reason, error = function(e) character(0))
      DBI::dbDisconnect(conn)
      permissive <- length(reason) > 0 && grepl("default|no expiry", reason, ignore.case = TRUE)
    }
    return(list(
      result      = if (isTRUE(rich)) "PASS" else "SKIP",
      reason      = if (permissive) "permissive default" else "scanner CSV",
      permissive  = permissive,
      source      = "scanner CSV",
      csv_path    = scan$csv_path,
      retrieved_at = scan$mtime
    ))
  }
  # Fallback: no scanner row — rely on Tdata helpers if reachable
  reason <- if (isTRUE(scan$stale))
              sprintf("scanner CSV stale (mtime %s) — live check skipped",
                      format(scan$mtime, "%Y-%m-%d %H:%M"))
            else "ticker not in scanner CSV; live check skipped"
  list(result = "STALE", reason = reason,
       permissive = FALSE, source = "live", csv_path = scan$csv_path)
}

# ── PHASE B ──────────────────────────────────────────────────────────────
run_phase_b <- function(ticker, direction, freshness = NULL) {
  scan <- .read_scanner_row(ticker, freshness)
  spot <- .live_price(ticker)
  breakdown <- .compute_phase_b_breakdown(ticker, spot, direction)

  empty <- list(
    result = "STALE", pull_score = NA_integer_, pull_direction = NA_character_,
    sector = NA_character_, sector_rs_rank = NA_integer_, stage = NA_character_,
    stage_pts = NA_integer_, sector_pts = NA_integer_, footprint_pts = NA_integer_,
    direction_match = NA_character_, price = spot, breakdown = breakdown)
  if (is.null(scan$row)) return(empty)

  r <- scan$row
  pull_dir <- r$pull_direction %||% "neutral"
  user_dir <- direction
  align <- if (pull_dir == "neutral") "NEUTRAL"
           else if ((user_dir == "long"  && pull_dir == "up") ||
                    (user_dir == "short" && pull_dir == "down")) "ALIGNED"
           else "MISMATCH"

  pull_pass <- isTRUE(as.logical(r$pull_pass))
  list(
    result          = if (pull_pass) "PASS" else "SKIP",
    pull_pass       = pull_pass,
    pull_score      = as.integer(r$pull_score),
    pull_direction  = pull_dir,
    sector          = r$sector,
    sector_rs_rank  = as.integer(r$sector_rs_rank),
    stage           = r$stage,
    stage_pts       = as.integer(r$stage_pts),
    sector_pts      = as.integer(r$sector_pts),
    footprint_pts   = as.integer(r$footprint_pts),
    direction_match = align,
    price           = spot,
    breakdown       = breakdown,
    scanner_csv_mtime = scan$mtime,
    breakdown_retrieved_at = if (!is.null(breakdown)) Sys.time() else NULL
  )
}

#' Compute the per-criterion technical breakdown for /analyze Phase B.
#' Returns NULL silently on any failure so the report falls back to aggregate.
.compute_phase_b_breakdown <- function(ticker, price, direction) {
  tryCatch({
    raw <- fetch_single_ohlcv(ticker)
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    ind <- calc_ind(raw)
    if (is.null(ind) || nrow(ind) == 0) return(NULL)
    last <- ind |> dplyr::filter(!is.na(adx10), !is.na(ma50), !is.na(rsi14),
                                 !is.na(obv_slope), !is.na(updn_ratio),
                                 !is.na(ret20)) |> tail(1)
    if (nrow(last) == 0) return(NULL)
    p <- if (is.null(price) || is.na(price)) tail(ind$Close, 1) else price
    compute_breakdown(last, p, direction)
  }, error = function(e) {
    message("Phase B breakdown failed: ", conditionMessage(e)); NULL
  })
}

.live_price <- function(ticker) {
  p <- tryCatch(Tdata::getLastSymPrice(ticker), error = function(e) NA_real_)
  if (is.null(p) || length(p) == 0) return(NA_real_)
  # Some Tdata variants return a data.frame; pluck a price-like column.
  if (is.data.frame(p)) {
    cand <- intersect(c("price", "Close", "close", "last", "value"), names(p))
    if (length(cand) > 0) p <- p[[cand[1]]] else {
      # Pick the last numeric column (date columns sit at the start)
      num_cols <- which(sapply(p, is.numeric))
      p <- if (length(num_cols) > 0) p[[tail(num_cols, 1)]] else p[[1]]
    }
  }
  as.numeric(p)[1]
}

# ── PHASE C (C.1 cheap score; C.2 funnel handled in funnel.R) ────────────
#
# Live-fetch policy: funnel runs unconditionally, even when the scanner CSV row
# is missing or all cached cheap_* fields are NA. When the scanner doesn't carry
# a cheap_score (the most common case after a Phase B SKIP), we recompute it
# from the live funnel using the same ivp_pts / vrp_pts / term_pts thresholds
# the scanner uses. cheap_side is derived from funnel.rr_vp sign.
run_phase_c <- function(ticker, direction, run_funnel = TRUE, config,
                        spot = NA_real_, freshness = NULL) {
  scan <- .read_scanner_row(ticker, freshness)
  r <- if (!is.null(scan$row)) scan$row else NULL

  funnel <- if (run_funnel)
    run_funnel_deep_dive(ticker, direction,
                         if (is.null(r)) NULL else r, config, spot = spot,
                         freshness = freshness)
  else NULL

  cached_cheap_pass  <- if (!is.null(r)) isTRUE(as.logical(r$cheap_pass)) else FALSE
  cached_cheap_score <- if (!is.null(r)) suppressWarnings(as.integer(r$cheap_score))
                        else NA_integer_
  cached_cheap_side  <- if (!is.null(r)) r$cheap_side else NA_character_
  cached_ivp_used    <- if (!is.null(r)) suppressWarnings(as.numeric(r$ivp_used))
                        else NA_real_
  cached_ivp_2y      <- if (!is.null(r)) suppressWarnings(as.numeric(r$ivp_2y))
                        else NA_real_
  cached_vrp         <- if (!is.null(r)) suppressWarnings(as.numeric(r$vrp))
                        else NA_real_

  recomputed <- if (is.na(cached_cheap_score) && !is.null(funnel))
    .recompute_cheap_score(funnel, direction, config) else NULL

  cheap_score <- if (!is.na(cached_cheap_score)) cached_cheap_score
                 else if (!is.null(recomputed)) recomputed$score
                 else NA_integer_
  cheap_side  <- if (!is.na(cached_cheap_side) && nzchar(cached_cheap_side)) cached_cheap_side
                 else if (!is.null(recomputed)) recomputed$side
                 else NA_character_
  ivp_used    <- if (!is.na(cached_ivp_used)) cached_ivp_used
                 else if (!is.null(funnel)) funnel$ivp_used else NA_real_
  vrp_value   <- if (!is.na(cached_vrp)) cached_vrp
                 else if (!is.null(funnel)) funnel$vrp_log else NA_real_

  cheap_pass <- if (!is.na(cached_cheap_score)) cached_cheap_pass
                else if (!is.na(cheap_score)) cheap_score >= 6L else FALSE

  result <- if (!is.na(cheap_score)) {
    if (cheap_pass) "PASS" else "SKIP"
  } else "STALE"

  list(
    result      = result,
    cheap_pass  = cheap_pass,
    cheap_score = cheap_score,
    cheap_side  = cheap_side,
    ivp_used    = ivp_used,
    ivp_2y      = cached_ivp_2y,
    vrp         = vrp_value,
    funnel      = funnel,
    source      = if (!is.na(cached_cheap_score)) "scanner CSV"
                  else if (!is.null(recomputed)) "live funnel"
                  else "unavailable"
  )
}

#' Recompute cheap_score from funnel data when scanner CSV row is NA.
#' Mirrors the swing scanner's points scheme (config$ivp_pts, vrp_pts, term_pts)
#' and adds a +1 RR-aligned bonus for direction agreement.
.recompute_cheap_score <- function(funnel, direction, config) {
  ivp <- funnel$ivp_used; vrp <- funnel$vrp_log
  term <- funnel$term_pct; rr <- funnel$rr_vp
  pts <- 0L

  ivp_pts <- if (is.na(ivp)) 0L
             else if (ivp <= config$ivp_pts$pt4_max) 4L
             else if (ivp <= config$ivp_pts$pt3_max) 3L
             else if (ivp <= config$ivp_pts$pt2_max) 2L
             else if (ivp <= config$ivp_pts$pt1_max) 1L
             else 0L
  vrp_pts <- if (is.na(vrp)) 0L
             else if (vrp <= config$vrp_pts$pt2_max) 2L
             else if (vrp <= config$vrp_pts$pt1_max) 1L
             else 0L
  term_pts <- if (is.na(term)) 0L
              else if (term <= config$term_pts$pt2_max) 2L
              else if (term <= config$term_pts$pt1_max) 1L
              else 0L
  rr_pts <- if (is.na(rr)) 0L
            else if ((direction == "long"  && rr > 0) ||
                     (direction == "short" && rr < 0)) 1L
            else 0L

  pts <- ivp_pts + vrp_pts + term_pts + rr_pts

  side <- if (is.na(rr) || abs(rr) < 1) "neutral"
          else if (rr > 0) "long" else "short"

  list(score = pts, side = side,
       components = list(ivp_pts = ivp_pts, vrp_pts = vrp_pts,
                         term_pts = term_pts, rr_pts = rr_pts))
}

# ── PHASE E ──────────────────────────────────────────────────────────────
run_phase_e <- function(phase_a, phase_b, phase_c, phase_d, config) {
  # Mechanical classification per template rules
  pass_a <- phase_a$result == "PASS"
  pass_b <- phase_b$result == "PASS"
  pass_c <- phase_c$result == "PASS"
  pass_d <- phase_d$result == "PASS"

  # phase_of_drop: A / B / C / D / none
  drop <- if (!pass_a) "A"
          else if (!pass_b) "B"
          else if (!pass_c) "C"
          else if (!pass_d) "D"
          else "none"

  # Classification label
  label <- if (pass_a && pass_b && pass_c && pass_d &&
               isTRUE(phase_d$any_within_cap)) "TOP PICK"
           else if (pass_a && pass_b && pass_c) "WATCH"
           else "SKIP"

  list(
    classification = label,
    phase_of_drop  = drop,
    pass_a = pass_a, pass_b = pass_b, pass_c = pass_c, pass_d = pass_d
  )
}
