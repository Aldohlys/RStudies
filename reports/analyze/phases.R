# reports/analyze/phases.R — Phase A, B, C.1, E orchestration for /analyze.
#
# Loads the latest swing_scanner v5 CSV row for the ticker and exposes its
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

# ── v5 CSV loader ─────────────────────────────────────────────────────────
.find_latest_v5_csv <- function(out_dir = "C:/Users/aldoh/Documents/NewTrading/reports") {
  files <- list.files(out_dir, pattern = "^swing_scanner_v5_\\d{8}\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files <- files[order(files, decreasing = TRUE)]
  files[1]
}

.read_v5_row <- function(ticker) {
  csv <- .find_latest_v5_csv()
  if (is.null(csv)) return(list(row = NULL, csv_path = NULL))
  df <- tryCatch(
    read.csv2(csv, stringsAsFactors = FALSE, na.strings = c("NA", "")),
    error = function(e) NULL)
  if (is.null(df) || !"sym" %in% names(df)) return(list(row = NULL, csv_path = csv))
  hit <- df[df$sym == ticker, , drop = FALSE]
  if (nrow(hit) == 0) return(list(row = NULL, csv_path = csv))
  list(row = hit[1, , drop = FALSE], csv_path = csv)
}

# ── PHASE A ──────────────────────────────────────────────────────────────
run_phase_a <- function(ticker) {
  v5 <- .read_v5_row(ticker)
  if (!is.null(v5$row)) {
    rich <- as.logical(v5$row$rich_pass)
    # v5 emits TRUE permissively when end-of-day chain wasn't fetched. Confirm
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
      reason      = if (permissive) "permissive default" else "v5 CSV",
      permissive  = permissive,
      source      = "v5 CSV",
      csv_path    = v5$csv_path
    ))
  }
  # Fallback: no v5 row — rely on Tdata helpers if reachable
  list(result = "STALE", reason = "ticker not in v5 CSV; live check skipped",
       permissive = FALSE, source = "live", csv_path = NULL)
}

# ── PHASE B ──────────────────────────────────────────────────────────────
run_phase_b <- function(ticker, direction) {
  v5 <- .read_v5_row(ticker)
  empty <- list(
    result = "STALE", pull_score = NA_integer_, pull_direction = NA_character_,
    sector = NA_character_, sector_rs_rank = NA_integer_, stage = NA_character_,
    stage_pts = NA_integer_, sector_pts = NA_integer_, footprint_pts = NA_integer_,
    direction_match = NA_character_, price = NA_real_)
  if (is.null(v5$row)) return(empty)

  r <- v5$row
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
    price           = .live_price(ticker)
  )
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
run_phase_c <- function(ticker, direction, run_funnel = TRUE, config) {
  v5 <- .read_v5_row(ticker)
  empty <- list(
    result = "STALE", cheap_score = NA_integer_, cheap_side = NA_character_,
    cheap_pass = FALSE, ivp_used = NA_real_, ivp_2y = NA_real_, vrp = NA_real_,
    funnel = NULL)
  if (is.null(v5$row)) return(empty)

  r <- v5$row
  cheap_pass <- isTRUE(as.logical(r$cheap_pass))
  out <- list(
    result      = if (cheap_pass) "PASS" else "SKIP",
    cheap_pass  = cheap_pass,
    cheap_score = as.integer(r$cheap_score),
    cheap_side  = r$cheap_side,
    ivp_used    = as.numeric(r$ivp_used),
    ivp_2y      = as.numeric(r$ivp_2y),
    vrp         = as.numeric(r$vrp))

  if (run_funnel) {
    out$funnel <- run_funnel_deep_dive(ticker, direction, r, config)
  }
  out
}

# ── PHASE E ──────────────────────────────────────────────────────────────
run_phase_e <- function(phase_a, phase_b, phase_c, phase_d, config) {
  v5 <- .read_v5_row(NULL)  # not used here directly
  # Try reading v5 CSV row again to grab rank + phase_of_drop verbatim
  ticker <- if (!is.null(phase_b$sector) || !is.na(phase_b$pull_score)) {
    # Pull ticker from the v5 row by finding the unique sym match in cached CSV.
    # Simpler: phase_b doesn't carry ticker, but we can re-derive from caller.
    NULL
  } else NULL

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

  # v5 classification label
  v5_label <- if (pass_a && pass_b && pass_c && pass_d &&
                  isTRUE(phase_d$any_within_cap)) "TOP PICK"
              else if (pass_a && pass_b && pass_c) "WATCH"
              else "SKIP"

  list(
    v5_classification = v5_label,
    phase_of_drop     = drop,
    pass_a = pass_a, pass_b = pass_b, pass_c = pass_c, pass_d = pass_d
  )
}
