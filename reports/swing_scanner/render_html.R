# render_html.R — HTML rendering for swing scanner report

source(file.path(SCRIPT_DIR, "..", "shared", "html_helpers.R"))
source(file.path(SCRIPT_DIR, "scoring.R"))

#' Build sector gate HTML rows with clear direction badge and macro context
build_sector_rows <- function(sector_ok) {
  paste0(lapply(names(sector_ok), function(sec) {
    s <- sector_ok[[sec]]
    css <- if (s$long && s$short) "gate-both"
           else if (s$long) "gate-long"
           else if (s$short) "gate-short"
           else "gate-blocked"

    # Direction badge
    if (s$long && s$short) {
      dir_badge <- '<span class="dir-badge-long">LONG</span> <span class="dir-badge-short">SHORT</span>'
    } else if (s$long) {
      dir_badge <- '<span class="dir-badge-long">LONG</span>'
    } else if (s$short) {
      dir_badge <- '<span class="dir-badge-short">SHORT</span>'
    } else {
      dir_badge <- '<span class="dir-badge-blocked">BLOCKED</span>'
    }

    # ETF status line: MA20 position, slope, ADX
    if (is.na(s$rs)) {
      etf_status <- "no data"
    } else {
      above_ma20 <- !is.null(s$etf_close) && !is.null(s$etf_ma20) &&
                     !is.na(s$etf_close) && !is.na(s$etf_ma20) && s$etf_close > s$etf_ma20
      adx_val <- if (!is.null(s$adx10) && !is.na(s$adx10)) s$adx10 else 0
      adx_ok <- adx_val > 20
      ma20_pos <- if (above_ma20) "above" else "below"

      # Determine slope direction from which gate passed
      if (s$long) slope_dir <- "rising"
      else if (s$short) slope_dir <- "falling"
      else slope_dir <- "flat"

      parts <- sprintf("ETF %s MA20, slope %s, ADX %.0f", ma20_pos, slope_dir, adx_val)

      # Add blocked reason if neither gate passes
      if (!s$long && !s$short) {
        if (!adx_ok) parts <- paste0(parts, " — ADX too low")
        else parts <- paste0(parts, " — no directional confirmation")
      }
      etf_status <- sprintf('<span class="etf-status">%s</span>', parts)
    }

    # Macro context: tailwinds, headwinds, mismatches
    macro_parts <- c()
    tw <- s$macro_mod$tw_active
    hw <- s$macro_mod$hw_active
    if (length(tw) > 0)
      macro_parts <- c(macro_parts, sprintf('<span class="macro-tw">TW: %s</span>', paste(tw, collapse = ", ")))
    if (length(hw) > 0)
      macro_parts <- c(macro_parts, sprintf('<span class="macro-hw">HW: %s</span>', paste(hw, collapse = ", ")))
    mn <- s$macro_mod$macro_note
    if (nzchar(mn))
      macro_parts <- c(macro_parts, sprintf('<span class="macro-mm">%s</span>', mn))
    macro_str <- if (length(macro_parts) > 0) paste(macro_parts, collapse = "<br>") else ""

    sprintf('<tr class="%s"><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class="gate-macro">%s</td></tr>',
      css, sec, s$etf, dir_badge, etf_status,
      ifelse(is.na(s$rs), "n/a", sprintf("%+.1f%%", s$rs)),
      macro_str)
  }), collapse = "\n")
}

#' Build transitions HTML section
build_transitions_html <- function(transitions) {
  if (is.null(transitions) || nrow(transitions) == 0) {
    return('<div class="no-data">No transitions (first run or no changes).</div>')
  }

  trans_css <- c(
    NEW_ENTRY   = "trans-new",
    UPGRADED    = "trans-up",
    DOWNGRADED  = "trans-down",
    DISAPPEARED = "trans-gone"
  )

  rows <- paste0(vapply(seq_len(nrow(transitions)), function(i) {
    t <- transitions[i, ]
    css <- if (t$transition_type %in% names(trans_css)) trans_css[t$transition_type] else ""
    sprintf('<tr class="%s"><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%+d</td></tr>',
      css, t$ticker, t$prev_signal, t$new_signal, t$transition_type, t$score_delta)
  }, character(1)), collapse = "\n")

  sprintf('<table>
    <tr><th>Ticker</th><th>Previous</th><th>Current</th><th>Type</th><th>Score &Delta;</th></tr>
    %s</table>', rows)
}

#' Build active alerts HTML section
build_alerts_html <- function(alerts, out_tickers) {
  if (is.null(alerts) || nrow(alerts) == 0) {
    return('<div class="no-data">No active alerts.</div>')
  }

  rows <- paste0(vapply(seq_len(nrow(alerts)), function(i) {
    a <- alerts[i, ]
    tk    <- if ("Asset" %in% names(a)) a[["Asset"]] else "?"
    theme <- if ("Theme" %in% names(a)) a[["Theme"]] else ""
    desc  <- if ("Description" %in% names(a)) a[["Description"]] else ""
    adate <- if ("AlertDate" %in% names(a)) a[["AlertDate"]] else ""
    in_scanner <- if (tk %in% out_tickers) '<span class="alert-match">IN SCANNER</span>' else ""
    sprintf('<tr><td class="alert-asset">%s %s</td><td class="alert-theme">%s</td><td class="alert-date">%s</td><td class="alert-desc">%s</td></tr>',
      tk, in_scanner, theme, adate, desc)
  }, character(1)), collapse = "\n")

  sprintf('<table><tr><th>Asset</th><th>Theme</th><th>Date</th><th>Description</th></tr>%s</table>', rows)
}

#' Optionality badge CSS class
opt_css <- function(g3) {
  switch(g3,
    "PASS"    = "g3-pass",
    "PARTIAL" = "g3-partial",
    "FAIL"    = "g3-fail",
    "g3-nodata")
}

#' Map signal to summary badge CSS class
summary_badge_css <- function(sig) {
  switch(sig,
    "LONG TRADE"  = "sig-trade-long",
    "SHORT TRADE" = "sig-trade-short",
    "LONG WATCH"  = "sig-watch",
    "SHORT WATCH" = "sig-watch",
    "sig-skip")
}

# ── Column definitions for the two tables ─────────────────────────────────────
# Technical columns first, then optionality columns.

SIGNAL_COLS <- c("Ticker", "Sector", "Composite", "Price",
  "ADX10", "RSI14", "RS_vs_ETF",
  "IV30", "RV30", "IVP", "VRP", "Optionality", "TermStr")

SIGNAL_TIPS <- c(
  Ticker      = "Stock ticker symbol",
  Sector      = "Industry sector classification",
  Composite   = "Tech score (0-10) + persistence (+1 if previously on watchlist)",
  Price       = "Last closing price in USD",
  ADX10       = "Average Directional Index (10d)",
  RSI14       = "Relative Strength Index (14d)",
  RS_vs_ETF   = "Relative Strength vs sector ETF (20d)",
  IV30        = "Implied volatility 30d",
  RV30        = "Realized volatility 30d",
  IVP         = "IV percentile vs history",
  VRP         = "Vol Risk Premium: IV30 - RV30 (percentage points)",
  Optionality = "PASS (3-4), PARTIAL (2), FAIL (0-1): IV30<40 + IVP<60 + VRP<0 + Contango",
  TermStr     = "IV term structure: Contango/Backwardation"
)

#' Build table header
build_th <- function(cols, tips) {
  paste0(vapply(cols, function(col) {
    tip <- tips[col]
    if (!is.na(tip)) sprintf('<th title="%s">%s</th>', tip, col)
    else sprintf('<th>%s</th>', col)
  }, character(1)), collapse = "")
}

#' Determine direction for a row
get_direction <- function(row) {
  best <- as.character(row[["Best_Signal"]])
  if (grepl("LONG", best)) "LONG" else "SHORT"
}

#' Build a table body for a subset of rows
#' @param df data.frame subset (TRADE or WATCH rows)
#' @param show_top_pick Whether to show TOP PICK badge
build_signal_tbody <- function(df, show_top_pick = FALSE) {
  if (nrow(df) == 0) {
    return(sprintf('<tr><td colspan="%d" class="no-data">None.</td></tr>', length(SIGNAL_COLS)))
  }

  cache_date_str <- format(Sys.Date(), "%Y-%m-%d")

  paste0(vapply(seq_len(nrow(df)), function(ri) {
    row <- df[ri, ]
    direction <- get_direction(row)
    r_css <- if (direction == "LONG") "row-long" else "row-short"

    vol_ts <- if ("Vol_Date" %in% names(row) && !is.na(row[["Vol_Date"]])) {
      as.character(row[["Vol_Date"]])
    } else ""

    cells <- paste0(vapply(SIGNAL_COLS, function(col) {
      v <- as.character(row[[col]])
      if (is.na(v) || v == "NA") v <- ""
      if (col == "Optionality") {
        sprintf('<td class="%s">%s</td>', opt_css(v), v)
      } else if (col %in% c("IV30", "RV30") && nzchar(v)) {
        sprintf('<td title="Vol data: %s">%s%%</td>', vol_ts, v)
      } else if (col == "Price") {
        sprintf('<td title="Yahoo cache: %s">%s</td>', cache_date_str, v)
      } else if (col == "ATR_pct") {
        sprintf('<td title="Yahoo cache: %s">%s</td>', cache_date_str, v)
      } else {
        sprintf('<td>%s</td>', v)
      }
    }, character(1)), collapse = "")

    # Prepend direction badge + TOP PICK if applicable
    badge <- sprintf('<span class="dir-badge dir-%s">%s</span>', tolower(direction), direction)
    if (show_top_pick) badge <- paste0('<span class="top-pick-badge">TOP PICK</span> ', badge)
    cells <- sub("^<td>", paste0("<td>", badge, " "), cells)

    sprintf('<tr class="%s">%s</tr>', r_css, cells)
  }, character(1)), collapse = "\n")
}

#' Render swing scanner HTML
render_scanner_html <- function(out, sector_ok, macro_bias, macro, csv_file, out_dir,
                                 price_max, transitions = NULL, active_alerts = NULL) {
  template_file <- file.path(SCRIPT_DIR, "template.html")
  template <- paste(readLines(template_file, encoding = "UTF-8"), collapse = "\n")

  # Split into TRADE and WATCH subsets
  trade_rows <- out[out$Best_Signal %in% c("LONG TRADE", "SHORT TRADE"), , drop = FALSE]
  watch_rows <- out[out$Best_Signal %in% c("LONG WATCH", "SHORT WATCH"), , drop = FALSE]

  # Ensure all columns exist
  for (col in c(SIGNAL_COLS, "Best_Signal", "Rank")) {
    if (!col %in% names(trade_rows)) trade_rows[[col]] <- NA
    if (!col %in% names(watch_rows)) watch_rows[[col]] <- NA
  }

  # Build tables
  trade_th    <- build_th(SIGNAL_COLS, SIGNAL_TIPS)
  watch_th    <- trade_th  # same columns
  trade_tbody <- build_signal_tbody(trade_rows, show_top_pick = TRUE)
  watch_tbody <- build_signal_tbody(watch_rows, show_top_pick = FALSE)

  # Signal summary badges
  display_out <- out[out$Best_Signal != "SKIP", , drop = FALSE]
  sig_counts <- table(display_out$Best_Signal)
  summary_items <- paste0(vapply(names(sig_counts), function(s) {
    sprintf('<span class="sig-badge %s">%s: %d</span>', summary_badge_css(s), s, sig_counts[s])
  }, character(1)), collapse = " ")
  n_top <- sum(display_out$Rank == "TOP PICK", na.rm = TRUE)
  if (n_top > 0) {
    summary_items <- paste0(
      sprintf('<span class="sig-badge sig-top-pick">TOP PICK: %d</span> ', n_top),
      summary_items)
  }

  # Macro banner
  macro_banner <- sprintf("Bias: %s | VIX: %s | S5FI: %s%%",
    macro_bias,
    ifelse(is.na(macro$vix[1]), "n/a", round(macro$vix[1], 1)),
    ifelse(is.na(macro$s5fi[1]), "n/a", round(macro$s5fi[1], 1)))
  macro_css <- switch(macro_bias, "LONG BIAS" = "badge-green", "SHORT BIAS" = "badge-red", "badge-orange")

  # Transitions and Alerts HTML
  transitions_html <- build_transitions_html(transitions)
  alerts_html <- build_alerts_html(active_alerts, display_out$Ticker)

  replacements <- list(
    "{{RUN_DATE}}"     = format(Sys.time(), "%d %B %Y — %H:%M"),
    "{{MACRO_CSS}}"    = macro_css,
    "{{MACRO_BANNER}}" = macro_banner,
    "{{SUMMARY}}"      = summary_items,
    "{{N_SCORED}}"     = as.character(nrow(out)),
    "{{N_DISPLAYED}}"  = as.character(nrow(display_out)),
    "{{PRICE_MAX}}"    = as.character(price_max),
    "{{SECTOR_ROWS}}"  = build_sector_rows(sector_ok),
    "{{TRANSITIONS}}"  = transitions_html,
    "{{ALERTS}}"       = alerts_html,
    "{{TRADE_TH}}"     = trade_th,
    "{{TRADE_TBODY}}"  = trade_tbody,
    "{{WATCH_TH}}"     = watch_th,
    "{{WATCH_TBODY}}"  = watch_tbody,
    "{{CSV_FILE}}"     = basename(csv_file)
  )

  html <- render_template(template, replacements)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  html_file <- file.path(out_dir, sprintf("swing_scanner_%s.html", format(Sys.Date(), "%Y%m%d")))
  writeLines(html, html_file)
  message(sprintf("HTML written: %s", html_file))
  html_file
}
