# render_html.R — HTML rendering for swing scanner report

source(file.path(SCRIPT_DIR, "..", "shared", "html_helpers.R"))
source(file.path(SCRIPT_DIR, "scoring.R"))

#' Build sector gate HTML rows
build_sector_rows <- function(sector_ok) {
  paste0(lapply(names(sector_ok), function(sec) {
    s <- sector_ok[[sec]]
    css <- if (s$long && s$short) "gate-both"
           else if (s$long) "gate-long"
           else if (s$short) "gate-short"
           else "gate-blocked"
    sprintf('<tr class="%s"><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
      css, sec, s$etf,
      ifelse(s$long, "APPROVED", "blocked"),
      ifelse(s$short, "APPROVED", "blocked"),
      ifelse(is.na(s$rs), "n/a", sprintf("%+.1f%%", s$rs)),
      s$macro_mod$macro_note)
  }), collapse = "\n")
}

#' Map Best_Signal to row CSS class
row_css <- function(sig) {
  switch(sig,
    "LONG TRADE"  = "row-long-trade",
    "SHORT TRADE" = "row-short-trade",
    "LONG WATCH"  = "row-long-watch",
    "SHORT WATCH" = "row-short-watch",
    "")
}

#' Map signal cell value to CSS class
cell_css <- function(sig) {
  switch(sig,
    "TRADE" = "sig-long-trade",
    "WATCH" = "sig-long-watch",
    "LONG TRADE"  = "sig-long-trade",
    "SHORT TRADE" = "sig-short-trade",
    "LONG WATCH"  = "sig-long-watch",
    "SHORT WATCH" = "sig-short-watch",
    "sig-skip")
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

#' Column tooltips for scanner output
COL_TIPS <- c(
  Ticker       = "Stock ticker symbol",
  Sector       = "Industry sector classification",
  Price        = "Last closing price in USD",
  ATR_pct      = "Average True Range (14d) as % of price",
  Long_Score   = "Long signal strength (0-9+macro)",
  Long_Signal  = "TRADE (>=7), WATCH (5-6), SKIP (<5)",
  Long_Entry   = "Suggested long entry: price x 1.002",
  Long_Stop    = "Suggested long stop: MA50 x 0.99",
  Long_Target  = "Suggested long target: Entry + 2.5x risk",
  Short_Score  = "Short signal strength (0-9+macro)",
  Short_Signal = "TRADE (>=7), WATCH (5-6), SKIP (<5)",
  Short_Entry  = "Suggested short entry: price x 0.998",
  Short_Stop   = "Suggested short stop: MA50 x 1.01",
  Short_Target = "Suggested short target: Entry - 2.5x risk",
  ADX10        = "Average Directional Index (10d)",
  RSI14        = "Relative Strength Index (14d)",
  RSI_slope    = "RSI change over 5 days",
  MA50_dist    = "Distance from 50-day MA as %",
  OBV_rising   = "On-Balance Volume trend (20d)",
  UpDn_ratio   = "Up-volume / Down-volume ratio (10d)",
  RS_vs_ETF    = "Relative Strength vs sector ETF",
  Macro_Note   = "Macro context signal from macro_context",
  Best_Signal  = "Best signal between long and short side"
)

DISPLAY_COLS <- c("Ticker","Sector","Price","ATR_pct","Long_Score","Long_Signal",
  "Long_Entry","Long_Stop","Long_Target","Short_Score","Short_Signal",
  "Short_Entry","Short_Stop","Short_Target","ADX10","RSI14","RSI_slope",
  "MA50_dist","OBV_rising","UpDn_ratio","RS_vs_ETF","Macro_Note","Best_Signal")

#' Render swing scanner HTML
render_scanner_html <- function(out, sector_ok, macro_bias, macro, csv_file, out_dir, price_max) {
  template_file <- file.path(SCRIPT_DIR, "template.html")
  template <- paste(readLines(template_file, encoding = "UTF-8"), collapse = "\n")

  # Table header with tooltips
  th_cells <- paste0(vapply(DISPLAY_COLS, function(col) {
    tip <- COL_TIPS[col]
    if (!is.na(tip)) sprintf('<th title="%s">%s</th>', tip, col)
    else sprintf('<th>%s</th>', col)
  }, character(1)), collapse = "")

  # Table body — row color based on Best_Signal
  tbody <- paste0(apply(out[, DISPLAY_COLS], 1, function(row) {
    best <- row["Best_Signal"]
    r_css <- row_css(best)
    cells <- paste0(vapply(DISPLAY_COLS, function(col) {
      v <- row[col]
      if (is.na(v) || v == "NA") v <- ""
      if (col %in% c("Long_Signal", "Short_Signal", "Best_Signal")) {
        sprintf('<td class="%s">%s</td>', cell_css(v), v)
      } else {
        sprintf('<td>%s</td>', v)
      }
    }, character(1)), collapse = "")
    sprintf('<tr class="%s">%s</tr>', r_css, cells)
  }), collapse = "\n")

  # Signal summary badges
  sig_counts <- table(out$Best_Signal)
  summary_items <- paste0(vapply(names(sig_counts), function(s) {
    sprintf('<span class="sig-badge %s">%s: %d</span>', summary_badge_css(s), s, sig_counts[s])
  }, character(1)), collapse = " ")

  # Macro banner
  macro_banner <- sprintf("Bias: %s | VIX: %s | S5FI: %s%%",
    macro_bias,
    ifelse(is.na(macro$vix[1]), "n/a", round(macro$vix[1], 1)),
    ifelse(is.na(macro$s5fi[1]), "n/a", round(macro$s5fi[1], 1)))
  macro_css <- switch(macro_bias, "LONG BIAS" = "badge-green", "SHORT BIAS" = "badge-red", "badge-orange")

  replacements <- list(
    "{{RUN_DATE}}"     = format(Sys.Date(), "%d %B %Y"),
    "{{MACRO_CSS}}"    = macro_css,
    "{{MACRO_BANNER}}" = macro_banner,
    "{{SUMMARY}}"      = summary_items,
    "{{N_SCORED}}"     = as.character(nrow(out)),
    "{{PRICE_MAX}}"    = as.character(price_max),
    "{{SECTOR_ROWS}}"  = build_sector_rows(sector_ok),
    "{{TH_CELLS}}"     = th_cells,
    "{{TBODY}}"        = tbody,
    "{{CSV_FILE}}"     = basename(csv_file)
  )

  html <- render_template(template, replacements)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  html_file <- file.path(out_dir, sprintf("swing_scanner_%s.html", format(Sys.Date(), "%Y%m%d")))
  writeLines(html, html_file)
  message(sprintf("HTML written: %s", html_file))
  html_file
}
