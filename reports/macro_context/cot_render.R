# cot_render.R — {{COT_DASHBOARD}}: COT positioning by market, from the CSV that
# refresh_cot.R writes every Saturday (NewTrading/Reports/cot_positioning_latest.csv).
# Long / short / net, each as a 1y / 3y / 5y COT index, plus week-on-week changes.

COT_DASH_FILE <- "C:/Users/aldoh/Documents/NewTrading/Reports/cot_positioning_latest.csv"
COT_SECTOR_ORDER <- c("Energy", "Metals", "Grains", "Softs", "Equity indices", "Rates", "Currencies")

load_cot_dashboard <- function(path = COT_DASH_FILE) {
  if (!file.exists(path)) return(NULL)
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
           error = function(e) { message("COT dashboard unreadable: ", conditionMessage(e)); NULL })
}

cot_idx_td <- function(x) {
  if (is.na(x)) return("<td class='im-num'>&ndash;</td>")
  cls <- if (x >= 90) " cot-hi" else if (x <= 10) " cot-lo" else ""
  sprintf("<td class='im-num%s'>%.0f</td>", cls, x)
}

cot_k <- function(x, signed = FALSE) {
  if (is.na(x)) return("&ndash;")
  sprintf(if (signed) "%+.1fk" else "%.1fk", x / 1000)
}

cot_wow_td <- function(x) {
  if (is.na(x)) return("<td class='im-num'>&ndash;</td>")
  cls <- if (x > 0) "im-pos" else if (x < 0) "im-neg" else ""
  sprintf("<td class='im-num %s'>%s</td>", cls, cot_k(x, TRUE))
}

cot_table <- function(d) {
  head <- paste0(
    "<tr class='im-head'><td rowspan='2'>Market</td><td rowspan='2'>Group</td>",
    "<td rowspan='2' class='im-num'>Net</td><td colspan='3' class='im-num'>Week on week</td>",
    "<td colspan='3' class='im-num'>Net index</td><td colspan='3' class='im-num'>Long index</td>",
    "<td colspan='3' class='im-num'>Short index</td></tr>",
    "<tr class='im-head'><td class='im-num'>Net</td><td class='im-num'>Long</td><td class='im-num'>Short</td>",
    paste(rep("<td class='im-num'>1y</td><td class='im-num'>3y</td><td class='im-num'>5y</td>", 3), collapse = ""),
    "</tr>")
  secs <- intersect(c(COT_SECTOR_ORDER, unique(d$sector)), unique(d$sector))
  body <- vapply(secs, function(s) {
    ds <- d[d$sector == s, ]
    rows <- vapply(seq_len(nrow(ds)), function(i) {
      r <- ds[i, ]
      paste0("<tr><td>", esc(r$market), "</td><td class='im-sub'>", esc(r$group), "</td>",
             "<td class='im-num'>", cot_k(r$net, TRUE), "</td>",
             cot_wow_td(r$net_wow), cot_wow_td(r$long_wow), cot_wow_td(r$short_wow),
             cot_idx_td(r$net_idx_1y), cot_idx_td(r$net_idx_3y), cot_idx_td(r$net_idx_5y),
             cot_idx_td(r$long_idx_1y), cot_idx_td(r$long_idx_3y), cot_idx_td(r$long_idx_5y),
             cot_idx_td(r$short_idx_1y), cot_idx_td(r$short_idx_3y), cot_idx_td(r$short_idx_5y),
             "</tr>")
    }, "")
    paste0("<tr><td colspan='15' class='im-h3'>", esc(s), "</td></tr>", paste(rows, collapse = ""))
  }, "")
  sprintf("<div class='im-tw'><table>%s%s</table></div>", head, paste(body, collapse = ""))
}

cot_dashboard_html <- function(d) {
  if (is.null(d) || nrow(d) == 0)
    return("<div class='no-data'>No COT dashboard file &mdash; run refresh_cot.R</div>")
  as_of <- max(d$report_date)
  spec <- d[d$spec %in% c(TRUE, "TRUE"), ]
  other <- d[d$group %in% c("commercials", "asset managers"), ]
  legacy <- d[d$group == "[legacy] large spec", ]
  paste0(
    sprintf("<div class='im-sub'>CFTC report of %s (Tuesday close). Speculative group: managed money for commodities (disaggregated report), leveraged funds for financials (Traders in Financial Futures).</div>", as_of),
    cot_table(spec),
    "<details class='im-details'><summary>The other side: commercials (commodities) and asset managers (financials)</summary>",
    cot_table(other), "</details>",
    "<details class='im-details'><summary>Legacy large speculators (managed money + other reportables; the COTSignal series)</summary>",
    cot_table(legacy), "</details>",
    "<div class='im-legend'>Index = 100 &times; (current &minus; lowest) / (highest &minus; lowest) over the last 52, 156 or 260 weekly reports (1y, 3y, 5y). ",
    "100 = the largest net long, long or short position of the window; 0 = the smallest. Shaded: 90 or more, 10 or less. ",
    "Net, long and short are in contracts; week on week compares with the previous report. ",
    "Full detail per trader group: Reports/cot_positioning_latest.csv.</div>")
}
