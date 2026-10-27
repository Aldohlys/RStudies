# cot_render.R — {{COT_DASHBOARD}}: COT positioning by market, from the CSV that
# refresh_cot.R writes every Saturday (NewTrading/Reports/cot_positioning_latest.csv).
# Overview: per market, the 3y net index of specs / other side / small traders and a
# 5-year chart of net contracts; per sector, the specs' long / short / net and 3y index
# history (output/cot_history.csv). The full 1y / 3y / 5y tables stay below, collapsed.

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

# ── Charts: weekly history from output/cot_history.csv (refresh_cot.R) ────────
# Speculators = managed money (commodities) / leveraged funds (financials);
# other side = commercials / asset managers; small traders = retail (non-reportable).
COT_HIST_FILE <- file.path(SCRIPT_DIR, "output", "cot_history.csv")
COT_CHART_WEEKS <- 260
COT_OTHER_SIDE <- c("commercials", "asset managers")

load_cot_history <- function(path = COT_HIST_FILE) {
  if (!file.exists(path)) return(NULL)
  h <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                error = function(e) { message("COT history unreadable: ", conditionMessage(e)); NULL })
  if (is.null(h)) return(NULL)
  h$date <- as.Date(h$date)
  h
}

#' Last COT_CHART_WEEKS reports of one market and group, oldest first
cot_hist_rows <- function(h, market, group) {
  x <- h[h$market == market & h$group == group, ]
  x <- x[order(x$date), ]
  utils::tail(x, COT_CHART_WEEKS)
}

#' Linear scale from [lo, hi] onto [a, b]
cot_scale <- function(lo, hi, a, b) function(v) a + (v - lo) / (if (hi == lo) 1 else hi - lo) * (b - a)

cot_poly <- function(x, y, cls) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 2) return("")
  sprintf("<polyline class='%s' points='%s'/>", cls, paste(sprintf("%.1f,%.1f", x[ok], y[ok]), collapse = " "))
}

#' Year gridlines (1 January) with 'YY labels under the plot
cot_years <- function(dates, sx, y0, y1, ylab) {
  yrs <- seq(as.integer(format(min(dates), "%Y")) + 1, as.integer(format(max(dates), "%Y")))
  if (!length(yrs) || yrs[1] > yrs[length(yrs)]) return("")
  paste(vapply(yrs, function(y) {
    x <- sx(as.numeric(as.Date(sprintf("%d-01-01", y))))
    sprintf("<line class='cot-grid' x1='%.1f' x2='%.1f' y1='%.1f' y2='%.1f'/><text class='cot-yr' x='%.1f' y='%.1f'>%s</text>",
            x, x, y0, y1, x + 2, ylab, substr(y, 3, 4))
  }, ""), collapse = "")
}

#' Row chart: net contracts of the speculators (blue) and the other side (black)
cot_spark <- function(sp, ot, w = 560, h = 58) {
  if (nrow(sp) < 2) return("")
  dn <- as.numeric(sp$date)
  sx <- cot_scale(min(dn), max(dn), 1, w - 1)
  vals <- c(sp$net, ot$net, 0)
  sy <- cot_scale(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE), h - 12, 3)
  z <- sy(0)
  sprintf("<svg class='cot-spark' viewBox='0 0 %d %d' width='%d' height='%d' aria-hidden='true'>%s<line class='cot-zero' x1='0' x2='%d' y1='%.1f' y2='%.1f'/>%s%s</svg>",
          w, h, w, h, cot_years(sp$date, sx, 0, h - 10, h - 1), w, z, z,
          cot_poly(sx(as.numeric(ot$date)), sy(ot$net), "cot-l-oth"),
          cot_poly(sx(dn), sy(sp$net), "cot-l-spec"))
}

cot_kax <- function(v) if (abs(v) >= 1e6) sprintf("%.1fM", v / 1e6) else sprintf("%.0fk", v / 1000)

#' Market card: speculators' longs / shorts / net, then their 3y net index with the 15 / 85 bands
cot_detail_svg <- function(sp, w = 560) {
  if (nrow(sp) < 2) return("")
  L <- 40; R <- w - 6; t1 <- 6; b1 <- 120; t2 <- 140; b2 <- 196; h <- 210
  dn <- as.numeric(sp$date)
  sx <- cot_scale(min(dn), max(dn), L, R)
  vals <- c(sp$long, sp$short, sp$net, 0)
  lo <- min(vals, na.rm = TRUE); hi <- max(vals, na.rm = TRUE)
  sy <- cot_scale(lo, hi, b1, t1)
  si <- cot_scale(0, 100, b2, t2)
  ax <- function(v, y) sprintf("<text class='cot-ax' x='%d' y='%.1f' text-anchor='end'>%s</text>", L - 4, y + 3, v)
  paste0(
    sprintf("<svg class='cot-chart' viewBox='0 0 %d %d' aria-hidden='true'>", w, h),
    cot_years(sp$date, sx, t1, b2, h - 2),
    sprintf("<line class='cot-zero' x1='%d' x2='%.1f' y1='%.1f' y2='%.1f'/>", L, R, sy(0), sy(0)),
    ax(cot_kax(hi), sy(hi)), ax(cot_kax(lo), sy(lo)), if (lo < 0 && hi > 0) ax("0", sy(0)) else "",
    cot_poly(sx(dn), sy(sp$long), "cot-l-long"), cot_poly(sx(dn), sy(sp$short), "cot-l-short"),
    cot_poly(sx(dn), sy(sp$net), "cot-l-net"),
    sprintf("<rect class='cot-band-hi' x='%d' y='%.1f' width='%.1f' height='%.1f'/>", L, si(100), R - L, si(85) - si(100)),
    sprintf("<rect class='cot-band-lo' x='%d' y='%.1f' width='%.1f' height='%.1f'/>", L, si(15), R - L, si(0) - si(15)),
    sprintf("<rect class='cot-frame' x='%d' y='%.1f' width='%.1f' height='%.1f'/>", L, si(100), R - L, si(0) - si(100)),
    ax("100", si(100)), ax("50", si(50)), ax("0", si(0)),
    cot_poly(sx(dn), si(sp$net_idx_3y), "cot-l-idx"),
    "</svg>")
}

#' 3y net index cell: blue = more long than usual for that group, red = less long
cot_score_td <- function(x, tip) {
  if (length(x) == 0 || is.na(x)) return("<td class='hz cot-score'>&ndash;</td>")
  cls <- if (x >= 85) "hz-p3" else if (x >= 70) "hz-p2" else if (x >= 60) "hz-p1" else
         if (x <= 15) "hz-n3" else if (x <= 30) "hz-n2" else if (x <= 40) "hz-n1" else ""
  sprintf("<td class='hz cot-score %s' title='%s'>%.0f</td>", cls, esc(tip), x)
}

#' Overview: one row per market, the three groups' 3y scores and the 5-year net chart
cot_overview <- function(d, h) {
  pick <- function(m, f) d[d$market == m & f, , drop = FALSE]
  secs <- intersect(c(COT_SECTOR_ORDER, unique(d$sector)), unique(d$sector))
  head <- paste0("<tr class='im-head'><td>Market</td><td class='cot-score'>Specs</td><td class='cot-score'>Other side</td>",
                 "<td class='cot-score'>Small</td><td class='im-num'>Specs net</td><td class='im-num'>WoW</td>",
                 "<td>Net contracts, 5 years: <span class='cot-key-spec'>specs</span> vs <span class='cot-key-oth'>other side</span></td></tr>")
  body <- vapply(secs, function(s) {
    mk <- unique(d$market[d$sector == s])
    rows <- vapply(mk, function(m) {
      sp <- pick(m, d$spec %in% c(TRUE, "TRUE")); ot <- pick(m, d$group %in% COT_OTHER_SIDE)
      rt <- pick(m, d$group == "retail")
      spark <- if (is.null(h)) "" else cot_spark(cot_hist_rows(h, m, sp$group), cot_hist_rows(h, m, ot$group))
      paste0("<tr><td class='hz-l'>", esc(m), "</td>",
             cot_score_td(sp$net_idx_3y, paste0(sp$group, ": 1y ", sp$net_idx_1y, ", 3y ", sp$net_idx_3y, ", 5y ", sp$net_idx_5y)),
             cot_score_td(ot$net_idx_3y, paste0(ot$group, ": 1y ", ot$net_idx_1y, ", 3y ", ot$net_idx_3y, ", 5y ", ot$net_idx_5y)),
             cot_score_td(rt$net_idx_3y, paste0("retail: 1y ", rt$net_idx_1y, ", 3y ", rt$net_idx_3y, ", 5y ", rt$net_idx_5y)),
             "<td class='im-num'>", cot_k(sp$net, TRUE), "</td>", cot_wow_td(sp$net_wow),
             "<td class='cot-spark-td'>", spark, "</td></tr>")
    }, "")
    paste0("<tr><td colspan='7' class='im-h3'>", esc(s), "</td></tr>", paste(rows, collapse = ""))
  }, "")
  sprintf("<div class='im-tw'><table class='hz-table cot-ov'>%s%s</table></div>", head, paste(body, collapse = ""))
}

#' Per-sector blocks of market cards (speculators' detail chart)
cot_cards <- function(d, h) {
  sp_all <- d[d$spec %in% c(TRUE, "TRUE"), ]
  secs <- intersect(c(COT_SECTOR_ORDER, unique(sp_all$sector)), unique(sp_all$sector))
  paste(vapply(secs, function(s) {
    ds <- sp_all[sp_all$sector == s, ]
    cards <- vapply(seq_len(nrow(ds)), function(i) {
      r <- ds[i, ]
      sprintf("<div class='cot-card'><div class='cot-card-h'><b>%s</b> <span class='im-sub'>%s: long %s, short %s, net %s; 3y index %.0f</span></div>%s</div>",
              esc(r$market), esc(r$group), cot_k(r$long), cot_k(r$short), cot_k(r$net, TRUE), r$net_idx_3y,
              cot_detail_svg(cot_hist_rows(h, r$market, r$group)))
    }, "")
    sprintf("<details class='im-details'><summary>%s &mdash; speculators' longs, shorts and net, 5 years</summary><div class='cot-cards'>%s</div></details>",
            esc(s), paste(cards, collapse = ""))
  }, ""), collapse = "")
}

cot_dashboard_html <- function(d, h = load_cot_history()) {
  if (is.null(d) || nrow(d) == 0)
    return("<div class='no-data'>No COT dashboard file &mdash; run refresh_cot.R</div>")
  as_of <- max(d$report_date)
  spec <- d[d$spec %in% c(TRUE, "TRUE"), ]
  other <- d[d$group %in% COT_OTHER_SIDE, ]
  legacy <- d[d$group == "[legacy] large spec", ]
  paste0(
    sprintf("<div class='im-sub'>CFTC report of %s (Tuesday close). Specs: managed money for commodities (disaggregated report), leveraged funds for financials (Traders in Financial Futures). Other side: commercials / asset managers. Small: retail (non-reportable).</div>", as_of),
    cot_overview(d, h),
    if (is.null(h)) "<div class='no-data'>No COT history file &mdash; run refresh_cot.R</div>" else cot_cards(d, h),
    "<details class='im-details'><summary>Tables: specs' net, long and short index (1y, 3y, 5y) and week on week</summary>",
    cot_table(spec), "</details>",
    "<details class='im-details'><summary>Tables: the other side &mdash; commercials (commodities) and asset managers (financials)</summary>",
    cot_table(other), "</details>",
    "<details class='im-details'><summary>Tables: legacy large speculators (managed money + other reportables; the COTSignal series)</summary>",
    cot_table(legacy), "</details>",
    leg_note("11 &mdash; COT positioning", paste0("Score = 3y net index = 100 &times; (current net &minus; lowest net) / (highest &minus; lowest) over the last 156 weekly reports. ",
    "100 = the group's largest net long of the window, 0 = its largest net short (or smallest net long). Same meaning in every column: ",
    "blue = more long than usual for that group (60&ndash;70 light, 70&ndash;85 medium, 85 or more strong), red = less long (40&ndash;30, 30&ndash;15, 15 or less); hover a cell for 1y / 3y / 5y. ",
    "Row chart: net contracts (long minus short) of specs (blue) and the other side (black) over the last 260 reports, one scale per row, dashed line = zero, vertical lines = 1 January. ",
    "Sector charts: specs' long (blue), short (red) and net (black) contracts, then their 3y index week by week (shaded 85&ndash;100 and 0&ndash;15; starts once 156 reports exist). ",
    "Tables: Index = the same formula over 52, 156 or 260 reports for net, long and short; shaded 90 or more, 10 or less; week on week compares with the previous report. ",
    "Sources: Reports/cot_positioning_latest.csv (latest week), output/cot_history.csv (weekly series).")))
}
