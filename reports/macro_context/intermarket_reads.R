# intermarket_reads.R — "the global movie": one paragraph per market, in reading order,
# then how the pieces fit together (scenario match). Facts come from the panels and z-scores.

find_m <- function(sections, sym) {
  for (s in sections) for (m in c(s$instruments, s$ratios)) if (identical(m$sym, sym)) return(m)
  NULL
}
tr <- function(m) if (is.null(m) || is.na(m$trend)) "n/a" else m$trend
pct <- function(m) if (is.null(m) || is.na(m$c1m)) "n/a" else sprintf("%+.1f%%", m$c1m)

#' Strength word from a 1-month z-score
zword <- function(zv) {
  if (is.na(zv)) return("")
  if (abs(zv) >= 2) "an unusually large" else if (abs(zv) >= 1) "a clear" else if (abs(zv) >= 0.5) "a modest" else "no meaningful"
}

build_movie <- function(sections, breadth, z, matches) {
  g <- function(s) find_m(sections, s)
  p <- list()

  spx <- g("^GSPC"); ndx <- g("^NDX/^GSPC"); ew <- g("RSP/^GSPC"); vix <- g("^VIX"); cons <- g("XLY/XLP")
  p$stocks <- paste0(
    sprintf("S&amp;P 500 %s over a month, %s move for this index, trend %s at %.0f%% of its 52-week range. ",
            pct(spx), zword(z["SPX"]), tr(spx), spx$pos52),
    if (!is.null(breadth)) sprintf("Only %.0f%% of its stocks are above their 50-day average. ", breadth$pct) else "",
    sprintf("Equal weight vs cap weight is %s (%s), Nasdaq 100 vs S&amp;P %s, discretionary vs staples %s. ",
            tr(ew), pct(ew), tr(ndx), tr(cons)),
    sprintf("VIX at %.1f, trend %s.", vix$last, tr(vix)))

  fx_keys <- c("EUR", "GBP", "JPY", "CHF", "AUD", "CAD", "EMFX", "CNY")
  weak <- fx_keys[!is.na(z[fx_keys]) & z[fx_keys] <= -0.5]
  strong <- fx_keys[!is.na(z[fx_keys]) & z[fx_keys] >= 0.5]
  dxy <- g("DX-Y.NYB")
  p$fx <- paste0(
    sprintf("Dollar index %s over a month, %s move. ", pct(dxy), zword(z["USD"])),
    if (length(weak)) sprintf("Weaker against the dollar: %s. ", paste(vapply(weak, fp_label, ""), collapse = ", ")) else "",
    if (length(strong)) sprintf("Stronger: %s. ", paste(vapply(strong, fp_label, ""), collapse = ", ")) else "",
    if (length(weak) >= 5 && z["USD"] >= 1) "The dollar is rising against almost everything: a broad dollar bid, not a single-currency story. " else "",
    sprintf("AUD/JPY, the carry and risk barometer, is %s.", tr(g("AUDJPY=X"))))

  gold <- g("GC=F"); gs <- g("GC=F/SI=F"); mg <- g("GDX/GC=F")
  p$metals <- paste0(
    sprintf("Gold %s, %s move, trend %s. ", pct(gold), zword(z["GOLD"]), tr(gold)),
    if (!is.na(z["GOLD"]) && !is.na(z["USD"]) && z["GOLD"] <= -0.5 && z["USD"] >= 0.5)
      "Gold falling with a rising dollar: it is being treated as a currency that loses to the dollar, or sold to raise cash. " else "",
    if (!is.na(z["GOLD"]) && !is.na(z["USD"]) && z["GOLD"] >= 0.5 && z["USD"] >= 0.5)
      "Gold rising together with the dollar: a fear bid, not a currency trade. " else "",
    sprintf("Gold/silver %s, miners vs gold %s.", tr(gs), tr(mg)))

  oil <- g("CL=F"); brent <- g("BZ=F"); eo <- g("XLE/CL=F")
  p$oil <- paste0(
    sprintf("WTI %s (%s move), Brent %s; trend %s. ", pct(oil), zword(z["OIL"]), pct(brent), tr(oil)),
    if (!is.na(z["OIL"]) && !is.na(z["USD"]) && z["OIL"] >= 0.5 && z["USD"] >= 0.5)
      "Oil up while the dollar is up: oil importers pay twice (higher price, dearer dollars), which adds to global dollar demand. " else "",
    sprintf("Energy stocks vs crude %s.", tr(eo)))

  cu <- g("HG=F"); cg <- g("HG=F/GC=F")
  p$commod <- paste0(
    sprintf("Copper %s (%s move), copper/gold %s. ", pct(cu), zword(z["COPPER"]), tr(cg)),
    sprintf("Wheat %s, corn %s, broad commodities %s.", pct(g("ZW=F")), pct(g("ZC=F")), pct(g("DBC"))))

  tny <- g("^TNX"); cur <- g("^TNX-^IRX"); cr <- g("HYG/IEF"); mv <- g("^MOVE")
  glob <- c("US10Y", "BUND", "GILT")
  up_all <- all(!is.na(z[glob]) & z[glob] >= 0.5)
  p$rates <- paste0(
    sprintf("US 10-year at %.2f%%, %+.0f bp over a month (%s move); 10y minus 3m %.2f pt, trend %s. ",
            tny$last, tny$c1m, zword(z["US10Y"]), cur$last, tr(cur)),
    sprintf("German long Bunds %s, UK long gilts %s (bond prices). ", pct(g("IS0L.DE")), pct(g("GLTL.L"))),
    if (up_all) "Yields are rising in the US, Germany and the UK together: a global bond move, not a US-specific one. " else "",
    sprintf("MOVE %.0f (trend %s); high yield vs Treasuries %s.", mv$last, tr(mv), tr(cr)))

  wl <- sections[[which(vapply(sections, `[[`, "", "id") == "world")]]$instruments
  w <- function(syms) paste(vapply(Filter(function(m) m$sym %in% syms, wl),
                                   function(m) sprintf("%s %s", m$label, pct(m)), ""), collapse = ", ")
  p$world <- paste0(
    "Europe: ", w(c("^STOXX50E", "^GDAXI", "^SSMI", "^FCHI", "^IBEX", "FTSEMIB.MI")), ". ",
    "Latin America: ", w(c("^BVSP", "^MXX")), ". ",
    "Asia: ", w(c("^N225", "^KS11", "^HSCE", "000001.SS")), " (local currency, 1 month).")

  # How it fits together
  top <- matches[[1]]
  fit <- vapply(matches[1:3], function(m) sprintf("%s %+.0f%% (3 months: %+.0f%%, %s%s)", m$name, 100 * m$score,
    100 * m$score3m, tolower(m$age$status), if (m$age$run > 0) sprintf(", %d days", m$age$run) else ""), "")
  p$fit <- paste0(
    sprintf("Closest scenarios: %s. ", paste(fit, collapse = "; ")),
    sprintf("For the closest one, %s, the moves that fit are: %s. ", top$name,
            if (length(top$agree)) paste(vapply(top$agree, function(k) describe_z(k, z[k]), ""), collapse = "; ") else "none"),
    if (length(top$against)) sprintf("What does not fit (the present is not a repeat): %s.",
                                     paste(vapply(top$against, function(k) describe_z(k, z[k]), ""), collapse = "; ")) else
      "No asset contradicts the fingerprint.",
    if (nzchar(top$age$text)) paste0(" ", top$age$text) else "")
  p
}
