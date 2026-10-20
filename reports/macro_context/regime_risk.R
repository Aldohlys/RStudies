# regime_risk.R — the two badges at the top of the report and the BOT framework they select.
#
# Regime: direction of the section-00 scenarios in place (status from scenario_age(), archetypes.R;
#         a scenario counts as in place only from MIN_IN_PLACE sessions in a row, before that it is PENDING).
# Risk:   VIX and the US high-yield spread, each "on" from a 20-session z-score of +1 and "off" again below +0.5;
#         CALM = both off, ELEVATED = one on, STRESS = both on. Thresholds provisional (2026-10-20).
# Framework: the user's fixed BOT rule for each regime x risk cell (wording confirmed 2026-10-20).
# Band:   the 60-session "scenario in place" band uses the same 3-session rule.
# Replaces the regime model's Daily Bias (scenarios.R::synthesize), dropped with sections 05-07 on 2026-10-20.

MIN_IN_PLACE <- 3          # sessions in a row with state >= STATE_ACTIVE before a scenario counts as in place
BAND_MAX_GAP <- 4          # band: a gap up to this many sessions between two runs of the same scenario is bridged

# Scenario id -> what it means for long-equity / BOT long entries
REGIME_DIR <- c(
  dash_for_cash = "DEFENSIVE", dollar_wrecking_ball = "DEFENSIVE", bond_rout = "DEFENSIVE",
  stagflation = "DEFENSIVE", growth_scare = "DEFENSIVE", em_china_shock = "DEFENSIVE",
  credit_event = "DEFENSIVE", leadership_unwind = "DEFENSIVE",
  reflation = "LONG BIAS", goldilocks = "LONG BIAS", policy_pivot = "LONG BIAS", narrow_mania = "LONG BIAS",
  debasement = "NEUTRAL")
IN_PLACE_STATUS <- c("BUILDING", "ESTABLISHED", "MATURE", "FADING")

regime_dir <- function(id) if (id %in% names(REGIME_DIR)) REGIME_DIR[[id]] else "NEUTRAL"

#' Regime from the scenario matches (statuses already carry the PENDING rule)
regime_state <- function(matches) {
  st <- vapply(matches, function(m) m$age$status, "")
  inplace <- matches[st %in% IN_PLACE_STATUS]
  dirs <- setdiff(unique(vapply(inplace, function(m) regime_dir(m$id), "")), "NEUTRAL")
  regime <- if (!length(dirs)) "NEUTRAL" else if (length(dirs) > 1) "MIXED" else dirs
  zone <- switch(regime, DEFENSIVE = "RED", "LONG BIAS" = "GREEN", "ORANGE")
  list(regime = regime, zone = zone, row = switch(regime, DEFENSIVE = "def", "LONG BIAS" = "long", "neu"),
       inplace = inplace, pending = matches[st == "PENDING"], emerging = matches[st == "EMERGING"])
}

RISK_ON <- 1; RISK_OFF <- 0.5; RISK_N <- 20; RISK_SD_WIN <- 252

#' 20-session z-score history of a series: change / (SD of daily changes over the prior 252 sessions x sqrt 20)
risk_z <- function(d, log_scale) {
  if (is.null(d) || nrow(d) < RISK_SD_WIN + RISK_N) return(NULL)
  d <- d[order(d$date), ]
  x <- if (log_scale) log(d$Close) else d$Close
  dx <- c(NA, diff(x))
  n <- length(x)
  # SD of the daily changes over the RISK_SD_WIN sessions before each session (known before the day)
  sdv <- vapply(seq_len(n), function(i) if (i <= RISK_SD_WIN + 1) NA_real_ else stats::sd(dx[(i - RISK_SD_WIN):(i - 1)], na.rm = TRUE), 0)
  z <- (x - c(rep(NA, RISK_N), head(x, -RISK_N))) / (sdv * sqrt(RISK_N))
  data.frame(date = d$date, z = z)
}

#' On / off with hysteresis: on from z >= RISK_ON, off again below RISK_OFF
risk_onoff <- function(z) {
  on <- FALSE
  vapply(z, function(v) { if (!is.na(v)) { if (v >= RISK_ON) on <<- TRUE else if (v < RISK_OFF) on <<- FALSE }; on }, FALSE)
}

#' Risk badge from VIX (intermarket raw) and the FRED high-yield spread
risk_state <- function(vix, hy) {
  part <- function(d, log_scale, label) {
    zz <- risk_z(d, log_scale)
    if (is.null(zz) || all(is.na(zz$z))) return(list(on = FALSE, z = NA_real_, txt = sprintf("%s n/a", label), date = NA))
    on <- risk_onoff(zz$z)
    k <- max(which(!is.na(zz$z)))
    list(on = on[k], z = zz$z[k], date = zz$date[k],
         txt = sprintf("%s z %+.1f (%s)", label, zz$z[k], if (on[k]) "on" else "off"))
  }
  v <- part(vix, TRUE, "VIX"); h <- part(hy, FALSE, "high-yield spread")
  n_on <- v$on + h$on
  level <- c("CALM", "ELEVATED", "STRESS")[n_on + 1]
  asof <- suppressWarnings(max(as.Date(c(v$date, h$date)), na.rm = TRUE))
  list(level = level, zone = c(CALM = "GREEN", ELEVATED = "ORANGE", STRESS = "RED")[[level]],
       col = if (level == "CALM") "calm" else "hot",
       why = sprintf("%s, %s%s", v$txt, h$txt, if (is.finite(asof)) sprintf("; as of %s", format(asof, "%d %b")) else ""))
}

# The user's BOT framework per regime x risk cell (wording confirmed 2026-10-20)
FRAMEWORK <- list(
  long = c(calm = "Normal operation: usual number of trades and size.",
           hot  = "Usual selection, reduced size, no entries during spike."),
  neu  = c(calm = "Usual size, no sector tilt.",
           hot  = "Usual selection, reduced size, no entries during spike."),
  def  = c(calm = "Fewer trades, longs (shorts) only in groups the scenario expects to rise (fall).",
           hot  = "Limit trades, smallest size."))
FRAMEWORK_ROWS <- c(long = "Regime long<br><span>goldilocks, reflation, policy pivot, narrow leadership</span>",
                    neu  = "Neutral / mixed",
                    def  = "Regime defensive<br><span>stagflation, bond rout, growth scare, dollar squeeze, credit event, EM/China, leadership unwind</span>")

#' What followed historically in each cell (Legend M7). Source: NewTrading/scripts/macro_forces/daily_check.py,
#' Fridays 2008-2026, 20 sessions ahead, conditions = the 7-force scenarios of the step-2 backtest.
FRAMEWORK_HISTORY <- list(
  long = c(calm = "Goldilocks (stress falling, no rate/oil/defensive pressure): QQQ higher 20 sessions later in 79% of cases vs 66% for all weeks (61 Fridays, 38 episodes).",
           hot  = "After a VIX spike (z &ge; 1): VIX lower 20 sessions later in 77% of cases vs 52% for all weeks (73 Fridays)."),
  neu  = c(calm = "Not tested.",
           hot  = "Credit weak (high-yield z &le; &minus;1): SPY lower 20 sessions later in 42% of cases vs 32% for all weeks (102 Fridays, 35 episodes; p 0.06)."),
  def  = c(calm = "Stagflation: XLY behind XLP 20 sessions later in 75% of cases vs 46% for all weeks (16 Fridays, 19 episodes). Other defensive scenarios: no measurable edge or not tested.",
           hot  = "Liquidity stress with weak credit: HYG higher 20 sessions later in 86% of cases vs 64% for all weeks (14 Fridays, 11 episodes); SPY lower in 29% vs 32% for all weeks (no edge)."))

#' Top bar: Regime badge, Risk badge, framework line
regime_bar_html <- function(rg, rk) {
  top <- if (length(rg$inplace)) short_name(rg$inplace[[1]]$name) else "no scenario in place"
  pend <- if (length(rg$pending)) paste0("; pending: ", paste(vapply(rg$pending, function(m)
    sprintf("%s (%d of %d sessions)", short_name(m$name), m$age$run, MIN_IN_PLACE), ""), collapse = ", ")) else ""
  sprintf(paste0("<div class='bias-bar'><span class='bias-label'>Regime</span><span class='bias-badge %s'>%s</span>",
                 "<span class='bias-explain'>%s%s</span>",
                 "<span class='bias-label' style='margin-left:18px'>Risk</span><span class='bias-badge %s'>%s</span>",
                 "<span class='bias-explain'>%s</span>",
                 "<div class='bias-read'><b>Framework:</b> %s</div></div>"),
          badge_class(rg$zone), rg$regime, esc(top), esc(pend), badge_class(rk$zone), rk$level, rk$why,
          FRAMEWORK[[rg$row]][[rk$col]])
}

#' Regime x risk table (summary), today's cell outlined
framework_table_html <- function(rg, rk) {
  cell <- function(r, c) sprintf("<td class='rr-cell%s'>%s</td>", if (r == rg$row && c == rk$col) " rr-on" else "", FRAMEWORK[[r]][[c]])
  rows <- paste(vapply(names(FRAMEWORK_ROWS), function(r)
    sprintf("<tr><th>%s</th>%s%s</tr>", FRAMEWORK_ROWS[[r]], cell(r, "calm"), cell(r, "hot")), ""), collapse = "")
  em <- if (length(rg$emerging)) sprintf("<div class='rr-foot'>Emerging: %s</div>", esc(paste(vapply(rg$emerging, function(m)
    sprintf("%s (%s)", short_name(m$name), tolower(regime_dir(m$id))), ""), collapse = ", "))) else ""
  paste0("<div class='rr-card'><div class='ck-h'>Regime &times; risk &mdash; BOT framework; today's cell outlined</div>",
         "<table class='rr'><tr><th></th><th>Risk calm</th><th>Risk elevated / stress</th></tr>", rows, "</table>", em,
         leg_note("Summary &mdash; regime &times; risk table", paste0(
           "Regime: direction of the 00 scenarios in place (emerging ones are listed, not counted). ",
           "Risk: VIX and high-yield spread 20-session z-scores, on at z &ge; 1, off below 0.5; one on = elevated, both = stress. ",
           "Thresholds provisional. What followed historically in each cell: Legend, M7. Framework lines are the user's rules.")),
         "</div>")
}

#' Legend M7: what followed historically in each cell
md_framework_history <- function() {
  lab <- c(long = "Regime long", neu = "Neutral / mixed", def = "Regime defensive")
  rows <- paste(vapply(names(lab), function(r) sprintf("<tr><th>%s</th><td>%s</td><td>%s</td></tr>", lab[[r]],
    FRAMEWORK_HISTORY[[r]][["calm"]], FRAMEWORK_HISTORY[[r]][["hot"]]), ""), collapse = "")
  paste0(
    "<p>Each figure compares the Fridays on which the condition was active with all Fridays: share of cases where the asset moved ",
    "the stated way over the next 20 sessions (about 4 weeks), against the same share for all Fridays (the base rate). ",
    "The difference is what the condition adds. Direction only, not size. Consecutive Fridays share most of their window, ",
    "so the effective sample is closer to the number of episodes.</p>",
    "<table class='rr'><tr><th></th><th>Risk calm</th><th>Risk elevated / stress</th></tr>", rows, "</table>",
    "<p class='rr-foot'>Source: NewTrading/scripts/macro_forces/daily_check.py, Yahoo adjusted closes and FRED, Fridays 2008&ndash;2026. ",
    "Conditions are the 7-force scenarios of the step-2 backtest (NewTrading/Reports/macro_scenarios_backtest_20261020.md), ",
    "close to but not the same as the 13 scenarios of section 00. Descriptive history only.</p>")
}

#' Band labels: per session, the top scenario in place by state; runs shorter than MIN_IN_PLACE dropped,
#' gaps up to BAND_MAX_GAP between two runs of the same scenario bridged, a short run still going today = pending.
#' @return data.frame(id, a, b, pending) of runs (row indices into dates)
band_runs <- function(matches, n) {
  st <- do.call(cbind, lapply(matches, function(m) if (length(m$hist) == n) m$hist else rep(NA_real_, n)))
  ids <- vapply(matches, `[[`, "", "id")
  top <- apply(st, 1, function(r) { if (all(is.na(r))) return(NA_character_); j <- which.max(r); if (r[j] >= STATE_ACTIVE) ids[j] else NA_character_ })
  r <- rle(ifelse(is.na(top), "", top)); b <- cumsum(r$lengths); a <- b - r$lengths + 1
  runs <- data.frame(id = r$values, a = a, b = b, stringsAsFactors = FALSE)
  runs <- runs[runs$id != "" & (runs$b - runs$a + 1 >= MIN_IN_PLACE | runs$b == n), , drop = FALSE]
  out <- runs[0, ]
  for (i in seq_len(nrow(runs))) {
    k <- nrow(out)
    if (k && out$id[k] == runs$id[i] && runs$a[i] - out$b[k] - 1 <= BAND_MAX_GAP) out$b[k] <- runs$b[i]
    else out <- rbind(out, runs[i, ])
  }
  out$pending <- out$b == n & out$b - out$a + 1 < MIN_IN_PLACE
  out
}
