# event_history.R — scheduled events and what followed them historically (section 08 rows, summary tiles).
#
# Source of every figure: NewTrading/scripts/macro_forces/event_study.py, report
# NewTrading/Reports/macro_events_study_20261020.md (daily Yahoo + FRED, 2008-2026). Descriptive history only.
# FOMC dates: scheduled decisions (last day of each meeting) from federalreserve.gov, copied from
# NewTrading/scripts/macro_forces/events/fomc_dates.csv. Extend the list when the Fed publishes the next year.

FOMC_DATES <- as.Date(c(
  "2025-01-29", "2025-03-19", "2025-05-07", "2025-06-18", "2025-07-30", "2025-09-17", "2025-10-29", "2025-12-10",
  "2026-01-28", "2026-03-18", "2026-04-29", "2026-06-17", "2026-07-29", "2026-09-16", "2026-10-28", "2026-12-09",
  "2027-01-27", "2027-03-17", "2027-04-28", "2027-06-09", "2027-07-28", "2027-09-15", "2027-10-27", "2027-12-08"))

MAG7 <- c("NVDA", "MSFT", "AAPL", "AMZN", "GOOGL", "META", "TSLA")
EVENT_LOOKAHEAD <- 28      # calendar days (about 20 sessions) for the FOMC and Mag 7 rows of section 08
EVENT_SRC <- "Source: NewTrading/scripts/macro_forces/event_study.py."

FOMC_HISTORY <- paste(
  "What followed historically (149 scheduled decisions, 2008&ndash;2026):",
  "SPY realized volatility over the 5 sessions after the decision was a median 1.17&times; the 5 sessions before",
  "(0.99&times; for all sessions; p &asymp; 0.002).",
  "Large 2-day SPY reactions (&gt; 1 SD) moved further the same way over the next 20 sessions in 31% of cases",
  "vs 52% for all sessions (42 decisions).",
  "SPY from 5 sessions before to the day before: no difference from other weeks.", EVENT_SRC)

MAG7_HISTORY <- paste(
  "What followed historically (Mag 7 reaction sessions, 2023&ndash;2026): QQQ/RSP moved 98 bp on reaction sessions",
  "vs 69 bp on all sessions (mean absolute daily move).",
  "The stock's reaction did not predict QQQ/RSP or SPY over the next 1, 5 or 20 sessions",
  "(pooled 49&ndash;55% in the reaction's direction vs 49&ndash;51% base, 105 reactions).")
NVDA_EXTRA <- "NVDA: correlation of its reaction with the same-session QQQ/RSP move +0.87 (15 reactions)."

# ── FOMC-made moves on the summary tiles ─────────────────────────────────────
FOMC_MADE_SHARE <- 0.5     # decision session + next session made at least this share of the 1-month move

FOMC_POOLED <- paste(
  "All 7 forces pooled: crossings of |z| = 1 made &ge; 50% on a decision session + the next one moved further the same way",
  "in 36% / 35% / 34% of cases over 5 / 10 / 20 sessions, vs 47% when a random 2-day block made the move",
  "(137 crossings, 59 decisions, p &asymp; 0.04).")
# Per tile (FP key): FOMC-made vs broad crossings, continuation at 5 / 10 / 20 sessions. Small samples.
FOMC_BY_FORCE <- c(
  VIX   = "VIX: 19% / 31% / 13% vs 40% / 51% / 43% for crossings spread over the month (16 FOMC-made crossings).",
  US10Y = "Tested on the 10-year real yield: 33% / 38% / 45% vs 39% / 37% / 47% (40 FOMC-made crossings).",
  USD   = "Dollar: 54% / 36% / 32% vs 48% / 48% / 54% (28 FOMC-made crossings).",
  GOLD  = "Gold: 28% / 24% / 32% vs 61% / 62% / 57% (25 FOMC-made crossings).",
  OIL   = "Oil: decision days are ordinary days for oil; 4 FOMC-made crossings, not tested.")

#' Share of the last n-session move made on FOMC decision sessions and the session after.
#' @param dates,close the tile's daily series; kind "yield"/"usd" = changes in points, else log changes
#' @return list(share, dates) or NULL when no decision falls in the window
fomc_share <- function(dates, close, kind, n = 21) {
  ok <- !is.na(close)
  dates <- dates[ok]; close <- close[ok]
  k <- length(close)
  if (k < n + 1) return(NULL)
  lvl <- kind %in% c("yield", "usd")
  if (!lvl && any(close <= 0)) return(NULL)
  dx <- if (lvl) diff(close) else diff(log(close))
  w <- (k - n):(k - 1)                          # indices into dx for the last n daily changes
  wd <- dates[w + 1]                           # session each change belongs to
  dpos <- match(FOMC_DATES, dates)
  dpos <- dpos[!is.na(dpos)]
  ev <- unique(c(dpos, dpos + 1))              # decision session and the next one (gold, oil settle before 14:00 ET)
  inw <- (w + 1) %in% ev
  if (!any(inw)) return(NULL)
  tot <- sum(dx[w])
  if (tot == 0) return(NULL)
  list(share = sum(dx[w][inw]) / tot, dates = FOMC_DATES[FOMC_DATES %in% wd])
}

#' Tile annotation: "FOMC-made" tag and the history tooltip text, or NULL
fomc_tile_note <- function(fp, dates, close, kind, z) {
  if (is.na(z) || abs(z) < 1) return(NULL)
  old_lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc), add = TRUE)
  s <- fomc_share(dates, close, kind)
  if (is.null(s) || s$share < FOMC_MADE_SHARE) return(NULL)
  hist <- if (fp %in% names(FOMC_BY_FORCE)) FOMC_BY_FORCE[[fp]] else "This market: not tested."
  list(tag = if (s$share > 1) "FOMC-made &gt;100%" else sprintf("FOMC-made %.0f%%", 100 * s$share),  # >100%: rest of the month moved the other way
       tip = sprintf("%.0f%% of the 1-month move came on the FOMC decision session(s) %s and the next session. %s %s %s",
                     100 * s$share, paste(format(s$dates, "%d %b"), collapse = ", "), FOMC_POOLED, hist, EVENT_SRC))
}

# ── Section 08 rows ──────────────────────────────────────────────────────────

#' Mag 7 announcement dates within EVENT_LOOKAHEAD days, from Yahoo via yfinance.
#' Runs the conda Python (has yfinance + lxml; the reticulate env lacks lxml, which get_earnings_dates needs).
#' Returns list() on any failure, so the section shows FOMC rows only.
MAG7_PYTHON <- "C:/Users/aldoh/miniconda3/python.exe"
mag7_dates <- function(today = Sys.Date()) {
  if (!file.exists(MAG7_PYTHON)) return(list())
  py <- tempfile(fileext = ".py")
  on.exit(unlink(py), add = TRUE)
  writeLines(c(
    "import yfinance as yf",
    sprintf("for t in %s:", paste0("['", paste(MAG7, collapse = "','"), "']")),
    "    try:",
    "        for x in yf.Ticker(t).get_earnings_dates(limit=4).index.strftime('%Y-%m-%d %H'):",
    "            print(t, x)",
    "    except Exception as e:",
    "        pass"), py)
  s <- tryCatch(suppressWarnings(system2(MAG7_PYTHON, shQuote(py), stdout = TRUE, stderr = FALSE)),
                error = function(e) character(0))
  s <- grep("^[A-Z]+ [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}$", s, value = TRUE)
  if (!length(s)) return(list())
  p <- do.call(rbind, strsplit(s, " "))
  d <- as.Date(p[, 2]); hr <- as.integer(p[, 3])
  keep <- which(!is.na(d) & d >= today & d <= today + EVENT_LOOKAHEAD & !duplicated(paste(p[, 1], p[, 2])))
  lapply(keep, function(i) list(ticker = p[i, 1], d = d[i], after = !is.na(hr[i]) && hr[i] >= 12))
}

#' FOMC decision and Mag 7 earnings rows for section 08 (boost = FALSE: no effect on scenario scores)
event_history_rows <- function(today = Sys.Date()) {
  old_lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc), add = TRUE)
  rows <- list()
  for (d in as.list(FOMC_DATES[FOMC_DATES >= today & FOMC_DATES <= today + EVENT_LOOKAHEAD]))
    rows[[length(rows) + 1]] <- list(date = format(d, "%b-%d"), d = d, event = "FOMC decision, statement 14:00 ET (USD)",
                                     impact = "CRITICAL", action = FOMC_HISTORY, boost = FALSE)
  m7 <- tryCatch(mag7_dates(today), error = function(e) list())
  for (m in m7) {
    when <- if (m$after) "after the close; reaction next session" else "before the open; reaction same session"
    rows[[length(rows) + 1]] <- list(date = format(m$d, "%b-%d"), d = m$d,
      event = sprintf("%s earnings (%s; Yahoo date, may be an estimate)", m$ticker, when), impact = "HIGH",
      action = paste(MAG7_HISTORY, if (m$ticker == "NVDA") NVDA_EXTRA else "", EVENT_SRC), boost = FALSE)
  }
  rows
}
