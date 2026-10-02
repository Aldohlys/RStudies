# refresh_cot.R — regenerate positioning.R from the CFTC weekly archives.
#
# Replaces the hand-curation that let the file drift 14 weeks (see the
# staleness banner added alongside this script). Everything in positioning.R
# is derived here: the position numbers, the week-on-week change, the 5-year
# percentile, the extreme flags and the notes.
#
# Source: the CFTC annual archives, which are refreshed weekly and already
# contain the newest release, so one source covers both the current values and
# the multi-year context needed for the percentiles. No HTML scraping.
#   Disaggregated (WTI, gold, copper, grains): fut_disagg_txt_<YYYY>.zip
#   Traders in Financial Futures (DXY):        fut_fin_txt_<YYYY>.zip
#
# Extreme rule: TRUE when the current net sits at or beyond the 5-year 90th
# percentile (crowded long) or 10th percentile (crowded short).
#
# Side-output: NewTrading/Reports/cot_actors_latest.csv -- the latest week for
# every trader category (producer, swap dealer, managed money, other
# reportables, retail) per contract, with long / short / net and the
# week-on-week change in each, plus derived legacy commercial / large-spec /
# small-spec rollups. positioning.R keeps only the one speculative net per
# asset that the regime score consumes; this is the detail behind it.
#
# Side-output: NewTrading/Reports/cot_positioning_latest.csv -- the positioning
# dashboard (38 markets x 5 trader groups): long / short / net, week-on-week,
# and the 1y / 3y / 5y COT index of each leg. Rendered as section 11 of the
# macro_context report (cot_render.R). Its "[legacy] large spec" rows match
# cotsignal.com exactly (checked 2026-10-02 on 14 markets: net, OI, indices).
#
# Run: Rscript RStudies/reports/macro_context/refresh_cot.R [--dry-run]
# Scheduled Saturday 08:00 via RApplication/scripts/RefreshCOT.xml — the CFTC
# releases Friday 15:30 ET (21:30 CET), so Saturday morning is clear of it.

suppressPackageStartupMessages(library(data.table))

DRY_RUN <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)

.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(dirname(sub("^--file=", "", file_arg[1]))))
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) return(normalizePath(dirname(f)))
  }
  getwd()
}
SCRIPT_DIR <- .get_script_dir()
TARGET     <- file.path(SCRIPT_DIR, "positioning.R")
CACHE_DIR  <- file.path(SCRIPT_DIR, "output", "cot_cache")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# Six past years: the dashboard's 5-year index needs 260 weekly rows even in January.
YEARS <- (as.integer(format(Sys.Date(), "%Y")) - 6):as.integer(format(Sys.Date(), "%Y"))

# Contract codes are matched with leading zeros stripped — fread types the
# column as integer whenever a year's file happens to hold only numeric codes.
.norm_code <- function(x) sub("^0+", "", trimws(as.character(x)))

# ── Assets ────────────────────────────────────────────────────────────────────
# Grains is a basket: its legs are summed before the percentile is taken, so the
# percentile describes the combined position the entry actually reports.
ASSETS <- list(
  list(label = "Crude Oil", sector = "Energy",        src = "disagg", codes = "067651"),
  list(label = "USD",       sector = "Macro",         src = "tff",    codes = "098662"),
  list(label = "Gold",      sector = "PreciousMetals", src = "disagg", codes = "088691"),
  list(label = "Grains",    sector = "Agriculture",   src = "disagg",
       codes = c(Corn = "002602", Soy = "005602", `SRW wheat` = "001602")),
  list(label = "Copper",    sector = "Materials",     src = "disagg", codes = "085692")
)

# Trader categories carried into the CSV side-output. Note the CFTC's own
# double-underscore typo in two Swap column names -- it is in the files, not here.
CATS_DISAGG <- list(
  producer      = c("Prod_Merc_Positions_Long_All",  "Prod_Merc_Positions_Short_All",  NA),
  swap_dealer   = c("Swap_Positions_Long_All",       "Swap__Positions_Short_All",       "Swap__Positions_Spread_All"),
  managed_money = c("M_Money_Positions_Long_All",    "M_Money_Positions_Short_All",    "M_Money_Positions_Spread_All"),
  other_rept    = c("Other_Rept_Positions_Long_All", "Other_Rept_Positions_Short_All", "Other_Rept_Positions_Spread_All"),
  retail        = c("NonRept_Positions_Long_All",    "NonRept_Positions_Short_All",    NA)
)
CATS_TFF <- list(
  dealer        = c("Dealer_Positions_Long_All",     "Dealer_Positions_Short_All",     "Dealer_Positions_Spread_All"),
  asset_manager = c("Asset_Mgr_Positions_Long_All",  "Asset_Mgr_Positions_Short_All",  "Asset_Mgr_Positions_Spread_All"),
  lev_money     = c("Lev_Money_Positions_Long_All",  "Lev_Money_Positions_Short_All",  "Lev_Money_Positions_Spread_All"),
  other_rept    = c("Other_Rept_Positions_Long_All", "Other_Rept_Positions_Short_All", "Other_Rept_Positions_Spread_All"),
  retail        = c("NonRept_Positions_Long_All",    "NonRept_Positions_Short_All",    NA)
)
.cat_cols <- function(cats) unique(stats::na.omit(unlist(cats, use.names = FALSE)))

SOURCES <- list(
  disagg = list(
    url  = "https://www.cftc.gov/files/dea/history/fut_disagg_txt_%d.zip",
    cols = c("Report_Date_as_YYYY-MM-DD", "CFTC_Contract_Market_Code",
             "Open_Interest_All", .cat_cols(CATS_DISAGG)),
    long = "M_Money_Positions_Long_All", short = "M_Money_Positions_Short_All",
    cats = CATS_DISAGG,
    who  = "MM"),
  tff = list(
    url  = "https://www.cftc.gov/files/dea/history/fut_fin_txt_%d.zip",
    cols = c("Report_Date_as_YYYY-MM-DD", "CFTC_Contract_Market_Code",
             "Open_Interest_All", .cat_cols(CATS_TFF)),
    long = c("Asset_Mgr_Positions_Long_All", "Lev_Money_Positions_Long_All"),
    short = c("Asset_Mgr_Positions_Short_All", "Lev_Money_Positions_Short_All"),
    cats = CATS_TFF,
    who  = "Lev+AM")
)

# One row per contract for the CSV: the grains basket is split into its legs,
# because at that level you want corn separately from soy.
CSV_CONTRACTS <- list(
  list(label = "Crude Oil", src = "disagg", code = "067651"),
  list(label = "Gold",      src = "disagg", code = "088691"),
  list(label = "Copper",    src = "disagg", code = "085692"),
  list(label = "Corn",      src = "disagg", code = "002602"),
  list(label = "Soybeans",  src = "disagg", code = "005602"),
  list(label = "SRW Wheat", src = "disagg", code = "001602"),
  list(label = "USD Index", src = "tff",    code = "098662")
)
CSV_OUT <- "C:/Users/aldoh/Documents/NewTrading/Reports/cot_actors_latest.csv"

# Positioning dashboard: the COTSignal-style view (long / short / net, each as a
# 1y / 3y / 5y COT index, plus week-on-week), built from the same CFTC files with
# the finer categories. One row per market x trader group; the macro_context
# report shows the speculative group (spec = TRUE) and the opposite side.
#   disagg markets: managed money (spec), commercials (producer + swap),
#                   [legacy] large spec (managed money + other reportables,
#                   the COTSignal / legacy "large speculators"), other, retail
#   tff markets:    leveraged funds (spec), asset managers, dealers, other, retail
DASH_CONTRACTS <- list(
  list(label = "WTI crude",         sector = "Energy",         src = "disagg", code = "067651"),
  list(label = "Brent crude",       sector = "Energy",         src = "disagg", code = "06765T"),
  list(label = "Natural gas",       sector = "Energy",         src = "disagg", code = "023651"),
  list(label = "RBOB gasoline",     sector = "Energy",         src = "disagg", code = "111659"),
  list(label = "Heating oil / ULSD", sector = "Energy",        src = "disagg", code = "022651"),
  list(label = "Gold",              sector = "Metals",         src = "disagg", code = "088691"),
  list(label = "Silver",            sector = "Metals",         src = "disagg", code = "084691"),
  list(label = "Copper",            sector = "Metals",         src = "disagg", code = "085692"),
  list(label = "Platinum",          sector = "Metals",         src = "disagg", code = "076651"),
  list(label = "Palladium",         sector = "Metals",         src = "disagg", code = "075651"),
  list(label = "Corn",              sector = "Grains",         src = "disagg", code = "002602"),
  list(label = "Soybeans",          sector = "Grains",         src = "disagg", code = "005602"),
  list(label = "Wheat (SRW)",       sector = "Grains",         src = "disagg", code = "001602"),
  list(label = "Soybean oil",       sector = "Grains",         src = "disagg", code = "007601"),
  list(label = "Soybean meal",      sector = "Grains",         src = "disagg", code = "026603"),
  list(label = "Cocoa",             sector = "Softs",          src = "disagg", code = "073732"),
  list(label = "Coffee",            sector = "Softs",          src = "disagg", code = "083731"),
  list(label = "Sugar",             sector = "Softs",          src = "disagg", code = "080732"),
  list(label = "Cotton",            sector = "Softs",          src = "disagg", code = "033661"),
  list(label = "Orange juice",      sector = "Softs",          src = "disagg", code = "040701"),
  list(label = "Lumber",            sector = "Softs",          src = "disagg", code = "058644"),
  list(label = "S&P 500 (E-mini)",  sector = "Equity indices", src = "tff",    code = "13874A"),
  list(label = "Nasdaq 100 (mini)", sector = "Equity indices", src = "tff",    code = "209742"),
  list(label = "Dow (x5)",          sector = "Equity indices", src = "tff",    code = "124603"),
  list(label = "Russell 2000 (E-mini)", sector = "Equity indices", src = "tff", code = "239742"),
  list(label = "VIX futures",       sector = "Equity indices", src = "tff",    code = "1170E1"),
  list(label = "UST 2Y",            sector = "Rates",          src = "tff",    code = "042601"),
  list(label = "UST 5Y",            sector = "Rates",          src = "tff",    code = "044601"),
  list(label = "UST 10Y",           sector = "Rates",          src = "tff",    code = "043602"),
  list(label = "UST bond (30Y)",    sector = "Rates",          src = "tff",    code = "020601"),
  list(label = "USD index",         sector = "Currencies",     src = "tff",    code = "098662"),
  list(label = "Euro",              sector = "Currencies",     src = "tff",    code = "099741"),
  list(label = "Japanese yen",      sector = "Currencies",     src = "tff",    code = "097741"),
  list(label = "British pound",     sector = "Currencies",     src = "tff",    code = "096742"),
  list(label = "Swiss franc",       sector = "Currencies",     src = "tff",    code = "092741"),
  list(label = "Canadian dollar",   sector = "Currencies",     src = "tff",    code = "090741"),
  list(label = "Australian dollar", sector = "Currencies",     src = "tff",    code = "232741"),
  list(label = "Bitcoin (CME)",     sector = "Currencies",     src = "tff",    code = "133741")
)
# Groups per report: name -> categories summed. fold = commercial-style gross
# legs (spreading added to both sides, as the legacy report does); net is the
# same either way.
DASH_GROUPS <- list(
  disagg = list(
    list(group = "managed money", cats = "managed_money", spec = TRUE),
    list(group = "commercials", cats = c("producer", "swap_dealer"), fold = TRUE),
    list(group = "[legacy] large spec", cats = c("managed_money", "other_rept")),
    list(group = "other reportables", cats = "other_rept"),
    list(group = "retail", cats = "retail")),
  tff = list(
    list(group = "leveraged funds", cats = "lev_money", spec = TRUE),
    list(group = "asset managers", cats = "asset_manager"),
    list(group = "dealers", cats = "dealer"),
    list(group = "other reportables", cats = "other_rept"),
    list(group = "retail", cats = "retail"))
)
# Rows, not calendar time, as COTSignal does: one row = one weekly report.
DASH_WINDOWS <- c(`1y` = 52, `3y` = 156, `5y` = 260)
DASH_OUT <- "C:/Users/aldoh/Documents/NewTrading/Reports/cot_positioning_latest.csv"

# ── Fetch ─────────────────────────────────────────────────────────────────────
# Past years never change, so they are downloaded once; the current year is
# re-fetched every run because that is where the new week lands.
load_source <- function(key) {
  spec <- SOURCES[[key]]
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  parts <- lapply(YEARS, function(y) {
    zip <- file.path(CACHE_DIR, sprintf("%s_%d.zip", key, y))
    if (!file.exists(zip) || y == this_year) {
      url <- sprintf(spec$url, y)
      ok <- tryCatch({
        utils::download.file(url, zip, mode = "wb", quiet = TRUE); TRUE
      }, error = function(e) FALSE)
      if (!ok || !file.exists(zip) || file.size(zip) < 10000) {
        if (file.exists(zip) && file.size(zip) < 10000) unlink(zip)
        stop(sprintf("download failed for %s (%d): %s", key, y, url))
      }
    }
    ex <- file.path(CACHE_DIR, sprintf("%s_%d", key, y))
    unlink(ex, recursive = TRUE); dir.create(ex, showWarnings = FALSE)
    utils::unzip(zip, exdir = ex)
    txt <- list.files(ex, pattern = "\\.txt$", full.names = TRUE)
    if (length(txt) != 1) stop(sprintf("expected one .txt in %s, found %d", ex, length(txt)))
    d <- data.table::fread(txt[1], select = spec$cols, showProgress = FALSE)
    unlink(ex, recursive = TRUE)   # keep the zips (~13 MB), drop the ~130 MB of extracted text
    data.table::setnames(d, spec$cols[1:2], c("date", "code"))
    d[, code := .norm_code(code)]
    d[, date := as.Date(date)]
    d
  })
  data.table::rbindlist(parts, use.names = TRUE)
}

message("Loading CFTC archives (", paste(range(YEARS), collapse = "-"), ") ...")
DATA <- lapply(names(SOURCES), load_source)
names(DATA) <- names(SOURCES)

# ── Series per asset ──────────────────────────────────────────────────────────
series_for <- function(a) {
  spec <- SOURCES[[a$src]]
  d <- DATA[[a$src]][code %in% .norm_code(a$codes)]
  if (nrow(d) == 0) stop(sprintf("no rows for %s (codes %s)", a$label, paste(a$codes, collapse = ",")))
  d[, long  := rowSums(.SD), .SDcols = spec$long]
  d[, short := rowSums(.SD), .SDcols = spec$short]
  # Sum the legs of a basket, then take one series per report date.
  s <- d[, .(long = sum(long), short = sum(short)), by = date][order(date)]
  s[, net := long - short]
  s
}

# Per-leg detail for the basket note (Corn +x, Soy +y, ...).
legs_for <- function(a) {
  if (length(a$codes) < 2 || is.null(names(a$codes))) return(NULL)
  spec <- SOURCES[[a$src]]
  out <- lapply(names(a$codes), function(nm) {
    d <- DATA[[a$src]][code == .norm_code(a$codes[[nm]])][order(date)]
    d[, net := rowSums(.SD[, spec$long, with = FALSE]) - rowSums(.SD[, spec$short, with = FALSE])]
    list(name = nm, net = d$net, date = d$date)
  })
  names(out) <- names(a$codes)
  out
}

fmtk <- function(x) sprintf("%+.1fk", x / 1000)
fmtk_abs <- function(x) sprintf("%.1fk", abs(x) / 1000)

# Previous state, so the notes can say what has moved since you last looked.
prev_env <- new.env()
prev_as_of <- NULL
prev_by_asset <- list()
if (file.exists(TARGET)) {
  try(sys.source(TARGET, envir = prev_env), silent = TRUE)
  prev_as_of <- tryCatch(get("COT_AS_OF", prev_env), error = function(e) NULL)
  pp <- tryCatch(get("COT_POSITIONING", prev_env), error = function(e) list())
  for (p in pp) prev_by_asset[[p$asset]] <- p
}

results <- lapply(ASSETS, function(a) {
  s <- series_for(a)
  latest <- s[.N]
  win5 <- s[date >= latest$date - 365.25 * 5]
  win1 <- s[date >= latest$date - 365]
  p5 <- mean(win5$net < latest$net) * 100
  p1 <- mean(win1$net < latest$net) * 100
  wow <- latest$net - s[.N - 1]$net
  since <- NA_real_
  if (!is.null(prev_as_of)) {
    ref <- s[date <= as.Date(prev_as_of)]
    if (nrow(ref) > 0) since <- latest$net - ref[.N]$net
  }
  list(a = a, s = s, latest = latest, p5 = p5, p1 = p1, wow = wow, since = since,
       lo = min(win5$net), hi = max(win5$net), n5 = nrow(win5),
       extreme = (p5 >= 90 || p5 <= 10), legs = legs_for(a),
       who = SOURCES[[a$src]]$who)
})

AS_OF <- max(vapply(results, function(r) as.character(r$latest$date), character(1)))
stale_legs <- vapply(results, function(r) as.character(r$latest$date), character(1))
if (length(unique(stale_legs)) > 1) {
  message("NOTE: report dates differ across assets: ",
          paste(sprintf("%s=%s", vapply(results, function(r) r$a$label, character(1)),
                        stale_legs), collapse = ", "))
}

# ── Notes ─────────────────────────────────────────────────────────────────────
build_note <- function(r) {
  a <- r$a; L <- r$latest
  side <- if (L$net >= 0) "net long" else "net short"
  txt <- sprintf("%s %s %s (long %s / short %s). 5y percentile %.0f, 1y percentile %.0f (5y range %s to %s, n=%d). WoW: %s.",
                 r$who, side, fmtk_abs(L$net), fmtk_abs(L$long), fmtk_abs(L$short),
                 r$p5, r$p1, fmtk(r$lo), fmtk(r$hi), r$n5, fmtk(r$wow))
  # Only worth saying when the previous file was actually an earlier week.
  if (!is.na(r$since) && !is.null(prev_as_of) && !identical(prev_as_of, AS_OF)) {
    txt <- paste0(txt, sprintf(" Since %s: %s.", prev_as_of, fmtk(r$since)))
  }
  if (!is.null(r$legs)) {
    leg_now <- vapply(names(r$legs), function(nm) {
      v <- r$legs[[nm]]; sprintf("%s %s", nm, fmtk(v$net[length(v$net)]))
    }, character(1))
    leg_wow <- vapply(names(r$legs), function(nm) {
      v <- r$legs[[nm]]; n <- length(v$net)
      sprintf("%s %s", nm, fmtk(v$net[n] - v$net[n - 1]))
    }, character(1))
    txt <- paste0(txt, " Legs: ", paste(leg_now, collapse = ", "),
                  "; WoW ", paste(leg_wow, collapse = ", "), ".")
  }
  prev <- prev_by_asset[[a$label]]
  if (!is.null(prev)) {
    now_dir <- if (L$net >= 0) "long" else "short"
    if (!identical(prev$net, now_dir)) {
      txt <- paste0(txt, sprintf(" DIRECTION FLIPPED since the last refresh (was %s).", prev$net))
    }
    if (!isTRUE(prev$extreme) && r$extreme) txt <- paste0(txt, " NEWLY EXTREME.")
    if (isTRUE(prev$extreme) && !r$extreme) txt <- paste0(txt, " No longer extreme.")
  }
  gsub('"', "'", txt, fixed = TRUE)
}

entries <- vapply(results, function(r) {
  sprintf('  list(asset = "%s", sector = "%s",\n       net = "%s", extreme = %s,\n       note = "%s")',
          r$a$label, r$a$sector, if (r$latest$net >= 0) "long" else "short",
          if (r$extreme) "TRUE" else "FALSE", build_note(r))
}, character(1))

header <- sprintf('# positioning.R -- weekly COT positioning summary
#
# GENERATED FILE -- do not hand-edit. Regenerate with:
#   Rscript RStudies/reports/macro_context/refresh_cot.R
# (scheduled Saturday 08:00, Windows task \\RApplication\\RefreshCOT). Hand
# edits are silently overwritten on the next run; to change what it says,
# change refresh_cot.R.
#
# Built from the CFTC annual archives (fut_disagg_txt / fut_fin_txt), which
# carry both the newest release and the multi-year history, so the current
# numbers and the percentiles come from one source. Series are Managed Money
# net, except USD which is Leveraged Funds + Asset Manager net (TFF report).
#
# extreme = TRUE when the net is at or beyond the 5-year 90th percentile
# (crowded long) or 10th percentile (crowded short). Reproducible from public
# data -- no narrative judgement, no Saxo digest dependency.
#
# This feeds compute_positioning_stress() in scenarios.R, which also warns when
# COT_AS_OF has gone stale (a missed weekly release).
#
# Generated %s from CFTC data as of %s.

COT_POSITIONING <- list(
%s
)

# Last updated: %s (COT data week ending %s)
COT_DATE <- "%s"
COT_AS_OF <- "%s"   # Tuesday-close date the positions refer to
',
  format(Sys.time(), "%Y-%m-%d %H:%M"), AS_OF,
  paste(entries, collapse = ",\n"),
  as.character(Sys.Date()), AS_OF, as.character(Sys.Date()), AS_OF)

# ── CSV side-output: every trader category, per contract, with WoW ────────────
# positioning.R deliberately carries one number per asset (the speculative net)
# because that is all the regime score consumes. This file is the full picture
# behind it -- who is on each side and how each moved over the week.
build_actor_csv <- function() {
  rows <- lapply(CSV_CONTRACTS, function(k) {
    spec <- SOURCES[[k$src]]
    d <- DATA[[k$src]][code == .norm_code(k$code)][order(date)]
    if (nrow(d) < 2) return(NULL)
    cur <- d[.N]; prv <- d[.N - 1]
    get <- function(row, col) if (is.na(col)) NA_real_ else as.numeric(row[[col]])

    per_cat <- lapply(names(spec$cats), function(cn) {
      cc <- spec$cats[[cn]]
      l <- get(cur, cc[1]); s <- get(cur, cc[2]); sp <- get(cur, cc[3])
      lp <- get(prv, cc[1]); spr <- get(prv, cc[2]); spp <- get(prv, cc[3])
      data.frame(category = cn, long = l, short = s, spreading = sp,
                 net = l - s, long_wow = l - lp, short_wow = s - spr,
                 net_wow = (l - s) - (lp - spr), spread_wow = sp - spp,
                 stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, per_cat)

    # Legacy rollups, so "commercial / large spec / small spec" is answerable
    # from the same file. Only meaningful for the disaggregated report --
    # producer + swap IS the old commercial, and managed money + other
    # reportables IS the old non-commercial (verified against the legacy report).
    if (k$src == "disagg") {
      # fold_spread: the legacy report gives Commercial no spreading column, so a
      # commercial trader's spread position is distributed into BOTH gross legs.
      # Non-Commercial keeps spreading separate, so large_spec must not fold.
      # Net is unaffected either way; this is only about matching the published
      # gross long/short. Verified against the legacy figures for WTI.
      roll <- function(nm, parts, fold_spread = FALSE) {
        p <- out[out$category %in% parts, ]
        sp <- sum(p$spreading, na.rm = TRUE)
        spw <- sum(p$spread_wow, na.rm = TRUE)
        add <- if (fold_spread) sp else 0
        addw <- if (fold_spread) spw else 0
        data.frame(category = nm,
                   long = sum(p$long) + add, short = sum(p$short) + add,
                   spreading = if (fold_spread) NA_real_ else sp,
                   net = sum(p$net), long_wow = sum(p$long_wow) + addw,
                   short_wow = sum(p$short_wow) + addw, net_wow = sum(p$net_wow),
                   spread_wow = if (fold_spread) NA_real_ else spw,
                   stringsAsFactors = FALSE)
      }
      out <- rbind(out,
                   roll("[legacy] commercial",  c("producer", "swap_dealer"), fold_spread = TRUE),
                   roll("[legacy] large_spec",  c("managed_money", "other_rept")),
                   roll("[legacy] small_spec",  "retail"))
    }
    oi <- as.numeric(cur[["Open_Interest_All"]])
    data.frame(report_date = as.character(cur$date), asset = k$label,
               cftc_code = k$code, report = k$src, open_interest = oi,
               out,
               pct_oi_long = round(100 * out$long / oi, 1),
               pct_oi_short = round(100 * out$short / oi, 1),
               prev_date = as.character(prv$date), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

actors <- build_actor_csv()

# ── Positioning dashboard: long / short / net x 1y / 3y / 5y COT index + WoW ──
# COT index (Williams) = 100 * (current - min) / (max - min) over the last n
# weekly reports: 100 = the most long (or most short contracts) in the window.
cot_index <- function(x, n) {
  w <- tail(x[!is.na(x)], n)
  if (length(w) < 2) return(NA_real_)
  rng <- max(w) - min(w)
  if (rng == 0) return(NA_real_)
  round(100 * (w[length(w)] - min(w)) / rng, 1)
}

build_dashboard <- function() {
  rows <- lapply(DASH_CONTRACTS, function(k) {
    spec <- SOURCES[[k$src]]
    d <- unique(DATA[[k$src]][code == .norm_code(k$code)], by = "date")[order(date)]
    if (nrow(d) < 2) {
      message(sprintf("dashboard: no data for %s (%s)", k$label, k$code))
      return(NULL)
    }
    col <- function(cn, i) {
      c <- spec$cats[[cn]][i]
      if (is.na(c)) rep(0, nrow(d)) else as.numeric(d[[c]])
    }
    per_group <- lapply(DASH_GROUPS[[k$src]], function(g) {
      sp <- if (isTRUE(g$fold)) Reduce(`+`, lapply(g$cats, col, 3)) else 0
      lg <- Reduce(`+`, lapply(g$cats, col, 1)) + sp
      sh <- Reduce(`+`, lapply(g$cats, col, 2)) + sp
      nt <- lg - sh
      n <- length(nt)
      idx <- function(x) setNames(vapply(DASH_WINDOWS, function(w) cot_index(x, w), 0),
                                  names(DASH_WINDOWS))
      il <- idx(lg); is <- idx(sh); inet <- idx(nt)
      data.frame(group = g$group, spec = isTRUE(g$spec),
                 long = lg[n], short = sh[n], net = nt[n],
                 long_wow = lg[n] - lg[n - 1], short_wow = sh[n] - sh[n - 1],
                 net_wow = nt[n] - nt[n - 1],
                 net_idx_1y = inet[["1y"]], net_idx_3y = inet[["3y"]], net_idx_5y = inet[["5y"]],
                 long_idx_1y = il[["1y"]], long_idx_3y = il[["3y"]], long_idx_5y = il[["5y"]],
                 short_idx_1y = is[["1y"]], short_idx_3y = is[["3y"]], short_idx_5y = is[["5y"]],
                 stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, per_group)
    oi <- as.numeric(d[.N][["Open_Interest_All"]])
    data.frame(report_date = as.character(d[.N]$date), prev_date = as.character(d[.N - 1]$date),
               market = k$label, sector = k$sector, report = k$src, cftc_code = k$code,
               open_interest = oi, weeks = nrow(d), out, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

dashboard <- build_dashboard()
if (!is.null(dashboard)) {
  short_hist <- unique(dashboard$market[dashboard$weeks < max(DASH_WINDOWS)])
  if (length(short_hist)) message("dashboard: fewer than ", max(DASH_WINDOWS),
                                  " weekly reports (5y index uses what exists): ",
                                  paste(short_hist, collapse = ", "))
  lagging <- unique(dashboard$market[dashboard$report_date < AS_OF])
  if (length(lagging)) message("dashboard: older report date than ", AS_OF, ": ",
                               paste(lagging, collapse = ", "))
}

# ── Report + write ────────────────────────────────────────────────────────────
cat("\n")
cat(sprintf("COT data as of %s%s\n", AS_OF,
            if (!is.null(prev_as_of) && identical(prev_as_of, AS_OF))
              "  (unchanged -- no new release since the last run)" else ""))
cat(sprintf("%-10s %10s %9s %9s %6s %6s  %s\n",
            "asset", "net", "WoW", "vs prev", "5y%", "1y%", "extreme"))
for (r in results) {
  prev <- prev_by_asset[[r$a$label]]
  flag <- if (r$extreme) "YES" else "-"
  if (!is.null(prev) && !identical(isTRUE(prev$extreme), r$extreme)) {
    flag <- paste0(flag, if (r$extreme) "  (was no)" else "  (was YES)")
  }
  cat(sprintf("%-10s %10s %9s %9s %6.0f %6.0f  %s\n",
              r$a$label, fmtk(r$latest$net), fmtk(r$wow),
              if (is.na(r$since) || identical(prev_as_of, AS_OF)) "n/a" else fmtk(r$since),
              r$p5, r$p1, flag))
}
cat("\n")

# Ignore the generation stamp when deciding whether anything actually moved, so
# a re-run in a week with no new release leaves the file (and git) untouched.
.body <- function(x) {
  x <- grep("^# Generated ", x, value = TRUE, invert = TRUE)
  while (length(x) && !nzchar(x[length(x)])) x <- x[-length(x)]  # writeLines adds a trailing newline
  x
}
unchanged <- file.exists(TARGET) &&
  identical(.body(strsplit(header, "\n", fixed = TRUE)[[1]]),
            .body(readLines(TARGET, warn = FALSE)))

if (!DRY_RUN && !is.null(actors)) {
  dir.create(dirname(CSV_OUT), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(actors, CSV_OUT, row.names = FALSE, na = "")
  cat(sprintf("Actor detail: %s (%d rows, %d contracts x categories)\n",
              CSV_OUT, nrow(actors), length(CSV_CONTRACTS)))
}
if (!DRY_RUN && !is.null(dashboard)) {
  tmp <- paste0(DASH_OUT, ".tmp")
  utils::write.csv(dashboard, tmp, row.names = FALSE, na = "")
  file.rename(tmp, DASH_OUT)
  cat(sprintf("Positioning dashboard: %s (%d markets x %d groups)\n",
              DASH_OUT, length(unique(dashboard$market)), 5L))
}

if (DRY_RUN) {
  cat("--dry-run: positioning.R not written.\n")
} else if (unchanged) {
  cat("No change since the last run -- positioning.R left untouched.\n")
} else {
  tmp <- paste0(TARGET, ".tmp")
  writeLines(header, tmp, useBytes = TRUE)
  # Only replace the live file once the new one parses.
  ok <- tryCatch({ parse(tmp); TRUE }, error = function(e) { message("generated file does not parse: ",
                                                                    conditionMessage(e)); FALSE })
  if (!ok) { unlink(tmp); stop("refusing to overwrite positioning.R") }
  file.rename(tmp, TARGET)
  cat("Written:", TARGET, "\n")
}
cat("COT refresh complete:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
