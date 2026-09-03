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

YEARS <- (as.integer(format(Sys.Date(), "%Y")) - 5):as.integer(format(Sys.Date(), "%Y"))

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

SOURCES <- list(
  disagg = list(
    url  = "https://www.cftc.gov/files/dea/history/fut_disagg_txt_%d.zip",
    cols = c("Report_Date_as_YYYY-MM-DD", "CFTC_Contract_Market_Code",
             "M_Money_Positions_Long_All", "M_Money_Positions_Short_All"),
    long = "M_Money_Positions_Long_All", short = "M_Money_Positions_Short_All",
    who  = "MM"),
  tff = list(
    url  = "https://www.cftc.gov/files/dea/history/fut_fin_txt_%d.zip",
    cols = c("Report_Date_as_YYYY-MM-DD", "CFTC_Contract_Market_Code",
             "Asset_Mgr_Positions_Long_All", "Asset_Mgr_Positions_Short_All",
             "Lev_Money_Positions_Long_All", "Lev_Money_Positions_Short_All"),
    long = c("Asset_Mgr_Positions_Long_All", "Lev_Money_Positions_Long_All"),
    short = c("Asset_Mgr_Positions_Short_All", "Lev_Money_Positions_Short_All"),
    who  = "Lev+AM")
)

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
