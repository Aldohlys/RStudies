# reports/analyze/main.R — single-ticker /analyze pipeline (data-only)
#
# Run from CLI:
#   Rscript reports/analyze/main.R <TICKER> <DIRECTION> [--no-html] [--no-vol-funnel]
#
# Pipeline: Phase A (Universe) -> B (Pull) -> C (Cheap + Vol Funnel)
#         -> D (Setup/Chain/R:R + Structures) -> E (Classification)
#
# Output: HTML report + terminal data table. NO verdicts, NO conviction,
# NO "Best/Alternative/Avoid" rankings. Mechanical phase outcomes only.

suppressPackageStartupMessages({
  library(Tdata); library(Tbasics); library(dplyr); library(DBI)
})

.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa) > 0) return(normalizePath(dirname(sub("^--file=", "", fa[1]))))
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)$ofile; if (!is.null(f)) return(normalizePath(dirname(f)))
  }
  getwd()
}
SCRIPT_DIR <- .get_script_dir()

source(file.path(SCRIPT_DIR, "..", "shared", "html_helpers.R"))
source(file.path(SCRIPT_DIR, "..", "shared", "cache.R"))
source(file.path(SCRIPT_DIR, "..", "shared", "freshness.R"))
source(file.path(SCRIPT_DIR, "..", "shared", "indicators.R"))
source(file.path(SCRIPT_DIR, "..", "shared", "universe.R"))
source(file.path(SCRIPT_DIR, "..", "shared", "live_sources.R"))
source(file.path(SCRIPT_DIR, "..", "shared", "vehicle_rule.R"))
source(file.path(SCRIPT_DIR, "..", "shared", "setup_chain_rr.R"))
source(file.path(SCRIPT_DIR, "defaults.R"))
source(file.path(SCRIPT_DIR, "phases.R"))
source(file.path(SCRIPT_DIR, "funnel.R"))
source(file.path(SCRIPT_DIR, "structures.R"))
source(file.path(SCRIPT_DIR, "report.R"))

# ── Config: defaults + optional override from RStudies/config.yml ─────────
# config.yml is instance-specific (may contain secrets) and is NOT tracked
# in git. The script ships with built-in defaults; if config.yml is present
# and exposes `default.analyze`, those keys override the defaults.
.cfg_path <- file.path(SCRIPT_DIR, "..", "..", "config.yml")
CONFIG <- load_analyze_config(.cfg_path)

# ── Args ──────────────────────────────────────────────────────────────────
parse_args <- function(argv) {
  if (length(argv) < 2) stop(
    "Usage: Rscript main.R <TICKER> <DIRECTION> [--no-html] [--no-vol-funnel] [--refresh] [--max-age <hours>]")
  ticker    <- toupper(argv[1])
  direction <- tolower(argv[2])
  if (!direction %in% c("long", "short")) stop("DIRECTION must be 'long' or 'short'")
  list(
    ticker        = ticker,
    direction     = direction,
    no_html       = "--no-html"        %in% argv,
    no_vol_funnel = "--no-vol-funnel"  %in% argv
  )
}

argv <- commandArgs(trailingOnly = TRUE)
args <- parse_args(argv)
freshness <- resolve_freshness_policy(argv)

message("=== /analyze ", args$ticker, " ", args$direction, "  (",
        as.character(Sys.Date()), ") ===")
message(sprintf("Freshness policy: max_age=%gh%s",
                freshness$max_age_hours,
                if (freshness$force_refresh) " (force-refresh: cached values ignored)" else ""))

# Make policy available to phase modules via CONFIG
CONFIG$freshness <- freshness
# Run timestamp threaded into the report for "data retrieved" tooltips
CONFIG$run_started_at <- Sys.time()

# Surface scanner CSV staleness up front
csv_mtime <- scanner_csv_mtime(CONFIG$out_dir)
csv_age_h <- hours_since(csv_mtime)
if (is.null(csv_mtime)) {
  message("Scanner CSV: none found in ", CONFIG$out_dir,
          " — every field will be live-fetched.")
} else {
  message(sprintf("Scanner CSV mtime: %s (%.1fh old) — %s",
                  format(csv_mtime, "%Y-%m-%d %H:%M:%S"),
                  csv_age_h,
                  if (is_fresh(csv_mtime, freshness)) "fresh, will use cached fields"
                  else "STALE, will refetch live"))
}

# ── TWS reachability probe ───────────────────────────────────────────────
# One quick check up front. If TWS isn't accepting connections, every
# downstream live fetch will short-circuit with a "TWS not reachable" reason
# instead of issuing a per-call request that blocks reticulate's asyncio loop.
TWS_REACHABLE <- tryCatch(isTRUE(Tdata::isIBAvailable()), error = function(e) FALSE)
if (!TWS_REACHABLE) {
  message("WARNING: TWS not reachable (isIBAvailable() returned FALSE). ",
          "Live IBKR fetches will be skipped; affected fields will surface ",
          "'FETCH FAILED: TWS not reachable' in the report.")
}
CONFIG$tws_reachable <- TWS_REACHABLE

# ── Phase A: option liquidity (informational only) ────────────────────────
message("Phase A: Option liquidity probe (informational)...")
phase_a <- run_phase_a(args$ticker, freshness = freshness)
message(sprintf("  A: %s | %s expiries (%s tradeable in 14-90 DTE) | src=%s",
                phase_a$result,
                phase_a$n_expiries %||% "n/a",
                phase_a$tradeable_expiries %||% "n/a",
                phase_a$source %||% "n/a"))

# ── Phase B: Trend + sector RS context ────────────────────────────────────
message("Phase B: Trend + sector RS context...")
phase_b <- run_phase_b(args$ticker, args$direction, freshness = freshness)
message(sprintf("  B: %s | stage=%s align=%s sector=%s rank=%s/%s",
                phase_b$result, phase_b$stage %||% "n/a",
                phase_b$direction_match,
                phase_b$sector %||% "n/a",
                phase_b$sector_rs_rank %||% "n/a",
                phase_b$n_sectors %||% "n/a"))

# ── Phase C: Cheap score + Vol Funnel ─────────────────────────────────────
message("Phase C: Cheap score + Vol Funnel...")
phase_c <- run_phase_c(args$ticker, args$direction,
                       run_funnel = !args$no_vol_funnel,
                       config = CONFIG,
                       spot = phase_b$price,
                       freshness = freshness)
message(sprintf("  C: %s | cheap_score=%s side=%s",
                phase_c$result, phase_c$cheap_score, phase_c$cheap_side))
if (!is.null(phase_c$funnel)) {
  tally <- phase_c$funnel$tally
  message(sprintf("    Funnel tally for %s: %d favorable / %d unfavorable / %d unavailable",
                  args$direction, tally$favorable, tally$unfavorable, tally$unavailable))
}

# ── Phase D: Setup, chain, R:R + structures table ─────────────────────────
message("Phase D: Setup, chain, R:R, structures...")
phase_d <- run_phase_d(args$ticker, args$direction, phase_b, phase_c,
                       config = CONFIG, freshness = freshness)
message(sprintf("  D: %s | targets_agreeing=%s | structures-within-cap=%s",
                phase_d$result, phase_d$targets_agreeing,
                phase_d$n_structures_within_cap))

# ── Phase E: classification (mechanical label only) ───────────────────────
phase_e <- run_phase_e(phase_a, phase_b, phase_c, phase_d, config = CONFIG)
message(sprintf("  E: classification=%s | phase_of_drop=%s",
                phase_e$classification, phase_e$phase_of_drop))

# ── Render report ─────────────────────────────────────────────────────────
ctx <- list(
  ticker    = args$ticker,
  direction = args$direction,
  date      = Sys.Date(),
  phase_a   = phase_a,
  phase_b   = phase_b,
  phase_c   = phase_c,
  phase_d   = phase_d,
  phase_e   = phase_e,
  config    = CONFIG
)

if (!args$no_html) {
  out_dir <- CONFIG$out_dir
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  html_file <- render_analyze_html(ctx, out_dir)
  message(sprintf("HTML written: %s", html_file))
  if (interactive()) utils::browseURL(html_file)
}

# Terminal summary (data-only — no GO/NO-GO, no conviction)
cat("\n")
cat(strrep("=", 64), "\n", sep = "")
cat(sprintf("  /analyze %s %s    %s\n", args$ticker, args$direction, Sys.Date()))
cat(strrep("=", 64), "\n", sep = "")
.spot_str <- {
  p <- phase_b$price
  if (is.null(p) || length(p) == 0 || is.na(p[1])) "n/a"
  else sprintf("%.2f", as.numeric(p[1]))
}
cat(sprintf("  Sector: %s | Spot: $%s\n",
            phase_b$sector %||% "n/a", .spot_str))
cat(sprintf("  classification: %s | phase_of_drop: %s\n",
            phase_e$classification, phase_e$phase_of_drop))
cat(sprintf("  Phase A: %s\n", phase_a$result))
cat(sprintf("  Phase B: %s  stage=%s align=%s sector_rank=%s/%s\n",
            phase_b$result, phase_b$stage %||% "n/a",
            phase_b$direction_match,
            phase_b$sector_rs_rank %||% "n/a",
            phase_b$n_sectors %||% "n/a"))
cat(sprintf("  Phase C: %s  cheap_score=%s side=%s\n",
            phase_c$result, phase_c$cheap_score, phase_c$cheap_side))
if (!is.null(phase_c$funnel)) {
  t <- phase_c$funnel$tally
  cat(sprintf("    Funnel tally: %d favorable / %d unfavorable / %d unavailable\n",
              t$favorable, t$unfavorable, t$unavailable))
}
cat(sprintf("  Phase D: %s  targets_agreeing=%s within-cap=%s\n",
            phase_d$result, phase_d$targets_agreeing,
            phase_d$n_structures_within_cap))
cat(strrep("=", 64), "\n", sep = "")
