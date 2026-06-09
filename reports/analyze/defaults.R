# reports/analyze/defaults.R — default config for /analyze.
#
# config.yml is instance-specific (may contain secrets) and not committed.
# These defaults let the script run on a fresh clone without one. If a
# config.yml IS present and has a `default.analyze` block, those values
# override these defaults (deep-merge, key-by-key).

default_analyze_config <- function() {
  list(
    risk_cap_lot_usd = 300,
    rr_min = 0.5,
    ivp_regime = list(
      cheap_max = 30,
      rich_min  = 70
    ),
    ivp_pts = list(
      pt4_max = 25,
      pt3_max = 40,
      pt2_max = 60,
      pt1_max = 75
    ),
    vrp_log_bands = list(
      neg_max      = 0,
      mild_max     = 10,
      moderate_max = 30,
      strong_max   = 70
    ),
    vrp_pts = list(
      pt2_max = 0,
      pt1_max = 10
    ),
    term_pts = list(
      pt2_max = -5,
      pt1_max = 0
    ),
    spread_widths = c(5, 10),
    # ±5% strike band for spread enumeration. Only the 10 best DEBIT spreads
    # are proposed and they sit near ATM (long ~ATM, short within the risk cap),
    # so a tight band suffices; wider just qualifies/prices more strikes and
    # enumerates more pairs that the within-cap + top-10 filters discard.
    moneyness_pct = 0.05,
    move_lookback_days = 40,
    earnings_window_days = 14,
    skew_lookback_days = 365,
    out_dir = "C:/Users/aldoh/Documents/NewTrading/reports"
  )
}

# Deep-merge user overrides on top of defaults: scalars and lists override,
# nested lists merge recursively. Anything in user_cfg that isn't in defaults
# is also kept (forward-compatible).
merge_config <- function(defaults, user_cfg) {
  if (is.null(user_cfg) || length(user_cfg) == 0) return(defaults)
  out <- defaults
  for (key in names(user_cfg)) {
    uv <- user_cfg[[key]]
    dv <- defaults[[key]]
    if (is.list(uv) && is.list(dv) && !is.null(names(uv)) && !is.null(names(dv))) {
      out[[key]] <- merge_config(dv, uv)
    } else {
      out[[key]] <- uv
    }
  }
  out
}

# Load config: tries config.yml, falls back to defaults, merges if both present.
load_analyze_config <- function(yml_path = NULL) {
  defaults <- default_analyze_config()
  if (is.null(yml_path) || !file.exists(yml_path)) {
    message("No config.yml at ", yml_path %||% "(unspecified)",
            " — using built-in defaults.")
    return(defaults)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    message("yaml package unavailable — using built-in defaults.")
    return(defaults)
  }
  user_cfg <- tryCatch({
    raw <- yaml::read_yaml(yml_path)
    raw$default$analyze
  }, error = function(e) {
    message("config.yml read failed (", conditionMessage(e),
            ") — using built-in defaults.")
    NULL
  })
  if (is.null(user_cfg)) {
    message("config.yml has no default.analyze block — using built-in defaults.")
    return(defaults)
  }
  merge_config(defaults, user_cfg)
}
