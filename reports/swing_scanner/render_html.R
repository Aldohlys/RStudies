# render_html.R — Interactive HTML report with DataTables, per-day file
#
# Renders the scanner output as a single self-contained HTML file:
#   - Funnel summary (Universe → Flow → Cheap → Setup → TOP PICK)
#   - Filter chips (phase-of-drop, sector, vehicle, show SKIP)
#   - DataTables-rendered master table with Tier 1 visible / Tier 2 hidden columns
#
# Data is embedded as a JSON blob. DataTables loaded from CDN. No build step.

#' Render the scanner HTML.
#' @param df data.frame with all phase columns + rank + phase_of_drop
#' @param funnel named integer vector: counts per phase
#' @param out_dir character output directory
#' @param run_date Date — typically Sys.Date()
#' @param rr_min numeric — calibrated threshold for footer note
#' @return path of written HTML file
render_scanner_html <- function(df, funnel, out_dir, run_date = Sys.Date(),
                                rr_min = 0.5) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("jsonlite required for render_scanner_html")

  # Tier 1 visible columns and Tier 2 hidden — exact subset names must exist
  # in df. Missing columns rendered as empty strings.
  tier1 <- c("sym", "sector", "stage", "flow_score", "cheap_score",
             "vehicle", "spot_target_low", "rr",
             "entry_floor", "entry_ceiling", "entry_state", "chain_state")
  tier2 <- c("flow_direction", "cheap_side", "targets_agreeing", "fib_confirms",
             "oi_cap_call", "oi_concentration_pct", "crowded_flag",
             "headroom_band", "phase_of_drop", "sector_rs_rank",
             "ivp_2y", "vrp", "expiry", "strike",
             "stage_pts", "sector_pts", "footprint_pts")
  all_cols <- intersect(c("rank", tier1, tier2), names(df))

  # Default-sort: rank then -rr
  rank_order <- function(r) match(r, c("TOP PICK", "WATCH", "SKIP"),
                                  nomatch = 99L)
  df <- df[order(rank_order(df$rank),
                 -ifelse(is.na(df$rr), -1e9, df$rr)), , drop = FALSE]

  data_json <- jsonlite::toJSON(df[, all_cols, drop = FALSE], dataframe = "rows",
                                na = "null", auto_unbox = TRUE)

  # Column config for DataTables
  col_defs <- lapply(all_cols, function(c) {
    list(data = c, title = c,
         visible = c %in% c("rank", tier1))
  })
  col_defs_json <- jsonlite::toJSON(col_defs, auto_unbox = TRUE)

  fname <- sprintf("swing_scanner_%s.html", format(run_date, "%Y%m%d"))
  out_path <- file.path(out_dir, fname)

  funnel_html <- paste0(
    sapply(seq_along(funnel), function(i) {
      sprintf('<div class="funnel-step"><div class="funnel-label">%s</div><div class="funnel-count">%d</div></div>',
              names(funnel)[i], funnel[i])
    }),
    collapse = '<div class="funnel-arrow">→</div>')

  sectors_unique <- sort(unique(df$sector[!is.na(df$sector)]))
  sector_options <- paste0(sprintf('<option value="%s">%s</option>',
                                   sectors_unique, sectors_unique),
                           collapse = "")
  vehicles_unique <- sort(unique(df$vehicle[!is.na(df$vehicle) &
                                            df$vehicle != ""]))
  vehicle_options <- paste0(sprintf('<option value="%s">%s</option>',
                                    vehicles_unique, vehicles_unique),
                            collapse = "")

  html <- sprintf('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Swing Scanner — %s</title>
<link rel="stylesheet" href="https://cdn.datatables.net/2.1.8/css/dataTables.dataTables.min.css">
<link rel="stylesheet" href="https://cdn.datatables.net/buttons/3.2.0/css/buttons.dataTables.min.css">
<style>
body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; margin: 12px 18px; color: #222; }
h1 { font-size: 18px; margin: 0 0 4px 0; }
.subtitle { color: #666; font-size: 12px; margin-bottom: 16px; }
.funnel { display: flex; align-items: center; gap: 6px; margin: 12px 0 18px 0;
          padding: 10px; background: #f7f7f7; border-radius: 6px; }
.funnel-step { padding: 8px 12px; background: white; border-radius: 4px;
               border: 1px solid #ddd; min-width: 90px; text-align: center; }
.funnel-label { font-size: 10px; color: #888; text-transform: uppercase; }
.funnel-count { font-size: 22px; font-weight: 600; color: #333; }
.funnel-arrow { color: #999; font-size: 18px; }
.chips { display: flex; gap: 8px; margin-bottom: 12px; align-items: center; flex-wrap: wrap; }
.chip { padding: 4px 10px; background: #eef; border-radius: 14px; font-size: 12px;
        border: 1px solid #ccd; cursor: pointer; user-select: none; }
.chip.active { background: #336; color: #fff; border-color: #336; }
select.chip { padding: 4px 8px; }
table.dataTable { font-size: 12px; }
table.dataTable thead th { background: #f0f0f0; }
.dt-rank-toppick { background: #e6f4d6; font-weight: 600; }
.dt-rank-watch   { background: #fff8e6; }
.dt-rank-skip    { color: #999; }
.footer { font-size: 11px; color: #777; margin-top: 18px; padding-top: 8px;
          border-top: 1px solid #ddd; }
</style>
</head><body>
<h1>Swing Scanner — %s</h1>
<div class="subtitle">Phases: A(Universe) → B(Flow) → C(Cheap) → D(Setup/Chain/R:R) → E(Classify). R:R_min = %.2f.</div>
<div class="funnel">%s</div>
<div class="chips">
  <select id="phaseChip" class="chip">
    <option value="">All phases</option>
    <option value="A">Dropped at A (Universe)</option>
    <option value="B">Dropped at B (Flow)</option>
    <option value="C">Dropped at C (Cheap)</option>
    <option value="D">Dropped at D (Setup/Chain/R:R)</option>
  </select>
  <select id="sectorChip" class="chip">
    <option value="">All sectors</option>
    %s
  </select>
  <select id="vehicleChip" class="chip">
    <option value="">All vehicles</option>
    %s
  </select>
  <span id="showSkip" class="chip">Show SKIP</span>
</div>
<table id="scanner" class="display"></table>
<div class="footer">Generated %s. R:R_min calibrated from BOT winners 25th pctile = %.2f. See <code>project_swing_scanner_redesign.md</code>.</div>
<script src="https://code.jquery.com/jquery-3.7.1.min.js"></script>
<script src="https://cdn.datatables.net/2.1.8/js/dataTables.min.js"></script>
<script src="https://cdn.datatables.net/buttons/3.2.0/js/dataTables.buttons.min.js"></script>
<script src="https://cdn.datatables.net/buttons/3.2.0/js/buttons.colVis.min.js"></script>
<script>
const SCANNER_DATA = %s;
const COL_DEFS = %s;

$(document).ready(function() {
  const showSkip = { value: false };
  const table = new DataTable("#scanner", {
    data: SCANNER_DATA,
    columns: COL_DEFS,
    pageLength: 50,
    order: [],
    dom: "Bfrtip",
    buttons: ["colvis"],
    rowCallback: function(row, data) {
      $(row).removeClass("dt-rank-toppick dt-rank-watch dt-rank-skip");
      if (data.rank === "TOP PICK") $(row).addClass("dt-rank-toppick");
      else if (data.rank === "WATCH") $(row).addClass("dt-rank-watch");
      else $(row).addClass("dt-rank-skip");
    }
  });

  $.fn.dataTable.ext.search.push(function(settings, data, dataIndex) {
    const row = SCANNER_DATA[dataIndex];
    if (!showSkip.value && row.rank === "SKIP") return false;
    const phaseFilter = $("#phaseChip").val();
    if (phaseFilter && (!row.phase_of_drop || !row.phase_of_drop.startsWith(phaseFilter))) return false;
    const sectorFilter = $("#sectorChip").val();
    if (sectorFilter && row.sector !== sectorFilter) return false;
    const vehFilter = $("#vehicleChip").val();
    if (vehFilter && row.vehicle !== vehFilter) return false;
    return true;
  });

  $("#phaseChip, #sectorChip, #vehicleChip").on("change", () => table.draw());
  $("#showSkip").on("click", function() {
    showSkip.value = !showSkip.value;
    $(this).toggleClass("active");
    table.draw();
  });
});
</script>
</body></html>',
    format(run_date, "%Y-%m-%d"),
    format(run_date, "%Y-%m-%d"),
    rr_min,
    funnel_html,
    sector_options,
    vehicle_options,
    format(Sys.time()),
    rr_min,
    data_json,
    col_defs_json)

  writeLines(html, out_path)
  out_path
}
