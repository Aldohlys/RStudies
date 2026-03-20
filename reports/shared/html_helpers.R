# shared/html_helpers.R — HTML rendering utilities
#
# Zone styling, tooltip headers, and template rendering helpers
# shared between macro_context and swing_scanner reports.

#' Map zone label to CSS class
zone_class <- function(z) {
  switch(z,
    GREEN = "zone-green", ORANGE = "zone-orange",
    RED = "zone-red", DARKRED = "zone-darkred",
    "zone-orange")
}

#' Map zone label to badge CSS class (for inline badges)
badge_class <- function(z) {
  switch(z,
    GREEN = "badge-green", ORANGE = "badge-orange",
    RED = "badge-red", DARKRED = "badge-red",
    "badge-orange")
}

#' Classify a numeric value into a zone
#' @param val Numeric value
#' @param breaks Numeric vector of thresholds (ascending)
#' @param labels Character vector of zone labels (length = length(breaks) + 1)
zone <- function(val, breaks, labels) {
  if (is.na(val)) return(labels[length(labels)])
  for (i in seq_along(breaks)) if (val < breaks[i]) return(labels[i])
  labels[length(labels)]
}

#' Build a table header cell with tooltip
#' @param col Column name
#' @param tip Tooltip text
tooltip_th <- function(col, tip = NULL) {
  if (!is.null(tip) && !is.na(tip)) {
    sprintf('<th title="%s">%s</th>', tip, col)
  } else {
    sprintf('<th>%s</th>', col)
  }
}

#' Render an HTML template by replacing {{PLACEHOLDER}} markers
#' @param template Character string containing {{...}} placeholders
#' @param replacements Named list of placeholder -> value mappings
render_template <- function(template, replacements) {
  for (ph in names(replacements)) {
    template <- gsub(ph, replacements[[ph]], template, fixed = TRUE)
  }
  template
}

#' Format a numeric change
fmt_chg <- function(cur, prev) {
  if (is.na(cur) || is.na(prev)) "n/a"
  else sprintf("%+.2f", cur - prev)
}

#' Format a numeric value or return "n/a"
naf <- function(x, digits = 2) {
  if (is.na(x)) "n/a"
  else sprintf(paste0("%.", digits, "f"), x)
}

#' Build a macro data row with tooltip
#' @param label Row label
#' @param value Numeric value
#' @param z Zone label
#' @param note Note text
#' @param tip Tooltip text (data source, meaning, thresholds)
html_row_tip <- function(label, value, z, note, tip = "") {
  vs <- if (is.na(value)) "n/a" else sprintf("%.2f", value)
  title_attr <- if (nzchar(tip)) sprintf(' title="%s"', tip) else ""
  sprintf('<tr class="data-row %s"%s><td class="label">%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    zone_class(z), title_attr, label, vs, note)
}

#' Build a macro sub-row with tooltip
html_sub_tip <- function(label, val_str, z, note, tip = "") {
  title_attr <- if (nzchar(tip)) sprintf(' title="%s"', tip) else ""
  sprintf('<tr class="sub-row %s"%s><td class="label indent">%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    zone_class(z), title_attr, label, val_str, note)
}
