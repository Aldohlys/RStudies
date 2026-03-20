# macro_outcomes.R — Post-event surprise direction
#
# EDIT AFTER MAJOR MACRO PRINTS (CPI, PPI, FOMC, NFP).
# Records the surprise direction and immediate market reaction.
# This feeds into regime modifiers for the next 2-4 weeks until next print.

MACRO_OUTCOMES <- list(
  # list(event = "CPI Feb", date = "2026-03-19", surprise = "hot",
  #      note = "Core +0.4% vs +0.3% exp, rates +8bps",
  #      impact = list(rates = "up", dxy = "up", tech = "down", financials = "up"))
)

# Surprise values: "hot", "cool", "inline"
# Impact: direction of immediate reaction for key assets
# Entries older than 30 days are ignored by the regime system.
