# swing_scanner/setup_chain_rr.R — thin shim sourcing the shared module.
#
# All Phase D primitives (compute_structural_target, walk_chain_oi,
# compute_rr_entry, classify_entry_state) live in shared/setup_chain_rr.R
# so that /analyze can re-derive them live when the scanner row is silent.
# Vehicle/expiry rule lives in shared/vehicle_rule.R.

source(file.path(dirname(sys.frame(1)$ofile), "..", "shared", "setup_chain_rr.R"))
