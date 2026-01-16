# ──────────────────────────────────────────────────────────────────────────────
# Build helper function for knowledge from example data
# ──────────────────────────────────────────────────────────────────────────────

build_kn_from_order <- function() {
  knowledge(
    tpc_example,
    tier(
      child ~ child_x2 + child_x1,
      youth ~ youth_x4 + youth_x3,
      oldage ~ oldage_x6 + oldage_x5
    )
  )
}
