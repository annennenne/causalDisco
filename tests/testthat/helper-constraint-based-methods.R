# ──────────────────────────────────────────────────────────────────────────────
# Helpers for testing constraint-based methods (pc, fci) across engines
# ──────────────────────────────────────────────────────────────────────────────

# register methods and the engines they support
method_registry_constraint <- list(
  pc = list(fn = pc, engines = c("tetrad", "pcalg", "bnlearn")),
  fci = list(fn = fci, engines = c("tetrad", "pcalg"))
)

# per-method arguments (add branches here if a method/engine needs extras)
method_args <- function(method_name, engine) {
  args <- list(test = "fisher_z", alpha = 0.05)
  if (engine == "pcalg") {
    args$directed_as_undirected_knowledge <- TRUE
  }
  args
}

toy_knowledge <- function(df) {
  knowledge(
    df,
    X1 %-->% X2,
    X3 %-->% Z
  )
}
