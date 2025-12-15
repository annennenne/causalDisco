# ──────────────────────────────────────────────────────────────────────────────
# Helpers for testing constraint-based methods (pc, fci) across engines
# ──────────────────────────────────────────────────────────────────────────────

# register methods and the engines they support
method_registry_constraint <- list(
  pc  = list(fn = pc, engines = c("tetrad", "pcalg", "bnlearn")),
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

# tiny continuous dataset; fast & stable
toy_df_constraint <- function(n = 60L) {
  set.seed(69)
  V1 <- rnorm(n)
  V3 <- rnorm(n, 0, 0.2)
  V2 <- 0.6 * V1 + 0.4 * V3 + rnorm(n, 0, 0.05)
  V4 <- V3 + rnorm(n)
  V5 <- V3 + rnorm(n)
  V6 <- 0.7 * V5 + rnorm(n, 0, 0.1)
  data.frame(V1, V2, V3, V4, V5, V6)
}

toy_knowledge <- function(df) {
  knowledge(
    df,
    V1 %-->% V2,
    V5 %-->% V6
  )
}
