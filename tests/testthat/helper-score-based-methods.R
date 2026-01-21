# ──────────────────────────────────────────────────────────────────────────────
# Helper functions for tests
# ──────────────────────────────────────────────────────────────────────────────

ges_registry <- list(
  ges = list(fn = ges, engines = c("tetrad", "pcalg"))
)

ges_args <- function(engine) {
  if (engine == "pcalg") {
    list(score = "sem_bic", directed_as_undirected_knowledge = TRUE)
  } else {
    list(score = "sem_bic")
  }
}
