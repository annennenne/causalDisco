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

toy_df_score <- function(n = 100) {
  set.seed(7)
  V1 <- rnorm(n)
  V3 <- rnorm(n, 0, 0.2)
  V2 <- 0.6 * V1 + 0.4 * V3 + rnorm(n, 0, 0.05)
  V4 <- V3 + rnorm(n)
  V5 <- V3 + rnorm(n)
  V6 <- 0.7 * V5 + rnorm(n, 0, 0.1)
  data.frame(V1, V2, V3, V4, V5, V6)
}
