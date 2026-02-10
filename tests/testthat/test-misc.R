# ──────────────────────────────────────────────────────────────────────────────
# as.graphNEL
# ──────────────────────────────────────────────────────────────────────────────

test_that("as.graphNEL drops tamat class", {
  m <- matrix(c(0, 1, 0, 0), nrow = 2, byrow = TRUE)
  rownames(m) <- colnames(m) <- c("A", "B")
  attr(m, "tamat_type") <- "pdag"
  class(m) <- c("tamat", "matrix")

  g <- as.graphNEL(m)
  expect_s4_class(g, "graphNEL")

  mat_from_g <- methods::as(g, "matrix")
  expect_equal(mat_from_g, t(unclass(m)), ignore_attr = TRUE)
})
