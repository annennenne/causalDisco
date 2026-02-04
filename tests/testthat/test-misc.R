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

# ──────────────────────────────────────────────────────────────────────────────
# graph_to_amat
# ──────────────────────────────────────────────────────────────────────────────

test_that("graph_to_amat converts to adjacency matrix", {
  nodes <- c("A", "B", "C")
  g <- methods::new("graphNEL", nodes = nodes, edgemode = "directed")
  g <- graph::addEdge("A", "B", g)
  g <- graph::addEdge("B", "C", g)

  A_tf <- graph_to_amat(g, to_from = TRUE, type = "pdag")
  expect_true(is.matrix(A_tf))
  expect_identical(rownames(A_tf), nodes)
  expect_identical(colnames(A_tf), nodes)
  expect_identical(attr(A_tf, "tamat_type"), "pdag")

  # to→from: row = to, col = from
  expect_equal(A_tf["B", "A"], 1) # A → B
  expect_equal(A_tf["A", "B"], 0)
  expect_equal(A_tf["C", "B"], 1) # B → C
  expect_equal(A_tf["B", "C"], 0)

  A_ft <- graph_to_amat(g, to_from = FALSE, type = "pdag")
  expect_equal(A_ft, t(A_tf))
  expect_identical(attr(A_ft, "tamat_type"), "pdag")
})
