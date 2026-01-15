test_that("orientation confusion returns zeros when no shared adjacency", {
  nodes <- c("A", "B", "C")

  A_true <- matrix(0L, 3, 3, dimnames = list(nodes, nodes))
  class(A_true) <- c("amat.cpdag", "matrix")
  A_true["B", "A"] <- 1L # A -> B

  A_est <- matrix(0L, 3, 3, dimnames = list(nodes, nodes))
  class(A_est) <- c("amat.cpdag", "matrix")
  A_est["C", "A"] <- 1L # A -> C

  tm_true <- tamat(A_true, order = nodes, type = "pdag")
  tm_est <- tamat(A_est, order = nodes, type = "pdag")

  got <- confusion(tm_est, tm_true, type = "dir")
  expect_equal(got, list(tp = 0, tn = 0, fp = 0, fn = 0))
})

test_that("adjacency confusion on empty graphs gives all TN", {
  nodes <- c("A", "B", "C", "D")
  A <- matrix(0L, 4, 4, dimnames = list(nodes, nodes))
  class(A) <- c("amat.cpdag", "matrix")

  Tm <- tamat(A, order = nodes, type = "pdag")
  got <- confusion(Tm, Tm, type = "adj")

  expect_equal(got, list(tp = 0, tn = 6, fp = 0, fn = 0))
})
