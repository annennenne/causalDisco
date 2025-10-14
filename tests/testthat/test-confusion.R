test_that("adjacency confusion on simple CPDAG example", {
  true_disc <- new_discography(tibble::tibble(
    from = c("A", "B"),
    to = c("B", "C"),
    edge_type = c("---", "-->")
  ))

  est_disc <- new_discography(tibble::tibble(
    from = c("A", "C", "A"),
    to = c("B", "B", "C"),
    edge_type = c("---", "-->", "-->")
  ))

  got <- confusion(est_disc, true_disc, type = "adj")
  expect_equal(got, list(tp = 2, tn = 0, fp = 1, fn = 0))
})

test_that("orientation confusion on simple CPDAG example", {
  true_disc <- new_discography(tibble::tibble(
    from = c("A", "B"),
    to = c("B", "C"),
    edge_type = c("---", "-->")
  ))

  est_disc <- new_discography(tibble::tibble(
    from = c("A", "C", "A"),
    to = c("B", "B", "C"),
    edge_type = c("---", "-->", "-->")
  ))

  got <- confusion(est_disc, true_disc, type = "dir")
  expect_identical(got, list(tp = 0, tn = 2, fp = 1, fn = 1))
})

test_that("coercion works for discography, tamat, and matrices", {
  true_disc <- new_discography(tibble::tibble(
    from = c("A", "B"),
    to = c("B", "C"),
    edge_type = c("---", "-->")
  ))
  est_disc <- new_discography(tibble::tibble(
    from = c("A", "C", "A"),
    to = c("B", "B", "C"),
    edge_type = c("---", "-->", "-->")
  ))

  true_mat <- amat(true_disc)
  est_mat <- amat(est_disc)

  true_tam <- tamat(true_mat, order = rownames(true_mat), type = "pdag")
  est_tam <- tamat(est_mat, order = rownames(est_mat), type = "pdag")

  base <- confusion(est_disc, true_disc, type = "dir")
  expect_identical(confusion(est_tam, true_tam, type = "dir"), base)
  expect_identical(confusion(est_mat, true_mat, type = "dir"), base)
})

test_that("node order is aligned automatically", {
  true_disc <- new_discography(tibble::tibble(
    from = c("A", "B"),
    to = c("B", "C"),
    edge_type = c("---", "-->")
  ))
  est_disc <- new_discography(tibble::tibble(
    from = c("A", "C", "A"),
    to = c("B", "B", "C"),
    edge_type = c("---", "-->", "-->")
  ))

  true_tam <- tamat(amat(true_disc), order = c("A", "B", "C"), type = "pdag")
  est_mat <- amat(est_disc)
  est_tam <- tamat(est_mat[rev(rownames(est_mat)), rev(colnames(est_mat))],
    order = rev(rownames(est_mat)), type = "pdag"
  )

  got1 <- confusion(est_disc, true_disc, type = "adj")
  got2 <- confusion(est_tam, true_tam, type = "adj")
  expect_identical(got1, got2)
})

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
