test_that("simDAG works", {
  amat <- simDAG(4, sparsity = 0, permute = FALSE)
  expected_amat <- matrix(c(
    0, 0, 0, 0,
    1, 0, 0, 0,
    1, 1, 0, 0,
    1, 1, 1, 0
  ), nrow = 4, byrow = TRUE)
  rownames(expected_amat) <- colnames(expected_amat) <- paste("x", 1:4, sep = "")
  expect_equal(amat, expected_amat)
  
  amat <- simDAG(5, sparsityLim = c(0.5, 0.5))
  # (5 * 5 - 5) / (2 * 2) = 5 edges
  expect_equal(sum(amat), 5)
})
