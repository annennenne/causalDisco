test_that("sim_gaus_from_dag respects causal structure", {
  set.seed(1405)
  amat <- matrix(c(
    0, 0, 1, 0,
    0, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 0, 0
  ), nrow = 4, byrow = TRUE)
  rownames(amat) <- colnames(amat) <- paste("x", 1:4, sep = "")

  dat <- sim_gaus_from_dag(amat, 1000, pnegRegpar = 0, standardize = TRUE)
  corr_mat <- cor(dat)

  # Check that correlations respect the DAG structure
  expect_true(abs(corr_mat[1, 4]) < 0.2)
  expect_true(abs(corr_mat[2, 4]) < 0.2)
  expect_true(abs(corr_mat[3, 4]) < 0.2)
  expect_true(abs(corr_mat[1, 2]) > 0.5)
  expect_true(abs(corr_mat[1, 3]) > 0.5)
  expect_true(abs(corr_mat[2, 3]) > 0.5)
})
