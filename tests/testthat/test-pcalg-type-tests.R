test_that(".get_pcalg_test_from_string error if computing suff_stat without X", {
  expect_error(
    .get_pcalg_test_from_string("g_square", suff_stat = TRUE),
    "X must be provided to compute sufficient statistic."
  )
})

test_that(".get_suff_stat works", {
  X <- data.frame(
    V1 = rnorm(10),
    V2 = rnorm(10)
  )
  suff_stat <- .get_suff_stat(X, method = "fisher_z_twd")
  expect_true(is.matrix(suff_stat))

  expect_error(
    .get_suff_stat("a", method = "fisher_z_mi"),
    "gaussMItest requires a list or a mids object."
  )

  suff_stat <- .get_suff_stat(list(X), method = "fisher_z_mi")
  expect_equal(suff_stat[2][[1]], 10)

  suff_stat <- .get_suff_stat(X, method = "g_square_twd")
  expect_equal(names(suff_stat), c("dm", "adaptDF"))

  expect_error(
    .get_suff_stat("a", method = "g_square_mi"),
    "disMItest requires a list or a mids object."
  )
  suff_stat <- .get_suff_stat(X, method = "g_square_mi")
  expect_equal(class(suff_stat), "data.frame")

  suff_stat <- .get_suff_stat(X, method = "conditional_gaussian_twd")
  expect_equal(class(suff_stat), "data.frame")

  expect_error(
    .get_suff_stat("a", method = "conditional_gaussian_mi"),
    "mixMItest requires a list or a mids object."
  )
  suff_stat <- .get_suff_stat(X, method = "conditional_gaussian_mi")
  expect_equal(class(suff_stat), "data.frame")

  expect_error(
    .get_suff_stat(X, method = "unknown_method"),
    "Unknown method: unknown_method"
  )
})

test_that(".classify_binary works", {
  expect_true(.classify_binary(c(TRUE, FALSE)))

  expect_true(.classify_binary(is.factor(c("a", "b"))))

  expect_true(.classify_binary(c(0, 1)))

  expect_false(.classify_binary(c(0, 1, 2)))
})
