test_that(".get_pcalg_test_from_string error if computing suffStat without X", {
  expect_error(
    .get_pcalg_test_from_string("g_square", suffStat = TRUE),
    "X must be provided to compute sufficient statistic."
  )
})

test_that(".get_suffStat works", {
  X <- data.frame(
    V1 = rnorm(10),
    V2 = rnorm(10)
  )
  suffStat <- .get_suffStat(X, method = "fisher_z_twd")
  expect_true(is.matrix(suffStat))

  expect_error(
    .get_suffStat("a", method = "fisher_z_mi"),
    "gaussMItest requires a list or a mids object."
  )

  suffStat <- .get_suffStat(list(X), method = "fisher_z_mi")
  expect_equal(suffStat[2][[1]], 10)

  suffStat <- .get_suffStat(X, method = "g_square_twd")
  expect_equal(names(suffStat), c("dm", "adaptDF"))

  expect_error(
    .get_suffStat("a", method = "g_square_mi"),
    "disMItest requires a list or a mids object."
  )
  suffStat <- .get_suffStat(X, method = "g_square_mi")
  expect_equal(class(suffStat), "data.frame")

  suffStat <- .get_suffStat(X, method = "conditional_gaussian_twd")
  expect_equal(class(suffStat), "data.frame")

  expect_error(
    .get_suffStat("a", method = "conditional_gaussian_mi"),
    "mixMItest requires a list or a mids object."
  )
  suffStat <- .get_suffStat(X, method = "conditional_gaussian_mi")
  expect_equal(class(suffStat), "data.frame")

  expect_error(
    .get_suffStat(X, method = "unknown_method"),
    "Unknown method: unknown_method"
  )
})

test_that(".classify_binary works", {
  expect_true(.classify_binary(c(TRUE, FALSE)))

  expect_true(.classify_binary(is.factor(c("a", "b"))))

  expect_true(.classify_binary(c(0, 1)))

  expect_false(.classify_binary(c(0, 1, 2)))
})
