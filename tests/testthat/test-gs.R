test_that("gs bnlearn algorithm run without error and return correct classes", {
  run_all_tests(
    alg_fun = gs,
    engine = "bnlearn",
    test = "fisher_z",
    alg_args = list(max.sx = 3, debug = FALSE, undirected = FALSE)
  )

  test_additional_test_or_score_args(
    alg_fun = gs,
    engine = "bnlearn",
    test = "mc_zf",
    test_args = list(B = 100)
  )
})
