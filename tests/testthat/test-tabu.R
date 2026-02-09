test_that("tabu bnlearn algorithm run without error and return correct classes", {
  run_all_tests(
    alg_fun = tabu,
    engine = "bnlearn",
    score = "ebic_g",
    alg_args = list(tabu = 3, debug = FALSE)
  )

  test_additional_test_or_score_args(
    alg_fun = tabu,
    engine = "bnlearn",
    score = "aic_g",
    test_args = list(k = 1)
  )
})
