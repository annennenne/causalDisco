test_that("hc bnlearn algorithm run without error and return correct classes", {
  run_all_tests(
    alg_fun = hc,
    engine = "bnlearn",
    score = "ebic_g",
    alg_args = list(
      debug = FALSE,
      restart = 10,
      perturb = 1,
      max.iter = Inf,
      maxp = Inf,
      optimized = TRUE
    )
  )

  test_additional_test_or_score_args(
    alg_fun = hc,
    engine = "bnlearn",
    score = "aic_g",
    test_args = list(k = 1)
  )
})
