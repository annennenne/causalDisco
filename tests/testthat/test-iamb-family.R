test_that("iamb-family bnlearn algorithms run without error and return correct classes", {
  algorithms <- list(
    list(
      label = "fast_iamb",
      name = fast_iamb,
      engine = "bnlearn",
      test = c("mi_g", "mc_mi_g"),
      test_args = list(B = 100)
    ),
    list(
      label = "iamb",
      name = iamb,
      engine = "bnlearn",
      test = c("cor", "mc_cor"),
      test_args = list(B = 100)
    ),
    list(
      label = "iamb_fdr",
      name = iamb_fdr,
      engine = "bnlearn",
      test = c("cor", "smc_cor"),
      test_args = list(B = 100)
    ),
    list(
      label = "inter_iamb",
      name = inter_iamb,
      engine = "bnlearn",
      test = c("fisher_z", "mc_zf"),
      test_args = list(B = 100)
    )
  )

  for (alg in algorithms) {
    run_all_tests(
      alg_fun = alg$name,
      engine = alg$engine,
      test = alg$test[1],
      alg_args = list(max.sx = 3, debug = FALSE, undirected = FALSE)
    )
    # bnlearn can give warnings about vstructure not applicable (due to background knowledge); we supress these
    suppressWarnings(
      test_additional_test_or_score_args(
        alg_fun = alg$name,
        engine = alg$engine,
        test = alg$test[2],
        test_args = alg$test_args
      )
    )
  }
})
