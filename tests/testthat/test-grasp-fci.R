test_that("grasp_fci Tetrad algorithm run without error and return correct classes", {
  skip_if_no_tetrad()

  test_tier_knowledge(
    alg_fun = grasp_fci,
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )

  test_forbidden_knowledge(
    alg_fun = grasp_fci,
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )

  test_that("required works", {
    skip(
      "grasp_fci Tetrad does not yet support required background knowledge correctly."
    )
    test_required_knowledge(
      alg_fun = grasp_fci,
      engine = "tetrad",
      score = "sem_bic",
      test = "fisher_z"
    )
  })

  test_additional_alg_args(
    alg_fun = grasp_fci,
    engine = "tetrad",
    score = "poisson_prior",
    test = "rank_independence",
    alg_args = list(
      depth = 3,
      stable_fas = FALSE,
      max_disc_path_length = 5,
      covered_depth = 3,
      singular_depth = 2,
      nonsingular_depth = 2,
      ordered_alg = TRUE,
      raskutti_uhler = TRUE,
      use_data_order = FALSE,
      num_starts = 3,
      guarantee_pag = TRUE
    )
  )

  test_additional_test_or_score_args(
    alg_fun = grasp_fci,
    engine = "tetrad",
    score = "rank_bic",
    test = "poisson_prior",
    test_args = list(
      poisson_lambda = 2,
      singularity_lambda = 0.1,
      gamma = 0.5,
      penalty_discount = 3
    )
  )
})
