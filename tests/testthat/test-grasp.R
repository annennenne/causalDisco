test_that("grasp Tetrad disco respects tier knowledge", {
  skip_if_no_tetrad()

  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  tetrad_grasp <- grasp(engine = "tetrad", test = "fisher_z", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_grasp, knowledge = kn)

  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Tier violations were found in the output graph."
  )

  kn <- knowledge(
    tpc_example,
    tier(
      1 ~ starts_with("old"),
      2 ~ starts_with("youth"),
      3 ~ starts_with("child")
    )
  )

  tetrad_grasp <- grasp(engine = "tetrad", test = "fisher_z", score = "sem_bic")
  output <- disco(tpc_example, tetrad_grasp, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Tier violations were found in the output graph."
  )
})

test_that("grasp Tetrad disco respects required background knowledge", {
  skip_if_no_tetrad()

  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  tetrad_grasp <- grasp(engine = "tetrad", test = "fisher_z", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_grasp, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Required edge not found in the output graph."
  )

  # With tier+required knowledge

  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    ),
    youth_x3 %-->% oldage_x5
  )

  tetrad_grasp <- grasp(engine = "tetrad", test = "fisher_z", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_grasp, knowledge = kn)
  edges <- output$caugi@edges

  violations_tiers <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(
    nrow(violations_tiers) == 0,
    info = "Tier violations were found in the output graph."
  )

  violations_req <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations_req) == 0,
    info = "Required edge not found in the output graph."
  )
})

test_that("grasp Tetrad disco respects forbidden background knowledge", {
  skip_if_no_tetrad()

  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %!-->% youth_x3,
    child_x2 %!-->% child_x1
  )

  tetrad_grasp <- grasp(engine = "tetrad", test = "fisher_z", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_grasp, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Required edge not found in the output graph."
  )
})

test_that("grasp Tetrad disco works with additional args", {
  skip_if_no_tetrad()
  data(num_data)
  grasp_tetrad <- grasp(
    engine = "tetrad",
    test = "poisson_prior",
    score = "rank_bic",
    covered_depth = 3,
    singular_depth = 2,
    ordered_alg = TRUE,
    raskutti_uhler = TRUE,
    use_data_order = FALSE,
    num_starts = 3
  )
  out <- disco(num_data, grasp_tetrad)

  expect_equal(class(out), c("knowledgeable_caugi", "knowledge"))
})

test_that("grasp_fci Tetrad disco works with additional test and score args", {
  skip_if_no_tetrad()
  data(num_data)
  grasp_tetrad <- grasp(
    engine = "tetrad",
    test = "poisson_prior",
    score = "rank_bic",
    alpha = 0.05,
    poisson_lambda = 2,
    singularity_lambda = 0.1,
    gamma = 0.5,
    penalty_discount = 3
  )
  out <- disco(num_data, grasp_tetrad)

  expect_equal(class(out), c("knowledgeable_caugi", "knowledge"))
})
