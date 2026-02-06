test_that("sp_fci Tetrad disco respects tier knowledge", {
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

  tetrad_sp_fci <- sp_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_sp_fci, knowledge = kn)

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

  tetrad_sp_fci <- sp_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(tpc_example, tetrad_sp_fci, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Tier violations were found in the output graph."
  )
})

test_that("sp_fci Tetrad disco respects required background knowledge", {
  skip_if_no_tetrad()

  data(tpc_example)

  # TODO Works in unreleased version, so will work in next released version (7.6.11?)
  skip(
    "sp_fci Tetrad does not yet support required background knowledge correctly."
  )

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  tetrad_sp_fci <- sp_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_sp_fci, knowledge = kn)
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

  tetrad_sp_fci <- sp_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_sp_fci, knowledge = kn)
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

test_that("sp_fci Tetrad disco respects forbidden background knowledge", {
  skip_if_no_tetrad()

  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %!-->% youth_x3,
    child_x2 %!-->% child_x1
  )

  tetrad_sp_fci <- sp_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_sp_fci, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Required edge not found in the output graph."
  )
})

test_that("sp_fci Tetrad disco works with additional alg args", {
  skip_if_no_tetrad()
  data(num_data)
  sp_fci_tetrad <- sp_fci(
    engine = "tetrad",
    score = "poisson_prior",
    test = "rank_independence",
    depth = 3,
    max_disc_path_length = 5,
    use_heuristic = FALSE,
    complete_rule_set_used = FALSE,
    guarantee_pag = TRUE,
    verbose = FALSE
  )
  out <- disco(num_data, sp_fci_tetrad)

  expect_equal(class(out), c("knowledgeable_caugi", "knowledge"))
})

test_that("sp_fci Tetrad disco works with additional score+test args", {
  skip_if_no_tetrad()
  data(num_data)
  sp_fci_tetrad <- sp_fci(
    engine = "tetrad",
    score = "poisson_prior",
    test = "basis_function_blocks",
    poisson_lambda = 2,
    singularity_lambda = 0.1,
    basis_type = "legendre",
    truncation_limit = 2
  )
  out <- disco(num_data, sp_fci_tetrad)

  expect_equal(class(out), c("knowledgeable_caugi", "knowledge"))
})
