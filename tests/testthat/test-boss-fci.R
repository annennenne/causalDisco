test_that("boss_fci Tetrad disco respects tier knowledge", {
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

  tetrad_boss_fci <- boss_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_boss_fci, knowledge = kn)

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

  tetrad_boss_fci <- boss_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(tpc_example, tetrad_boss_fci, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Tier violations were found in the output graph."
  )
})

test_that("boss_fci Tetrad disco respects required background knowledge", {
  skip_if_no_tetrad()

  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )
  skip(
    "boss_fci Tetrad runs forever with required. See #1950 in Tetrad."
  )

  tetrad_boss_fci <- boss_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_boss_fci, knowledge = kn)
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

  tetrad_boss_fci <- boss_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_boss_fci, knowledge = kn)
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

test_that("boss_fci Tetrad disco respects forbidden background knowledge", {
  skip_if_no_tetrad()

  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %!-->% youth_x3,
    child_x2 %!-->% child_x1
  )

  tetrad_boss_fci <- boss_fci(
    engine = "tetrad",
    score = "sem_bic",
    test = "fisher_z"
  )
  output <- disco(data = tpc_example, method = tetrad_boss_fci, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Required edge not found in the output graph."
  )
})

test_that("boss_fci Tetrad disco works with additional args", {
  skip_if_no_tetrad()
  data(num_data)
  boss_fci_tetrad <- boss_fci(
    engine = "tetrad",
    score = "poisson_prior",
    test = "rank_independence",
    depth = 3,
    max_disc_path_length = 5,
    use_bes = FALSE,
    use_heuristic = FALSE,
    complete_rule_set_used = FALSE,
    guarantee_pag = TRUE
  )
  out <- disco(num_data, boss_fci_tetrad)

  expect_equal(class(out), c("knowledgeable_caugi", "knowledge"))
})
