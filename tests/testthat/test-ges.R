test_that("ges Tetrad disco respects tier knowledge", {
  skip_if_no_tetrad()

  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  tetrad_ges <- ges(engine = "tetrad", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_ges, knowledge = kn)

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

  tetrad_ges <- ges(engine = "tetrad", score = "sem_bic")
  output <- disco(tpc_example, tetrad_ges, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Tier violations were found in the output graph."
  )
})

test_that("ges Tetrad disco respects required background knowledge", {
  skip_if_no_tetrad()

  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  tetrad_ges <- ges(engine = "tetrad", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_ges, knowledge = kn)
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

  tetrad_ges <- ges(engine = "tetrad", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_ges, knowledge = kn)
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

test_that("ges Tetrad disco respects forbidden background knowledge", {
  skip_if_no_tetrad()

  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    child_x1 %!-->% youth_x3,
    child_x2 %!-->% child_x1
  )

  tetrad_ges <- ges(engine = "tetrad", score = "sem_bic")
  output <- disco(data = tpc_example, method = tetrad_ges, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Required edge not found in the output graph."
  )
})

#### pcalg ges tests

test_that("ges pcalg disco errors on tier knowledge", {
  # See ?as_pcalg_constraints - only forbidden edges are supported
  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  pcalg_ges <- ges(engine = "pcalg", score = "sem_bic")
  expect_error(
    disco(data = tpc_example, method = pcalg_ges, knowledge = kn),
    regexp = "pcalg does not support directed tier constraints."
  )
})

test_that("ges pcalg disco errors on required background knowledge", {
  # See ?as_pcalg_constraints - only forbidden edges are supported
  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  pcalg_ges <- ges(engine = "pcalg", score = "sem_bic")
  expect_error(
    disco(data = tpc_example, method = pcalg_ges, knowledge = kn),
    regexp = "pcalg does not support asymmetric edges."
  )
})

test_that("ges pcalg disco respects forbidden background knowledge", {
  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    child_x1 %!-->% youth_x3
  )

  pcalg_ges <- ges(engine = "pcalg", score = "sem_bic")
  expect_error(
    disco(data = tpc_example, method = pcalg_ges, knowledge = kn),
    regexp = "pcalg does not support asymmetric edges."
  )

  skip("ges pcalg gives fake warning?")

  kn <- knowledge(
    tpc_example,
    child_x1 %!-->% youth_x3,
    youth_x3 %!-->% child_x1
  )

  pcalg_ges <- ges(engine = "pcalg", score = "sem_bic")
  output <- disco(data = tpc_example, method = pcalg_ges, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Required edge not found in the output graph."
  )

  pcalg_ges <- ges(engine = "pcalg", score = "sem_bic")
  output <- disco(data = tpc_example, method = pcalg_ges)
  edges_new <- output$caugi@edges

  # Test the original edges had the forbidden edge
  forbidden_present <-
    (edges$from == "child_x1" & edges$to == "child_x2") |
    (edges$from == "child_x2" & edges$to == "child_x1")

  expect_true(
    sum(forbidden_present) >= 1,
    info = "Forbidden edge child_x1 --> child_x2 was not found in the output graph without this knowledge."
  )
})
