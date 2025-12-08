test_that("pc Tetrad disco respects tier knowledge", {
  tetrad_installed <- check_tetrad_install()$installed
  java_ok <- check_tetrad_install()$java_ok
  if (!tetrad_installed || !java_ok) {
    skip("Tetrad is not installed or Java version is insufficient. Skipping test.")
  }

  skip("pc Tetrad does not yet support tier knowledge correctly.")

  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpcExample, method = tetrad_pc, knowledge = kn)

  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations) == 0, info = "Tier violations were found in the output graph.")

  kn <- knowledge(
    tpcExample,
    tier(
      1 ~ starts_with("old"),
      2 ~ starts_with("youth"),
      3 ~ starts_with("child")
    )
  )

  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(tpcExample, tetrad_pc, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations) == 0, info = "Tier violations were found in the output graph.")
})

test_that("pc Tetrad disco respects required background knowledge", {
  tetrad_installed <- check_tetrad_install()$installed
  java_ok <- check_tetrad_install()$java_ok
  if (!tetrad_installed || !java_ok) {
    skip("Tetrad is not installed or Java version is insufficient. Skipping test.")
  }

  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    required(child_x1 ~ youth_x3)
  )

  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpcExample, method = tetrad_pc, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(nrow(violations) == 0, info = "Required edge not found in the output graph.")

  # With tier+required knowledge
  skip("pc Tetrad does not yet support knowledge with both tiers+required edges.") # Fails because of tiers I think

  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    ),
    required(youth_x3 ~ oldage_x5)
  )

  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpcExample, method = tetrad_pc, knowledge = kn)
  edges <- output$caugi@edges

  violations_tiers <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations_tiers) == 0, info = "Tier violations were found in the output graph.")

  violations_req <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(nrow(violations_req) == 0, info = "Required edge not found in the output graph.")
})

test_that("pc Tetrad disco respects forbidden background knowledge", {
  tetrad_installed <- check_tetrad_install()$installed
  java_ok <- check_tetrad_install()$java_ok
  if (!tetrad_installed || !java_ok) {
    skip("Tetrad is not installed or Java version is insufficient. Skipping test.")
  }

  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    forbidden(child_x1 ~ youth_x3),
    forbidden(child_x2 ~ child_x1)
  )

  tetrad_pc <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpcExample, method = tetrad_pc, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(nrow(violations) == 0, info = "Required edge not found in the output graph.")

  # Verify it actually changes the output when adding forbidden knowledge
  tetrad_pc_no_kn <- pc(engine = "tetrad", test = "conditional_gaussian", alpha = 0.05)
  out_no_kn <- disco(data = tpcExample, method = tetrad_pc_no_kn)
  edges_no_kn <- out_no_kn$caugi@edges

  # The forbidden edge is present
  forbidden_present <- edges_no_kn$from == "child_x2" & edges_no_kn$to == "oldage_x5"
  expect_true(
    sum(forbidden_present) >= 1,
    info = "Forbidden edge child_x2 --> oldage_x5 was not found in the output graph without knowledge."
  )
})

#### pcalg PC tests

test_that("pc pcalg disco errors on tier knowledge", {
  # See ?as_pcalg_constraints - only forbidden edges are supported
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  pcalg_pc <- pc(engine = "pcalg", test = "conditional_gaussian", alpha = 0.05)
  expect_error(
    disco(data = tpcExample, method = pcalg_pc, knowledge = kn),
    regexp = "pcalg does not support directed tier constraints."
  )
})

test_that("pc pcalg disco errors on required background knowledge", {
  # See ?as_pcalg_constraints - only forbidden edges are supported
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    required(child_x1 ~ youth_x3)
  )

  pcalg_pc <- pc(engine = "pcalg", test = "conditional_gaussian", alpha = 0.05)
  expect_error(
    disco(data = tpcExample, method = pcalg_pc, knowledge = kn),
    regexp = "pcalg does not support asymmetric edges."
  )
})

test_that("pc pcalg disco respects forbidden background knowledge", {
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    forbidden(child_x1 ~ youth_x3)
  )

  pcalg_pc <- pc(engine = "pcalg", test = "conditional_gaussian", alpha = 0.05)
  expect_error(
    disco(data = tpcExample, method = pcalg_pc, knowledge = kn),
    regexp = "pcalg does not support asymmetric edges."
  )

  kn <- knowledge(
    tpcExample,
    forbidden(child_x1 ~ youth_x3),
    forbidden(youth_x3 ~ child_x1)
  )

  pcalg_pc <- pc(engine = "pcalg", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpcExample, method = pcalg_pc, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(nrow(violations) == 0, info = "Required edge not found in the output graph.")

  pcalg_pc <- pc(engine = "pcalg", test = "conditional_gaussian", alpha = 0.05)
  output <- disco(data = tpcExample, method = pcalg_pc)
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
