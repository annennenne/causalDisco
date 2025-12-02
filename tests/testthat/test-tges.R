test_that("TGES respects tier-based background knowledge", {
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  my_tfci <- tges(engine = "causalDisco", score = "tbic")

  output <- disco(tpcExample, my_tfci, knowledge = kn)
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

  my_tfci <- tges(engine = "causalDisco", score = "tbic")

  output <- disco(tpcExample, my_tfci, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations) == 0, info = "Tier violations were found in the output graph.")
})

test_that("TGES respects required/forbidden background knowledge", {
  skip("TGES does not yet support required/forbidden edges from knowledge objects.")
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    required(child_x1 ~ youth_x3),
    forbidden(child_x2 ~ oldage_x5)
  )

  # Run TGES with TemporalBIC
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  set.seed(1405)
  out <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  edges <- out$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  missing_violation <- violations[violations$violation_type == "missing_required", ]
  forbidden_violation <- violations[violations$violation_type == "forbidden_present", ]

  expect_true(nrow(missing_violation) == 0, info = "Required edge not found in the output graph.")
  expect_true(nrow(forbidden_violation) == 0, info = "Forbidden edge found in the output graph.")
})
