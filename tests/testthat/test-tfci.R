test_that("tfci disco respects tier knowledge", {
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  my_tfci <- tfci(engine = "causalDisco", test = "fisher_z")

  output <- disco(tpcExample, my_tfci, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations) == 0, info = "Tier violations were found in the output graph.")

  skip() # Doesn't work yet
  kn <- knowledge(
    tpcExample,
    tier(
      1 ~ starts_with("old"),
      2 ~ starts_with("youth"),
      3 ~ starts_with("child")
    )
  )

  my_tfci <- tfci(engine = "causalDisco", test = "fisher_z")
  set.seed(1405)
  output <- disco(tpcExample, my_tfci, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations) == 0, info = "Tier violations were found in the output graph.")
})

test_that("tfci respects required background knowledge", {
  skip("tfci does not yet support required edges from knowledge objects.")
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    required(child_x1 ~ youth_x3)
  )

  cd_tges <- tfci(engine = "causalDisco", test = "fisher_z")
  set.seed(1405)
  out <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  edges <- out$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)

  expect_true(nrow(violations) == 0, info = "Required edge not found in the output graph.")
})

test_that("tfci respects forbidden background knowledge", {
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    forbidden(child_x2 ~ oldage_x5)
  )

  cd_tges <- tfci(engine = "causalDisco", test = "fisher_z")
  set.seed(1405)
  out <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  edges <- out$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)

  expect_true(nrow(violations) == 0, info = "Forbidden edges were found in the output graph.")

  # Verify it actually changes the output when adding forbidden knowledge
  cd_tges_no_kn <- tfci(engine = "causalDisco", test = "fisher_z")
  set.seed(1405)
  out_no_kn <- disco(data = tpcExample, method = cd_tges_no_kn, knowledge = knowledge())
  edges_no_kn <- out_no_kn$caugi@edges

  # The forbidden edge is present
  forbidden_present <- edges_no_kn$from == "child_x2" & edges_no_kn$to == "oldage_x5"
  expect_true(
    sum(forbidden_present) >= 1,
    info = "Forbidden edge child_x2 --> oldage_x5 was not found in the output graph without knowledge."
  )
})
