test_that("tpc causalDisco arguments to tfci_run can be passed along correctly", {
  # Just test no warning given
  data("tpc_example")

  my_tpc <- tpc(engine = "causalDisco", test = "fisher_z", method = "stable")

  expect_no_warning(disco(tpc_example, my_tpc))
})

test_that("tpc causalDisco respects tier knowledge", {
  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  my_tpc <- tpc(engine = "causalDisco", test = "fisher_z")

  output <- disco(tpc_example, my_tpc, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations) == 0, info = "Tier violations were found in the output graph.")

  kn <- knowledge(
    tpc_example,
    tier(
      1 ~ starts_with("old"),
      2 ~ starts_with("youth"),
      3 ~ starts_with("child")
    )
  )

  my_tpc <- tpc(engine = "causalDisco", test = "fisher_z")
  output <- disco(tpc_example, my_tpc, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(nrow(violations) == 0, info = "Tier violations were found in the output graph.")
})

test_that("tpc causalDisco respects required background knowledge", {
  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  my_tpc <- tpc(engine = "causalDisco", test = "fisher_z")
  out <- disco(data = tpc_example, method = my_tpc, knowledge = kn)

  edges <- out$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)

  expect_true(nrow(violations) == 0, info = "Required edge not found in the output graph.")

  # edges contains child_x1 -> child_x2. Verify graph changes when we require child_x2 -> child_x1 instead.
  skip("tpc causalDisco does not yet support required edges from knowledge objects.") # Above works due to chance

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3,
    child_x2 %-->% child_x1
  )

  my_tpc <- tpc(engine = "causalDisco", test = "fisher_z")
  out <- disco(data = tpc_example, method = my_tpc, knowledge = kn)

  edges <- out$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)

  expect_true(nrow(violations) == 0, info = "Required edge not found in the output graph.")
})

test_that("tpc causalDisco respects forbidden background knowledge", {
  data("tpc_example")

  kn <- knowledge(
    tpc_example,
    child_x2 %--x% oldage_x5
  )

  my_tpc <- tpc(engine = "causalDisco", test = "fisher_z")
  out <- disco(data = tpc_example, method = my_tpc, knowledge = kn)

  edges <- out$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)

  expect_true(nrow(violations) == 0, info = "Forbidden edges were found in the output graph.")

  # edges contains oldage_x6 -> oldage_x5. Verify graph changes when we forbid oldage_x5 -> oldage_x6.
  kn <- knowledge(
    tpc_example,
    child_x2 %--x% oldage_x5,
    oldage_x5 %--x% oldage_x6
  )

  my_tpc <- tpc(engine = "causalDisco", test = "fisher_z")
  out <- disco(data = tpc_example, method = my_tpc, knowledge = kn)
  edges <- out$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(nrow(violations) == 0, info = "Forbidden edges were found in the output graph.")
})
