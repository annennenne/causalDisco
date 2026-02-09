test_that("iamb_fdr bnlearn disco respects tier knowledge", {
  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  bnlearn_iamb_fdr <- iamb_fdr(engine = "bnlearn", test = "mi_g")
  output <- disco(
    data = tpc_example,
    method = bnlearn_iamb_fdr,
    knowledge = kn
  )

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

  bnlearn_iamb_fdr <- iamb_fdr(engine = "bnlearn", test = "mi_g")
  output <- disco(tpc_example, bnlearn_iamb_fdr, knowledge = kn)
  edges <- output$caugi@edges

  violations <- causalDisco:::check_tier_violations(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Tier violations were found in the output graph."
  )
})

test_that("iamb_fdr bnlearn disco respects required background knowledge", {
  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3
  )

  bnlearn_iamb_fdr <- iamb_fdr(engine = "bnlearn", test = "mi_g")
  output <- disco(
    data = tpc_example,
    method = bnlearn_iamb_fdr,
    knowledge = kn
  )
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

  bnlearn_iamb_fdr <- iamb_fdr(engine = "bnlearn", test = "mi_g")
  output <- disco(
    data = tpc_example,
    method = bnlearn_iamb_fdr,
    knowledge = kn
  )
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

test_that("iamb_fdr bnlearn disco respects forbidden background knowledge", {
  data(tpc_example)

  kn <- knowledge(
    tpc_example,
    child_x1 %!-->% youth_x3,
    child_x2 %!-->% child_x1
  )

  bnlearn_iamb_fdr <- iamb_fdr(engine = "bnlearn", test = "mi_g")
  output <- disco(
    data = tpc_example,
    method = bnlearn_iamb_fdr,
    knowledge = kn
  )
  edges <- output$caugi@edges

  violations <- causalDisco:::check_edge_constraints(edges, kn)
  expect_true(
    nrow(violations) == 0,
    info = "Required edge not found in the output graph."
  )
})

test_that("iamb_fdr bnlearn disco works with additional alg args", {
  data(num_data)
  iamb_fdr_bnlearn <- iamb_fdr(
    engine = "bnlearn",
    test = "mi_g",
    max.sx = 3,
    debug = FALSE,
    undirected = FALSE
  )
  out <- disco(num_data, iamb_fdr_bnlearn)

  expect_equal(class(out), c("knowledgeable_caugi", "knowledge"))
})

test_that("iamb_fdr bnlearn disco works with additional test args", {
  data(num_data)
  iamb_fdr_bnlearn <- iamb_fdr(
    engine = "bnlearn",
    test = "mc_zf",
    B = 100
  )
  out <- disco(num_data, iamb_fdr_bnlearn)

  expect_equal(class(out), c("knowledgeable_caugi", "knowledge"))
})
