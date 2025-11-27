test_that("disco respects tier-based background knowledge", {
  data("tpcExample")

  # Background knowledge: child < youth < old
  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  # Run TGES with TemporalBIC
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  out <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  edges <- out$caugi@edges

  # The node order is stored in out$caugi@nodes
  tier_index <- causalDisco:::.tier_index(kn, out$caugi@nodes)

  # Add tier indices to edge list
  edges$from_tier <- tier_index[match(edges$from, names(tier_index))]
  edges$to_tier <- tier_index[match(edges$to, names(tier_index))]

  # Forbidden: edges from later tier → earlier tier
  violating <- subset(edges, from_tier > to_tier)

  expect_true(nrow(violating) == 0)
})

test_that("disco respects required background knowledge", {
  skip("TGES does not yet support required/forbidden edges from knowledge objects.")
  data("tpcExample")

  kn <- knowledge(
    tpcExample,
    required(child_x1 ~ youth_x3),
    forbidden(child_x2 ~ oldage_x5)
  )

  # Run TGES with TemporalBIC
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  out <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  edges <- out$caugi@edges

  forbidden_present <- edge_df$from == "child_x2" & edge_df$to == "oldage_x5"

  expect_true(
    sum(forbidden_present) == 0,
    info = "Forbidden edge child_x2 --> oldage_x5 was found in the output graph."
  )

  required_present <- edge_df$from == "child_x1" & edge_df$to == "youth_x3"

  expect_true(
    sum(required_present) == 1,
    info = "Required edge child_x1 --> youth_x3 was found in the output graph."
  )
})
