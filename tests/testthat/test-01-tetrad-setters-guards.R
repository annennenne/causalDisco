library(testthat)

test_that("set_test accepts known tests and rejects unknown; mc path sets mc_test", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  expect_no_condition(ts$set_test("fisher_z", alpha = 0.05))
  expect_no_condition(ts$set_test("chi_square", mc = TRUE))
  expect_jobj(ts$mc_test)

  expect_error(
    ts$set_test("definitely_not_a_test"),
    "Unknown test type using tetrad engine"
  )
})

test_that("set_score accepts known scores and rejects unknown", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  expect_no_condition(ts$set_score("sem_bic"))
  expect_no_condition(ts$set_score("discrete_bic"))

  expect_error(
    ts$set_score("definitely_not_a_score"),
    "Unknown score type using tetrad engine"
  )
})

test_that("set_alg enforces prerequisites for score-only, test-only, and both", {
  skip_if_no_tetrad()

  # score-only alg requires score
  ts1 <- TetradSearch$new()
  expect_error(ts1$set_alg("fges"), "No score is set")
  ts1$set_score("sem_bic")
  expect_no_condition(ts1$set_alg("fges"))

  # test-only alg requires test
  ts2 <- TetradSearch$new()
  expect_error(ts2$set_alg("pc"), "No test is set")
  ts2$set_test("fisher_z")
  expect_no_condition(ts2$set_alg("pc"))

  # both-required alg needs both
  ts3 <- TetradSearch$new()
  ts3$set_score("discrete_bic")
  expect_error(ts3$set_alg("gfci"), "No test is set")
  ts3$set_test("chi_square")
  expect_no_condition(ts3$set_alg("gfci"))
})

test_that("set_alg warns when background knowledge is set but algorithm ignores it", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  df <- make_cont_test_data()
  kn <- make_knowledge_test_object(df)

  ts$set_score("sem_bic")
  ts$set_knowledge(kn$tiered_kn)
  expect_warning(
    ts$set_alg("restricted_boss"),
    "This algorithm does not use background knowledge"
  )
})

test_that("set_knowledge attaches and propagates to existing algorithm", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  df <- make_cont_test_data()
  kn <- make_knowledge_test_object(df)

  ts$set_score("sem_bic")
  ts$set_alg("fges")

  # set after algorithm => should propagate without error
  expect_no_condition(ts$set_knowledge(kn$tiered_kn))
  expect_jobj(ts$get_knowledge())
})

test_that("set_knowledge propagates to an already-set algorithm", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  df <- make_cont_test_data()
  kn <- make_knowledge_test_object(df)

  ts$set_score("sem_bic")
  ts$set_alg("fges") # algo first
  expect_no_condition(ts$set_knowledge(kn$tiered_kn)) # should push into alg branch
  expect_jobj(ts$get_knowledge())
})

test_that("warning about ignored knowledge only fires when knowledge is set", {
  skip_if_no_tetrad()

  # no knowledge -> no warning
  ts0 <- TetradSearch$new()
  ts0$set_score("sem_bic")
  expect_no_condition(ts0$set_alg("restricted_boss"))

  # with knowledge -> warning
  ts1 <- TetradSearch$new()
  df <- make_cont_test_data()
  kn <- make_knowledge_test_object(df)
  ts1$set_score("sem_bic")
  ts1$set_knowledge(kn$tiered_kn)
  expect_warning(
    ts1$set_alg("restricted_boss"),
    "This algorithm does not use background knowledge"
  )
})
