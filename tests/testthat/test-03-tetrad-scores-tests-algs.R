library(testthat)

test_that("all known scores can be set without error", {
  skip_if_no_tetrad()

  scores <- c(
    "sem_bic",
    "ebic",
    "bdeu",
    "basis_function_bic",
    "conditional_gaussian",
    "degenerate_gaussian",
    "discrete_bic",
    "gic",
    "mag_degenerate_gaussian_bic",
    "mixed_variable_polynomial",
    "poisson_prior",
    "zhang_shen_bound"
  )

  purrr::walk(scores, \(s) {
    ts <- TetradSearch$new()
    expect_no_condition(ts$set_score(s))
  })
})

test_that("all known tests can be set without error (and mc path sets mc_test once)", {
  skip_if_no_tetrad()

  tests <- c(
    "chi_square",
    "g_square",
    "basis_function_lrt",
    "probabilistic",
    "fisher_z",
    "degenerate_gaussian",
    "cci",
    "conditional_gaussian",
    "kci"
  )

  # plain set
  purrr::walk(tests, \(tst) {
    ts <- TetradSearch$new()
    expect_no_condition(ts$set_test(tst))
  })

  # one explicit mc=TRUE path to cover mc_test
  ts_mc <- TetradSearch$new()
  expect_no_condition(ts_mc$set_test("chi_square", mc = TRUE))
  expect_jobj(ts_mc$mc_test)
})

test_that("unknown method names error clearly", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  expect_error(
    ts$set_score("definitely_not_a_score"),
    "Unknown score type using tetrad engine"
  )
  expect_error(
    ts$set_test("definitely_not_a_test"),
    "Unknown test type using tetrad engine"
  )
  expect_error(
    ts$set_alg("definitely_not_an_alg"),
    "Unknown method type using tetrad engine"
  )
})

test_that("set_alg() succeeds for score-only algorithms when a score is set", {
  skip_if_no_tetrad()

  score_only <- c(
    "fges",
    "fges_mb",
    "boss",
    "restricted_boss",
    "sp",
    "fask",
    "direct_lingam",
    "boss_pod"
  )

  purrr::walk(score_only, \(alg) {
    ts <- TetradSearch$new()
    ts$set_score("sem_bic")
    expect_no_condition(ts$set_alg(alg))
  })
})

test_that("set_alg() succeeds for test-only algorithms when a test is set", {
  skip_if_no_tetrad()

  test_only <- c(
    "pc",
    "cpc",
    "pc_max",
    "fci",
    "rfci",
    "cfci",
    "ccd"
  )

  purrr::walk(test_only, \(alg) {
    ts <- TetradSearch$new()
    ts$set_test("fisher_z")
    expect_no_condition(ts$set_alg(alg))
  })
})

test_that("set_alg() succeeds for algorithms that require both score and test", {
  skip_if_no_tetrad()

  both_required <- c(
    "gfci",
    "grasp",
    "grasp_fci",
    "sp_fci",
    "boss_fci",
    "fcit",
    "cstar"
  )

  purrr::walk(both_required, \(alg) {
    ts <- TetradSearch$new()
    ts$set_score("sem_bic")
    ts$set_test("fisher_z")
    expect_no_condition(ts$set_alg(alg))
  })
})

test_that("set_alg() succeeds for algorithms with no explicit precheck", {
  skip_if_no_tetrad()

  # these don't check prerequisites in set_alg(); just ensure they construct
  loose <- c(
    "fofc",
    "dagma",
    "ica_lingam",
    "ica_lingd"
  )

  purrr::walk(loose, \(alg) {
    ts <- TetradSearch$new()
    expect_no_condition(ts$set_alg(alg))
  })
})

test_that("set_alg() with svar_fci and svar_gfci", {
  skip_if_no_tetrad()

  df <- make_cont_test_data()
  ts <- TetradSearch$new()
  ts$set_score("sem_bic")
  ts$set_test("fisher_z")
  ts$set_data(df)
  expect_no_condition(ts$set_alg("svar_fci"))
  expect_no_condition(ts$set_alg("svar_gfci"))
})
