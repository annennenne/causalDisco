library(testthat)
test_that("getters work after a run", {
  skip_if_no_tetrad()

  df <- make_cont_test_data(n = 150)

  ts <- TetradSearch$new()
  ts$set_data(df)
  ts$set_score("sem_bic")
  ts$set_alg("fges")

  res <- ts$run_search()
  expect_s3_class(res, "discography")

  # data + java objects
  expect_jobj(ts$get_data())
  expect_jobj(ts$get_java())

  # get_string: default and explicit
  s1 <- ts$get_string()
  s2 <- ts$get_string(ts$get_java())
  expect_type(s1, "character")
  expect_true(nzchar(s1))
  expect_type(s2, "character")
  expect_true(nzchar(s2))

  # dot / amat
  dot <- ts$get_dot()
  amat <- ts$get_amat()
  expect_type(dot, "character")
  expect_true(nzchar(dot))
  expect_type(amat, "character")
  expect_true(nzchar(amat))
})

test_that("get_dot() and get_amat() cover explicit java_obj + cast_obj branch", {
  skip_if_no_tetrad()

  df <- make_cont_test_data(n = 120)

  ts <- TetradSearch$new()
  ts$set_data(df)
  ts$set_score("sem_bic")
  ts$set_alg("fges")
  ts$run_search()

  g <- ts$get_java() # a Graph jobjRef
  expect_jobj(g)

  # explicit java_obj → exercises cast_obj(java_obj) for Graph
  dot_explicit <- ts$get_dot(g)
  amat_explicit <- ts$get_amat(g)

  expect_type(dot_explicit, "character")
  expect_type(amat_explicit, "character")
  expect_true(nzchar(dot_explicit))
  expect_true(nzchar(amat_explicit))
})

test_that("get_parameters_for_function() returns names for an algorithm", {
  # alg branch
  ts <- TetradSearch$new()
  pars <- ts$get_parameters_for_function("fges", alg = TRUE)
  expect_type(pars, "character")
  expect_true(length(pars) >= 1)
})

test_that("get_parameters_for_function() returns names for a score", {
  # score branch: matches ^(set_|use_)sem_bic(_score)?$
  ts <- TetradSearch$new()
  pars <- ts$get_parameters_for_function("sem_bic", score = TRUE)
  expect_type(pars, "character")
  expect_true(length(pars) >= 1)
})

test_that("get_parameters_for_function() returns names for a test", {
  # test branch: matches ^(set_|use_)fisher_z(_test)?$
  ts <- TetradSearch$new()
  pars <- ts$get_parameters_for_function("fisher_z", test = TRUE)
  expect_type(pars, "character")
  expect_true(length(pars) >= 1)
})

test_that("get_parameters_for_function() errors when there is no match", {
  ts <- TetradSearch$new()
  expect_error(
    ts$get_parameters_for_function("this_pattern_matches_nothing", alg = TRUE),
    "There is 0 matches to the function pattern"
  )
})

test_that("get_parameters_for_function() enforces exclusivity of flags", {
  ts <- TetradSearch$new()
  expect_error(
    ts$get_parameters_for_function("fges", score = TRUE, alg = TRUE),
    "\\(Exclusively\\) one of them should be TRUE\\."
  )
})

test_that("get_parameters_for_function() errors if no flag is TRUE", {
  ts <- TetradSearch$new()
  expect_error(
    ts$get_parameters_for_function("fges"),
    "Score is: FALSE, test is: FALSE, and alg is: FALSE. (Exclusively) one of them should be TRUE",
    fixed = TRUE
  )
})
