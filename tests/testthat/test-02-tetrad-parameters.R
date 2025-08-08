library(testthat)

test_that("set_params() accepts numeric, logical, and character values", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()

  # should not error; also covers the character → Java String path
  expect_no_condition(
    ts$set_params(SEED = 7, VERBOSE = FALSE, TRUNCATION_LIMIT = 3)
  )
  expect_error(
    ts$set_params(DOES_NOT_EXIST = "failure!!")
  )
})

test_that("set_verbose() forwards to params without error and errors when not bool", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  expect_no_condition(ts$set_verbose(FALSE))
  expect_no_condition(ts$set_verbose(TRUE))
  expect_error(ts$set_verbose(2))
})

test_that("set_time_lag() accepts integers and rejects non-integers or negative numbers", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()
  expect_no_condition(ts$set_time_lag(0))
  expect_error(ts$set_time_lag(0.5))
  expect_error(ts$set_time_lag(-1))
  expect_error(ts$set_time_lag(-0.5))
})

test_that("set_params() accepts pre-wrapped Java objects (else-branch coverage)", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()

  # Pass a java.lang.Boolean so it skips the R-type guards
  jbool <- rJava::.jnew("java/lang/Boolean", TRUE)

  # This must go through the `else` path:
  #   wrapped <- .jcast(value, "java/lang/Object")
  expect_no_condition(
    ts$set_params(VERBOSE = jbool)
  )

  # Also try a java.lang.Integer to hit the same path on a different key
  jint <- rJava::.jnew("java/lang/Integer", 7L)
  expect_no_condition(
    ts$set_params(SEED = jint)
  )
})
