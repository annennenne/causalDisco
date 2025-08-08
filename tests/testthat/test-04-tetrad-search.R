library(testthat)

test_that("run_search() errors when pieces are missing", {
  skip_if_no_tetrad()

  # no data
  ts <- TetradSearch$new()
  ts$set_score("sem_bic")
  ts$set_alg("fges")
  expect_error(ts$run_search(),
    "No data is set",
    fixed = TRUE
  )

  # data but no algorithm
  ts2 <- TetradSearch$new()
  ts2$set_data(make_cont_test_data(n = 150))
  expect_error(ts2$run_search(),
    "No algorithm is set",
    fixed = TRUE
  )
})

test_that("FGES pipeline runs; toggles populate outputs; accessors work", {
  skip_if_no_tetrad()

  df <- make_cont_test_data(n = 150)

  ts <- TetradSearch$new()
  ts$set_data(df)
  ts$set_score("sem_bic")
  ts$set_alg("fges")

  # keep bootstrap light so CI stays snappy
  ts$set_bootstrapping(
    number_resampling       = 5L,
    percent_resample_size   = 50,
    add_original            = TRUE,
    with_replacement        = TRUE,
    resampling_ensemble     = 1L,
    seed                    = 1L
  )

  res <- ts$run_search(
    bootstrap = TRUE
  )

  # main return type
  expect_s3_class(res, "discography")

  # java result object exists
  expect_jobj(ts$get_java())

  # dot / amat are non-empty strings
  dot <- ts$get_dot()
  amat <- ts$get_amat()
  expect_type(dot, "character")
  expect_type(amat, "character")
  expect_true(nzchar(dot))
  expect_true(nzchar(amat))
})
