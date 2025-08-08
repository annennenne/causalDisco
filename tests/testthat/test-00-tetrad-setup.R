library(testthat)

test_that("continuous generator returns standardized X1..X5", {
  skip_if_no_tetrad()

  df <- make_cont_test_data() # n = 300 by default
  expect_s3_class(df, "data.frame")
  expect_setequal(names(df), paste0("X", 1:5))
  expect_true(all(purrr::map_lgl(df, is.numeric)))

  # means ~ 0, sds ~ 1
  m <- vapply(df, mean, numeric(1))
  s <- vapply(df, stats::sd, numeric(1))
  expect_true(all(abs(m) < 0.05))
  expect_true(all(abs(s - 1) < 0.05))
})

test_that("discrete generator returns integer-coded X1..X5 in 0..k-1", {
  skip_if_no_tetrad()

  k <- 3
  df <- make_disc_test_data(n = 300, k = k)
  expect_s3_class(df, "data.frame")
  expect_setequal(names(df), paste0("X", 1:5))
  expect_true(all(purrr::map_lgl(df, is.integer)))

  rng_ok <- purrr::map_lgl(df, \(x) all(x >= 0L & x <= (k - 1L)))
  expect_true(all(rng_ok))
})

test_that("knowledge helper builds tiered/required/forbidden objects and casts to Tetrad", {
  skip_if_no_tetrad()

  df <- make_cont_test_data()
  kn <- make_knowledge_test_object(df)

  expect_true(is.list(kn))
  expect_true(all(c("tiered_kn", "forbidden_kn", "required_kn", "combi_kn") %in% names(kn)))

  # should cast to a Java Knowledge via your package API
  tk <- as_tetrad_knowledge(kn$combi_kn)
  expect_jobj(tk)

  # TetradSearch should accept it without complaint
  ts <- TetradSearch$new()
  expect_no_condition(ts$set_knowledge(kn$combi_kn))
  expect_jobj(ts$get_knowledge())
})

test_that("Java and Tetrad JARs are ready", {
  skip_if_no_tetrad()

  jars <- find_tetrad_jars()
  expect_true(length(jars) > 0)
  expect_true(all(file.exists(jars)))
})

test_that("TetradSearch constructor sets defaults correctly", {
  skip_if_no_tetrad()

  ts <- TetradSearch$new()

  # java-side objects exist
  expect_jobj(ts$knowledge)
  expect_jobj(ts$params)

  # everything else starts NULL
  expect_null(ts$data)
  expect_null(ts$rdata)
  expect_null(ts$score)
  expect_null(ts$test)
  expect_null(ts$mc_test)
  expect_null(ts$alg)
  expect_null(ts$java)
  expect_null(ts$result)
  expect_null(ts$bootstrap_graphs)
  expect_null(ts$mc_ind_results)
  expect_null(ts$bhat)
  expect_null(ts$unstable_bhats)
  expect_null(ts$stable_bhats)
})
