skip_if_not_installed("R6")
skip_if_not_installed("pcalg")
skip_if_not_installed("rlang")
skip_if_not_installed("stats")

# ──────────────────────────────────────────────────────────────────────────────
# Scores
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_score builds temporal scores lazily and errors on unknown", {
  s_bad <- causalDiscoSearch$new()
  expect_error(
    s_bad$set_score("not-a-score"),
    "Unknown score type using causalDisco engine: not-a-score",
    fixed = TRUE
  )

  df_g <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(df_g) <- paste0("p1_X", 1:4)

  s_bic <- causalDiscoSearch$new()
  s_bic$set_data(df_g, set_suff_stat = FALSE)
  s_bic$set_score("tbic")
  sc_bic <- s_bic$.__enclos_env__$private$score_function()
  expect_true(methods::is(sc_bic, "TemporalBIC"))

  df_d <- data.frame(
    A = factor(sample(letters[1:3], 100, TRUE)),
    B = factor(sample(letters[1:2], 100, TRUE))
  )
  s_bdeu <- causalDiscoSearch$new()
  s_bdeu$set_data(df_d, set_suff_stat = FALSE)
  s_bdeu$set_score("tbdeu")
  sc_bdeu <- s_bdeu$.__enclos_env__$private$score_function()
  expect_true(methods::is(sc_bdeu, "TemporalBDeu"))

  s_err <- causalDiscoSearch$new()
  s_err$set_score("tbic")
  expect_error(
    s_err$.__enclos_env__$private$score_function(),
    "Data must be set before score.",
    fixed = TRUE
  )
})

test_that("set_score internal unsupported method branch errors", {
  s <- causalDiscoSearch$new()
  s$set_data(data.frame(X = rnorm(100)), set_suff_stat = FALSE)
  s$set_score("tbic")
  s$.__enclos_env__$private$score_method <- "unknown-internal"
  expect_error(
    s$.__enclos_env__$private$score_function(),
    "Internal: unsupported score method.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Tests
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_test maps names", {
  s <- causalDiscoSearch$new()

  s$set_test("fisher_z")
  expect_identical(s$test, corTest)

  s$set_test("reg")
  expect_identical(s$test, regTest)

  expect_error(
    s$set_test("nope"),
    "Unknown test type using causalDisco engine: nope",
    fixed = TRUE
  )
})

test_that("set_test unknown method errors", {
  s <- causalDiscoSearch$new()
  expect_error(
    s$set_test("not-a-test"),
    "Unknown test type using causalDisco engine: not-a-test",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Initialization
# ──────────────────────────────────────────────────────────────────────────────

test_that("initialize sets sensible defaults", {
  s <- causalDiscoSearch$new()
  expect_null(s$data)
  expect_null(s$score)
  expect_null(s$test)
  expect_null(s$knowledge)
  expect_type(s$params, "list")
  expect_identical(s$params$methodNA, "none")
  expect_null(s$suff_stat)
  expect_null(s$alg)
  expect_null(s$continuous)
})

# ──────────────────────────────────────────────────────────────────────────────
# Sufficient Statistics
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_suff_stat requires data and test; builds correct suff stats", {
  s <- causalDiscoSearch$new()

  expect_error(
    s$set_suff_stat(),
    "Data must be set before sufficient statistic.",
    fixed = TRUE
  )

  s$data <- data.frame(X = rnorm(100), Y = rnorm(100))
  expect_error(
    s$set_suff_stat(),
    "Test must be set before sufficient statistic.",
    fixed = TRUE
  )

  s$set_test("reg")
  expect_silent(s$set_suff_stat())
  expect_true(is.list(s$suff_stat))
  expect_true("data" %in% names(s$suff_stat) || "C" %in% names(s$suff_stat) ||
    "binary" %in% names(s$suff_stat))

  s$set_test("fisher_z")
  expect_silent(s$set_suff_stat())
  expect_named(s$suff_stat, c("C", "n"))

  s$data <- c(1, 2, 3)
  expect_error(
    s$set_suff_stat(),
    "Data must be a matrix or data frame.",
    fixed = TRUE
  )
})

test_that("set_suff_stat covers reg, cor and bad-type paths", {
  s <- causalDiscoSearch$new()
  df <- data.frame(X = rnorm(100), Y = rnorm(100))

  s$set_test("reg", alpha = 0.01)
  s$set_data(df, set_suff_stat = TRUE)
  expect_true(is.list(s$suff_stat))

  s$set_test("fisher_z", alpha = 0.01)
  s$set_data(df, set_suff_stat = FALSE)
  expect_silent(s$set_suff_stat())
  expect_named(s$suff_stat, c("C", "n"))

  expect_error(
    s$set_test("bad"),
    "Unknown test type using causalDisco engine: bad",
    fixed = TRUE
  )

  s$data <- c(1, 2, 3)
  expect_error(
    s$set_suff_stat(),
    "Data must be a matrix or data frame.",
    fixed = TRUE
  )
})

test_that("set_data triggers set_suff_stat when requested", {
  s <- causalDiscoSearch$new()
  df <- matrix(rnorm(100), ncol = 2) |> as.data.frame()
  colnames(df) <- c("X", "Y")
  s$set_test("fisher_z")
  expect_silent(s$set_data(df, set_suff_stat = TRUE))
  expect_named(s$suff_stat, c("C", "n"))
})

# ──────────────────────────────────────────────────────────────────────────────
# Other setters
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_params stores values and respects reserved keys", {
  s <- causalDiscoSearch$new()

  s$set_params(list(alpha = 0.05, method = "stable.fast"))
  expect_identical(s$params$alpha, 0.05)
  expect_identical(s$params$method, "stable.fast")

  expect_error(
    s$set_params(list(data = iris)),
    "reserved and cannot be set via set_params",
    fixed = TRUE
  )
})

test_that("set_params(NULL) is a no-op and returns invisibly", {
  s <- causalDiscoSearch$new()
  before <- s$params
  expect_invisible(s$set_params(NULL))
  expect_identical(s$params, before)
})

test_that("set_data stores data; can skip suff stat", {
  s <- causalDiscoSearch$new()

  df <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(df) <- c("X", "Y", "Z", "W")

  s$set_data(df, set_suff_stat = FALSE)
  expect_identical(s$data, df)
  expect_null(s$suff_stat)
})

test_that("set_knowledge assigns to self$knowledge and validates", {
  s <- causalDiscoSearch$new()

  expect_error(
    s$set_knowledge(knowledge_obj = 123),
    class = "simpleError"
  )

  df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
  kn <- knowledge(
    df,
    tier(
      early ~ tidyselect::starts_with("a"),
      late ~ tidyselect::starts_with("b")
    )
  )
  expect_silent(s$set_knowledge(kn))
  expect_identical(s$knowledge, kn)
})

# ──────────────────────────────────────────────────────────────────────────────
# Alg
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_alg builds callables and guards correctly", {
  s <- causalDiscoSearch$new()

  expect_error(
    s$set_alg("tpc"),
    "No test is set. Use set_test() first.",
    fixed = TRUE
  )
  expect_error(
    s$set_alg("tfci"),
    "No test is set. Use set_test() first.",
    fixed = TRUE
  )

  s$set_test("fisher_z")
  s$set_alg("tpc")
  expect_true(is.function(s$alg))

  s$set_alg("tfci")
  expect_true(is.function(s$alg))

  s2 <- causalDiscoSearch$new()
  s2$set_alg("tges")
  expect_true(is.function(s2$alg))

  expect_error(
    s$set_alg("nope"),
    "Unknown method type using causalDisco engine: nope",
    fixed = TRUE
  )
})

test_that("set_alg builds tpc/tfci callables and unknown errors", {
  s <- causalDiscoSearch$new()
  s$set_test("fisher_z")
  s$set_alg("tpc")
  expect_true(is.function(s$alg))

  s$set_alg("tfci")
  expect_true(is.function(s$alg))

  expect_error(
    s$set_alg("nope"),
    "Unknown method type using causalDisco engine: nope",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# run_search
# ──────────────────────────────────────────────────────────────────────────────

test_that("run_search errors are thrown in the right order", {
  s <- causalDiscoSearch$new()

  expect_error(
    s$run_search(),
    "No data is set. Use set_data() first or pass data to run_search().",
    fixed = TRUE
  )

  df <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(df) <- c("X", "Y", "Z", "W")
  s$set_data(df, set_suff_stat = FALSE)

  expect_error(
    s$run_search(),
    "No algorithm is set. Use set_alg() first.",
    fixed = TRUE
  )

  s$set_test("fisher_z")
  s$set_alg("tpc")
  expect_error(
    s$run_search(),
    "No sufficient statistic is set. Use set_data() first.",
    fixed = TRUE
  )
})

test_that("run_search returns discography for tpc success path", {
  skip_if_not_installed("tidyselect")
  set.seed(1)
  df <- data.frame(
    p1_x = rnorm(100),
    p1_y = rnorm(100),
    p2_z = rnorm(100)
  )
  kn <- knowledge(
    df,
    tier(
      p1 ~ tidyselect::starts_with("p1"),
      p2 ~ tidyselect::starts_with("p2")
    )
  )
  s <- causalDiscoSearch$new()
  s$set_params(list(output = "discography"))
  s$set_test("fisher_z")
  s$set_knowledge(kn)
  s$set_alg("tpc")
  s$set_data(df, set_suff_stat = TRUE)
  res <- s$run_search()
  expect_s3_class(res, "discography")
})

test_that("tpc and tfci run end-to-end and return discography", {
  skip_on_cran()
  skip_if_not_installed("tidyselect")

  set.seed(1)
  df <- data.frame(
    child_x = rnorm(100),
    child_y = rnorm(100),
    adult_x = rnorm(100),
    adult_y = rnorm(100)
  )

  kn <- knowledge(
    df,
    tier(
      child ~ tidyselect::starts_with("child"),
      adult ~ tidyselect::starts_with("adult")
    )
  )

  s_tpc <- causalDiscoSearch$new()
  s_tpc$set_params(list(method = "stable.fast", methodNA = "none"))
  s_tpc$set_test("fisher_z")
  s_tpc$set_knowledge(kn)
  s_tpc$set_alg("tpc")
  s_tpc$set_data(df, set_suff_stat = TRUE)
  res_tpc <- s_tpc$run_search()
  expect_s3_class(res_tpc, "discography")

  s_tfci <- causalDiscoSearch$new()
  s_tfci$set_params(list(method = "stable.fast", methodNA = "none"))
  s_tfci$set_test("fisher_z")
  s_tfci$set_knowledge(kn)
  s_tfci$set_alg("tfci")
  s_tfci$set_data(df, set_suff_stat = TRUE)
  res_tfci <- s_tfci$run_search()
  expect_s3_class(res_tfci, "discography")
})

test_that("tges runs with TemporalBIC (Gaussian) and TemporalBDeu (categorical)", {
  skip_on_cran()

  set.seed(11)
  gdf <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(gdf) <- c("p1_A", "p1_B", "p2_C", "p2_D")

  kn_g <- knowledge(
    gdf,
    tier(
      p1 ~ tidyselect::starts_with("p1"),
      p2 ~ tidyselect::starts_with("p2")
    )
  )

  s_g <- causalDiscoSearch$new()
  s_g$set_data(gdf, set_suff_stat = FALSE)
  s_g$set_knowledge(kn_g)
  s_g$set_score("tbic")
  s_g$set_alg("tges")
  out_g <- s_g$run_search()
  expect_s3_class(out_g, "discography")

  set.seed(12)
  dfc <- data.frame(
    a = factor(sample(letters[1:3], 300, TRUE)),
    b = factor(sample(letters[1:2], 300, TRUE)),
    c = factor(sample(letters[1:2], 300, TRUE))
  )
  colnames(dfc) <- c("t1_a", "t1_b", "t2_c")

  kn_c <- knowledge(
    dfc,
    tier(
      t1 ~ tidyselect::starts_with("t1"),
      t2 ~ tidyselect::starts_with("t2")
    )
  )

  s_c <- causalDiscoSearch$new()
  s_c$set_data(dfc, set_suff_stat = FALSE)
  s_c$set_knowledge(kn_c)
  s_c$set_score("tbdeu")
  s_c$set_alg("tges")
  out_c <- s_c$run_search()
  expect_s3_class(out_c, "discography")
})

test_that("verbose is accepted via set_params and passed to tges", {
  gdf <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(gdf) <- paste0("p1_X", 1:4)

  kn <- knowledge(
    gdf,
    tier(
      p1 ~ tidyselect::starts_with("p1")
    )
  )

  s <- causalDiscoSearch$new()
  s$set_params(list(verbose = TRUE))
  s$set_data(gdf, set_suff_stat = FALSE)
  s$set_knowledge(kn)
  s$set_score("tbic")
  s$set_alg("tges")
  expect_s3_class(s$run_search(), "discography")
})

test_that("run_search errors when suff_stat missing for constraint-based algs", {
  s <- causalDiscoSearch$new()
  df <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(df) <- c("X", "Y", "Z", "W")
  s$set_data(df, set_suff_stat = FALSE)
  s$set_test("fisher_z")
  s$set_alg("tpc")
  expect_error(
    s$run_search(),
    "No sufficient statistic is set. Use set_data() first.",
    fixed = TRUE
  )
})

test_that("run_search tges errors without score and covers knowledge-NULL branch", {
  s_err <- causalDiscoSearch$new()
  gdf <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(gdf) <- paste0("X", 1:4)
  s_err$set_data(gdf, set_suff_stat = FALSE)
  s_err$set_alg("tges")
  expect_error(
    s_err$run_search(),
    "No score is set. Use set_score() first.",
    fixed = TRUE
  )

  s_ok <- causalDiscoSearch$new()
  s_ok$set_data(gdf, set_suff_stat = FALSE)
  s_ok$set_score("tbic")
  s_ok$set_alg("tges")
  out <- s_ok$run_search()
  expect_s3_class(out, "discography")
})

test_that("run_search(data=...) takes constraint-based path and computes suff_stat", {
  skip_if_not_installed("tidyselect")

  df <- data.frame(
    p1_x = rnorm(100),
    p1_y = rnorm(100),
    p2_z = rnorm(100)
  )
  kn <- knowledge(
    df,
    tier(
      p1 ~ tidyselect::starts_with("p1"),
      p2 ~ tidyselect::starts_with("p2")
    )
  )

  s <- causalDiscoSearch$new()
  s$set_test("fisher_z")
  s$set_knowledge(kn)
  s$set_alg("tpc")

  out <- s$run_search(data = df, set_suff_stat = TRUE)

  expect_false(is.null(s$suff_stat))
  expect_named(s$suff_stat, c("C", "n"))
  expect_s3_class(out, "discography")
})

test_that("run_search(data=...) takes score-based path and skips suff_stat", {
  df <- matrix(rnorm(100), ncol = 4) |> as.data.frame()
  colnames(df) <- paste0("X", 1:4)

  s <- causalDiscoSearch$new()
  s$set_score("tbic")
  s$set_alg("tges")

  out <- s$run_search(data = df, set_suff_stat = TRUE)

  expect_null(s$suff_stat)
  expect_s3_class(out, "discography")
})
