# ──────────────────────────────────────────────────────────────────────────────
# Initialization
# ──────────────────────────────────────────────────────────────────────────────

test_that("initialize sets clean defaults", {
  s <- BnlearnSearch$new()
  expect_null(s$data)
  expect_null(s$score)
  expect_null(s$test)
  expect_null(s$knowledge)
  expect_identical(s$params, list())
})

# ──────────────────────────────────────────────────────────────────────────────
# Setting params
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_params and set_data store values", {
  s <- BnlearnSearch$new()
  s$set_params(list(alpha = 0.1, whatev = 2))
  expect_identical(s$params, list(alpha = 0.1, whatev = 2))

  my_df <- data.frame(a = rnorm(10), b = rnorm(10))
  s$set_data(my_df)
  expect_identical(s$data, my_df)
})

# ──────────────────────────────────────────────────────────────────────────────
# Tests
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_test validates alpha, maps fisher_z -> zf, and errors on unknown", {
  s <- BnlearnSearch$new()

  # alpha validation
  expect_error(s$set_test("zf", alpha = -0.1), class = "simpleError")
  expect_error(s$set_test("zf", alpha = 1.1), class = "simpleError")
  expect_error(s$set_test("zf", alpha = c(0.05, 0.1)), class = "simpleError")
  expect_error(s$set_test("zf", alpha = "0.05"), class = "simpleError")

  # unknown test
  expect_error(
    s$set_test("not-a-test", alpha = 0.05),
    "Unknown test type using bnlearn engine: not-a-test",
    fixed = TRUE
  )

  # alias mapping
  s$set_test("fisher_z", alpha = 0.05)
  expect_identical(s$test, "zf")
  expect_identical(s$params$alpha, 0.05)
})

# ──────────────────────────────────────────────────────────────────────────────
# Scores
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_score accepts known names and rejects unknown", {
  s <- BnlearnSearch$new()

  expect_error(
    s$set_score("not-a-score"),
    "Unknown score type using bnlearn engine: not-a-score",
    fixed = TRUE
  )

  s$set_score("bic")
  expect_identical(s$score, "bic")
})

# ──────────────────────────────────────────────────────────────────────────────
# Algs and run_search()
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_alg guards fire with exact messages", {
  s <- BnlearnSearch$new()

  # args must be a list if provided
  expect_error(
    s$set_alg("pc", args = 1),
    "Arguments must be provided as a list.",
    fixed = TRUE
  )

  # need_test algorithms without test
  expect_error(
    s$set_alg("pc"),
    "No test is set. Use set_test() first.",
    fixed = TRUE
  )

  # need_score algorithms without score
  s$set_test("zf", alpha = 0.05)
  expect_error(
    s$set_alg("hc"),
    "No score is set. Use set_score() first.",
    fixed = TRUE
  )

  # need_both algorithms without both
  expect_error(
    s$set_alg("mmhc"),
    "Both test and score must be set for this algorithm.",
    fixed = TRUE
  )

  # rsmax2 requires both + maximize/restrict algs present
  s$set_score("bic")
  expect_error(
    s$set_alg("rsmax2"),
    paste0(
      "Both maximize and restrict algorithms must be set for this ",
      "algorithm."
    ),
    fixed = TRUE
  )

  # unknown method
  expect_error(
    s$set_alg("def-not-an-alg"),
    "Unknown method type using bnlearn engine: def-not-an-alg",
    fixed = TRUE
  )
})

test_that("set_alg builds partials for each family when prerequisites satisfied", {
  # constraint-based example: pc (needs test)
  s1 <- BnlearnSearch$new()
  s1$set_test("zf", alpha = 0.05)
  s1$set_alg("pc", args = list()) # also exercises args=list() path
  expect_true(is.function(s1$alg))

  # score-based example: hc (needs score)
  s2 <- BnlearnSearch$new()
  s2$set_score("bic")
  s2$set_alg("hc")
  expect_true(is.function(s2$alg))

  # pairwise learner: chow.liu (needs neither)
  s3 <- BnlearnSearch$new()
  s3$set_alg("chow.liu")
  expect_true(is.function(s3$alg))
})

test_that("run_search errors when data/alg missing with exact messages", {
  s <- BnlearnSearch$new()

  # missing both data and alg -> first error is data
  expect_error(
    s$run_search(),
    "No data is set. Use set_data() first or pass data to run_search().",
    fixed = TRUE
  )

  # with data but no alg -> alg error
  my_df <- data.frame(a = rnorm(20), b = rnorm(20))
  s$set_data(my_df)
  expect_error(
    s$run_search(),
    "No algorithm is set. Use set_alg() first.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# Knowledge
# ──────────────────────────────────────────────────────────────────────────────

test_that("run_search works and covers whitelist/blacklist branches", {
  set.seed(1405)
  my_df <- data.frame(
    X = sample(c(1, 2), 40, replace = TRUE),
    Y = sample(c(1, 2), 40, replace = TRUE),
    Z = sample(c(1, 2), 40, replace = TRUE)
  )
  df2 <- data.frame(
    X = sample(c(2, 3), 40, replace = TRUE),
    Y = sample(c(2, 3), 40, replace = TRUE),
    Z = sample(c(2, 3), 40, replace = TRUE)
  )
  # simplest learner: chow.liu (no test/score needed)
  s <- BnlearnSearch$new()
  s$set_alg("chow.liu")
  s$set_data(my_df)

  # run without knowledge first
  g1 <- s$run_search()
  expect_true(inherits(g1, "knowledgeable_caugi"))

  # whitelist only
  kn <- knowledge(
    X %-->% Y
  )
  s$set_knowledge(kn)
  g2 <- s$run_search()
  expect_true(inherits(g2, "knowledgeable_caugi"))

  # blacklist only
  kn <- knowledge(
    Y %!-->% Z
  )
  s$set_knowledge(kn)
  g3 <- s$run_search()
  expect_true(inherits(g3, "knowledgeable_caugi"))

  # both present
  kn <- knowledge(
    X %-->% Y,
    Y %!-->% Z
  )
  s$set_knowledge(kn)
  g4 <- s$run_search()
  expect_true(inherits(g4, "knowledgeable_caugi"))

  # run with df2
  g5 <- s$run_search(df2)
  expect_true(inherits(g4, "knowledgeable_caugi"))
})

test_that("set_knowledge delegates to validators (error path covered)", {
  s <- BnlearnSearch$new()
  expect_error(
    s$set_knowledge(knowledge_obj = 123),
    class = "simpleError"
  )
})
