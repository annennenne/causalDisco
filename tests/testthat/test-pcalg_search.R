# ──────────────────────────────────────────────────────────────────────────────
# pcalgSearch
# ──────────────────────────────────────────────────────────────────────────────


# ──────────────────────────────────────────────────────────────────────────────
# Initialize
# ──────────────────────────────────────────────────────────────────────────────

test_that("initialize sets clean defaults", {
  s <- pcalgSearch$new()
  expect_null(s$data)
  expect_null(s$score)
  expect_null(s$test)
  expect_null(s$knowledge)
  expect_null(s$params)
  expect_null(s$suff_stat)
  expect_null(s$alg)
  expect_null(s$continuous)
})

# ──────────────────────────────────────────────────────────────────────────────
# set_params, set_data, set_suff_stat
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_params and set_data store values; set_data can skip suff stat", {
  s <- pcalgSearch$new()
  s$set_params(list(alpha = 0.05, m.max = 2L))
  expect_identical(s$params, list(alpha = 0.05, m.max = 2L))

  df <- matrix(rnorm(12), ncol = 3) |> as.data.frame()
  colnames(df) <- c("X", "Y", "Z")

  # skipping suff stat path
  s$set_data(df, set_suff_stat = FALSE)
  expect_identical(s$data, df)
  expect_null(s$suff_stat)
})

test_that("set_suff_stat guards and branches", {
  skip_if_not_installed("pcalg")

  s <- pcalgSearch$new()

  # error: no data
  expect_error(
    s$set_suff_stat(),
    "Data must be set before sufficient statistic.",
    fixed = TRUE
  )

  # error: no test
  s$data <- data.frame(X = rnorm(5), Y = rnorm(5))
  expect_error(
    s$set_suff_stat(),
    "Test must be set before sufficient statistic.",
    fixed = TRUE
  )

  # error: continuous flag unknown
  s$set_test("fisher_z", alpha = 0.05)
  s$continuous <- NULL
  expect_error(
    s$set_suff_stat(),
    "The pcalgSearch class does not have knowledge on whether the
             sufficient statistic is for a continuous or discrete test.
             Please set test using set_test() or set continuous directly
             by self$continuous <- TRUE/FALSE.",
    fixed = TRUE
  )

  # continuous = TRUE with good data
  s$continuous <- TRUE
  expect_silent(s$set_suff_stat())
  expect_true(is.list(s$suff_stat))
  expect_named(s$suff_stat, c("C", "n"))

  # continuous = TRUE but bad data type
  s$data <- c(1, 2, 3) # not matrix/data.frame
  expect_error(
    s$set_suff_stat(),
    "Data must be a matrix or data frame if numeric.",
    fixed = TRUE
  )

  # discrete branch (FALSE) with data.frame
  s$continuous <- FALSE
  s$data <- data.frame(A = sample(letters[1:2], 10, TRUE), B = sample(letters[1:2], 10, TRUE))
  expect_silent(s$set_suff_stat())
  expect_named(s$suff_stat, c("dm", "adaptDF"))
})

test_that("set_suff_stat errors on unrecognized data format", {
  skip_if_not_installed("pcalg")

  s <- pcalgSearch$new()
  s$set_params(list(alpha = 0.05))
  s$set_test("g_square") # sets continuous = FALSE
  s$data <- matrix(1L, nrow = 5, ncol = 2) # not a data.frame when discrete

  expect_error(
    s$set_suff_stat(),
    "Unrecognized data format. The data should be either continouos or discrete, and the data should be in a data.frame.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# set_test()
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_test requires alpha for both tests, sets flags, unknown test errors", {
  s <- pcalgSearch$new()

  # fisher_z requires alpha
  expect_error(
    s$set_test("fisher_z"),
    "Alpha must be set before test.",
    fixed = TRUE
  )
  s$set_params(list())
  s$set_test("fisher_z", alpha = 0.01)
  expect_identical(s$test, pcalg::gaussCItest)
  expect_true(s$continuous)

  # g_square requires alpha and sets continuous = FALSE
  s <- pcalgSearch$new()
  expect_error(
    s$set_test("g_square"),
    "Alpha must be set before test.",
    fixed = TRUE
  )
  s$set_params(list())
  s$set_test("g_square", alpha = 0.05)
  expect_false(s$continuous)

  # unknown test
  expect_error(
    s$set_test("not-a-test", alpha = 0.05),
    "Unknown test type using pcalg engine: not-a-test",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# use_g_square private function
# ──────────────────────────────────────────────────────────────────────────────

test_that("private use_g_square picks binCItest/disCItest and low-unique error", {
  skip_if_not_installed("pcalg")

  set.seed(1)

  # less than 2 uniques -> error
  s0 <- pcalgSearch$new()
  s0$set_params(list(alpha = 0.05))
  s0$set_test("g_square")
  s0$data <- data.frame(X = rep(1L, 20), Y = rep(1L, 20)) # one unique value only
  s0$set_suff_stat()
  gfun0 <- s0$test
  expect_error(
    gfun0(1, 2, integer(), s0$suff_stat),
    "The data contains less than 2 unique values. If this is the case, there is nothing to discover.",
    fixed = TRUE
  )

  # exactly 2 uniques -> binCItest path
  s2 <- pcalgSearch$new()
  s2$set_params(list(alpha = 0.05))
  s2$set_test("g_square")
  s2$data <- data.frame(X = sample(0:1, 40, TRUE), Y = sample(0:1, 40, TRUE), Z = sample(0:1, 40, TRUE))
  s2$set_suff_stat()
  gfun2 <- s2$test
  expect_silent(gfun2(1, 2, integer(), s2$suff_stat))

  # more than 2 uniques -> disCItest path
  s3 <- pcalgSearch$new()
  s3$set_params(list(alpha = 0.05))
  s3$set_test("g_square")
  s3$data <- data.frame(X = sample(0:2, 40, TRUE), Y = sample(0:2, 40, TRUE), Z = sample(0:2, 40, TRUE))
  s3$set_suff_stat()
  gfun3 <- s3$test
  expect_silent(gfun3(1, 2, integer(), s3$suff_stat))
})

# ──────────────────────────────────────────────────────────────────────────────
# set_score
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_score builds scorer for obs/int and errors on unknown", {
  skip_if_not_installed("pcalg")

  # obs score
  s1 <- pcalgSearch$new()
  s1$set_score("sem_bic")
  # scorer errors if data missing when invoked through run
  expect_error(
    s1$run_search(data = NULL, set_suff_stat = TRUE),
    "No data is set. Use set_data() first or input data directly into run_search().",
    fixed = TRUE
  )

  # int score
  s2 <- pcalgSearch$new()
  s2$set_score("sem_bic_int")
  expect_error(
    s2$run_search(data = NULL, set_suff_stat = TRUE),
    "No data is set. Use set_data() first or input data directly into run_search().",
    fixed = TRUE
  )

  # unknown
  s3 <- pcalgSearch$new()
  expect_error(
    s3$set_score("not-a-score"),
    "Unknown score type using pcalg engine: not-a-score",
    fixed = TRUE
  )
})

test_that("set_score() lazy builder errors if data missing", {
  skip_if_not_installed("pcalg")

  s <- pcalgSearch$new()
  s$set_score("sem_bic") # stores closure only

  # call the stored builder directly to hit the error site
  expect_error(
    s$.__enclos_env__$private$score_function(),
    "Data must be set before score.",
    fixed = TRUE
  )
})

test_that("GaussL0penIntScore is constructed when data present", {
  skip_if_not_installed("pcalg")

  df <- data.frame(
    A = as.integer(sample(0:3, 20, TRUE)),
    B = as.integer(sample(0:3, 20, TRUE))
  )
  s <- pcalgSearch$new()
  s$set_data(df, set_suff_stat = FALSE)
  s$set_score("sem_bic_int")

  sc <- s$.__enclos_env__$private$score_function()
  expect_true(methods::is(sc, "GaussL0penIntScore"))
})

# ──────────────────────────────────────────────────────────────────────────────
# set_alg()
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_alg builds partials and errors on unknown/guard", {
  skip_if_not_installed("pcalg")

  s <- pcalgSearch$new()

  # pc and fci requires test
  expect_error(
    s$set_alg("pc"),
    "No test is set. Use set_test() first.",
    fixed = TRUE
  )
  expect_error(
    s$set_alg("fci"),
    "No test is set. Use set_test() first.",
    fixed = TRUE
  )

  s$set_params(list(alpha = 0.05))
  s$set_test("fisher_z")
  s$set_alg("pc")
  expect_true(is.function(s$alg))

  # fci builds partial even if no test set (it will be passed as NULL)
  s2 <- pcalgSearch$new()
  s2$set_params(list(alpha = 0.05))
  s2$set_test("fisher_z")
  s2$set_alg("fci")
  expect_true(is.function(s2$alg))

  # ges builds partial; score is added in run_search
  s3 <- pcalgSearch$new()
  s3$set_alg("ges")
  expect_true(is.function(s3$alg))

  # unknown
  expect_error(
    s$set_alg("nope"),
    "Unknown method type using pcalg engine: nope",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# set_knowledge()
# ──────────────────────────────────────────────────────────────────────────────

test_that("set_knowledge defers building constraints and validates input", {
  # error path from check_knowledge_obj propagated
  s_bad <- pcalgSearch$new()
  expect_error(
    s_bad$set_knowledge(knowledge_obj = 123),
    class = "simpleError"
  )
  skip_if_not_installed("pcalg")

  df <- data.frame(A = rnorm(20), B = rnorm(20), C = rnorm(20))
  s <- pcalgSearch$new()
  kn <- knowledge(
    df,
    required(A ~ B),
    forbidden(B ~ C)
  )
  s$set_knowledge(kn, directed_as_undirected = TRUE)
  # knowledge_function is deferred; becomes a concrete list during run_search()
  s$set_params(list(alpha = 0.05))
  s$set_test("fisher_z")
  s$set_alg("pc")
  expect_s3_class(s$run_search(df), "discography")
})

test_that("knowledge builder errors if data missing", {
  # exercise the internal 'Data must be set before knowledge.' stop site
  s <- pcalgSearch$new()
  df <- data.frame(X = rnorm(5), Y = rnorm(5))
  kn <- knowledge(df, required(X ~ Y))

  s$set_knowledge(kn)
  expect_error(
    s$.__enclos_env__$private$knowledge_function(),
    "Data must be set before knowledge.",
    fixed = TRUE
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# run_search()
# ──────────────────────────────────────────────────────────────────────────────

test_that("run_search errors in correct order and messages", {
  s <- pcalgSearch$new()

  expect_error(
    s$run_search(),
    "No data is set. Use set_data() first or input data directly into run_search().",
    fixed = TRUE
  )

  df <- matrix(rnorm(30), ncol = 3) |> as.data.frame()
  colnames(df) <- c("X", "Y", "Z")
  s$set_data(df, set_suff_stat = FALSE)

  expect_error(
    s$run_search(),
    "No algorithm is set. Use set_alg() first.",
    fixed = TRUE
  )

  s$set_params(list(alpha = 0.05))
  s$set_test("fisher_z")
  s$set_alg("pc")
  expect_error(
    s$run_search(),
    "No sufficient statistic is set. Use set_data() first.",
    fixed = TRUE
  )
})

test_that("run_search without score_function (pc) works; with score_function (ges) warns on fixedEdges", {
  skip_if_not_installed("pcalg")

  set.seed(11)
  df <- matrix(rnorm(100), ncol = 5) |> as.data.frame()
  colnames(df) <- LETTERS[1:5]

  # no score_function path: PC
  s_pc <- pcalgSearch$new()
  s_pc$set_params(list(alpha = 0.05, m.max = 1L))
  s_pc$set_test("fisher_z")
  s_pc$set_alg("pc")
  res_pc <- s_pc$run_search(df)
  expect_s3_class(res_pc, "discography")

  # score_function path: GES without knowledge
  s_ges <- pcalgSearch$new()
  s_ges$set_alg("ges")
  s_ges$set_score("sem_bic")
  res_ges <- s_ges$run_search(df)
  expect_s3_class(res_ges, "discography")

  # score_function path + knowledge with fixedEdges -> explicit warning
  kn_req <- knowledge(df, required(A ~ B)) # required edge
  s_ges2 <- pcalgSearch$new()
  s_ges2$set_alg("ges")
  s_ges2$set_score("sem_bic")
  s_ges2$set_knowledge(kn_req, directed_as_undirected = TRUE)
  expect_warning(
    s_ges2$run_search(df),
    "pcalg::ges() does not take required edges as arguments.\n  They will not be used here.",
    fixed = TRUE
  )
})
