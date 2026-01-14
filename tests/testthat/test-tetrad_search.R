# ──────────────────────────────────────────────────────────────────────────────
# Tetrad test setup and utility functions
# ──────────────────────────────────────────────────────────────────────────────

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

test_that("discrete generator returns integer-coded X1,...,X5 in 0,...,k-1", {
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
  expect_true(all(
    c("tiered_kn", "forbidden_kn", "required_kn", "combi_kn") %in% names(kn)
  ))

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

  jars <- find_tetrad_jar()
  expect_true(length(jars) > 0)
  expect_true(all(file.exists(jars)))
})


# ──────────────────────────────────────────────────────────────────────────────
# TetradSearch constructor sets defaults correctly
# ──────────────────────────────────────────────────────────────────────────────

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


# ──────────────────────────────────────────────────────────────────────────────
# Setters
# ──────────────────────────────────────────────────────────────────────────────

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

# ──────────────────────────────────────────────────────────────────────────────
# Scores, tests, and algorithms
# ──────────────────────────────────────────────────────────────────────────────

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
    # "mixed_variable_polynomial",
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
    "conditional_gaussian",
    "kci"
  )

  # plain set
  purrr::walk(tests, \(tst) {
    ts <- TetradSearch$new()
    expect_no_condition(ts$set_test(tst))
  })

  # one explicit mc=TRUE path to cover mc_test
  purrr::walk(tests, \(tst) {
    ts <- TetradSearch$new()
    expect_no_condition(ts$set_test(tst, mc = TRUE))
  })
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

test_that("set_alg() succeeds for score-only algorithms when a score is set and fails if not", {
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
  purrr::walk(score_only, \(alg) {
    ts <- TetradSearch$new()
    expect_error(ts$set_alg(alg))
  })
})

test_that("set_alg() succeeds for test-only algorithms when a test is set and fails if not", {
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
  purrr::walk(test_only, \(alg) {
    ts <- TetradSearch$new()
    expect_error(ts$set_alg(alg))
  })
})

test_that("set_alg() succeeds for algorithms that require both score and test and fails if both are not provided", {
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
  purrr::walk(both_required, \(alg) {
    ts <- TetradSearch$new()
    ts$set_test("fisher_z")
    expect_error(ts$set_alg(alg))
  })
  purrr::walk(both_required, \(alg) {
    ts <- TetradSearch$new()
    ts$set_score("sem_bic")
    expect_error(ts$set_alg(alg))
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

  # no data
  ts <- TetradSearch$new()
  ts$set_score("sem_bic")
  ts$set_test("fisher_z")
  expect_error(ts$set_alg("svar_fci"))
  expect_error(ts$set_alg("svar_gfci"))
})

test_that("set_alg() warns when background knowledge is set for algorithms that do not use it", {
  skip_if_no_tetrad()

  no_background_algorithms <- c(
    "restricted_boss",
    "cstar",
    "ica_lingam",
    "ica_lingd",
    "fofc",
    "ccd",
    "direct_lingam",
    "dagma",
    "svar_gfci"
  )
  df <- make_cont_test_data()
  purrr::walk(no_background_algorithms, \(alg) {
    ts <- TetradSearch$new()
    kn_list <- make_knowledge_test_object(df)
    ts$set_knowledge(kn_list$combi_kn)
    ts$set_score("sem_bic")
    ts$set_test("fisher_z")
    ts$set_data(df)
    expect_warning(ts$set_alg(alg))
  })
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

# ──────────────────────────────────────────────────────────────────────────────
# Search
# ──────────────────────────────────────────────────────────────────────────────

test_that("run_search() errors when pieces are missing", {
  skip_if_no_tetrad()

  # no data
  ts <- TetradSearch$new()
  ts$set_score("sem_bic")
  ts$set_alg("fges")
  expect_error(ts$run_search(), "No data is set", fixed = TRUE)

  # data but no algorithm
  ts2 <- TetradSearch$new()
  ts2$set_data(make_cont_test_data(n = 150))
  expect_error(ts2$run_search(), "No algorithm is set", fixed = TRUE)
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
    number_resampling = 5L,
    percent_resample_size = 50,
    add_original = TRUE,
    with_replacement = TRUE,
    resampling_ensemble = 1L,
    seed = 1L
  )

  res <- ts$run_search(
    bootstrap = TRUE
  )

  # main return type
  expect_s3_class(res, "knowledgeable_caugi")

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

test_that("run_search(df) works instead of using set_data(df)", {
  skip_if_no_tetrad()

  df <- make_cont_test_data(n = 150)

  ts <- TetradSearch$new()
  ts$set_score("sem_bic")
  ts$set_alg("fges")

  res <- ts$run_search(df)

  # main return type
  expect_s3_class(res, "knowledgeable_caugi")

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

test_that("getters work after a run", {
  skip_if_no_tetrad()

  df <- make_cont_test_data(n = 150)

  ts <- TetradSearch$new()
  ts$set_data(df)
  ts$set_score("sem_bic")
  ts$set_alg("fges")

  res <- ts$run_search()
  expect_s3_class(res, "knowledgeable_caugi")

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
