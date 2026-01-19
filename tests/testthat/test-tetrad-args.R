test_that("tetrad test argument works (discrete)", {
  skip_if_no_tetrad()
  data(cat_data)

  run_pc_test <- function(data, test) {
    pc_method <- pc(engine = "tetrad", test = test, alpha = 0.05)
    pc_result <- disco(data, method = pc_method)
    expect_equal(class(pc_result), c("knowledgeable_caugi", "knowledge"))
    pc_result
  }

  tests <- c(
    "chi_square",
    "g_square",
    "basis_function_lrt",
    "probabilistic"
  )

  lapply(tests, function(t) run_pc_test(cat_data, t))
})

test_that("tetrad test argument works (continuous)", {
  skip_if_no_tetrad()
  data(num_data)

  run_pc_test <- function(data, test) {
    pc_method <- pc(engine = "tetrad", test = test, alpha = 0.05)
    pc_result <- disco(data, method = pc_method)
    expect_equal(class(pc_result), c("knowledgeable_caugi", "knowledge"))
    pc_result
  }

  tests <- c(
    "fisher_z",
    "poisson_prior"
  )

  lapply(tests, function(t) run_pc_test(num_data, t))
})

test_that("tetrad test argument works (mixed)", {
  skip_if_no_tetrad()
  data(mix_data)

  run_pc_test <- function(data, test) {
    pc_method <- pc(engine = "tetrad", test = test, alpha = 0.05)
    pc_result <- disco(data, method = pc_method)
    expect_equal(class(pc_result), c("knowledgeable_caugi", "knowledge"))
    pc_result
  }

  tests <- c(
    "degenerate_gaussian",
    "conditional_gaussian",
    "kci"
  )

  lapply(tests, function(t) run_pc_test(mix_data, t))
})


test_that("tetrad score argument works (continuous)", {
  skip_if_no_tetrad()

  data(num_data)
  run_ges_test <- function(data, score) {
    ges_method <- ges(engine = "tetrad", score = score)
    ges_result <- disco(data, method = ges_method)
    expect_equal(class(ges_result), c("knowledgeable_caugi", "knowledge"))
    ges_result
  }

  tests <- c(
    "sem_bic",
    "ebic",
    "gic",
    "poisson_prior",
    "zhang_shen_bound",
    "rank_bic"
  )

  lapply(tests, function(t) run_ges_test(num_data, t))
})


test_that("tetrad score argument works (discrete)", {
  skip_if_no_tetrad()

  data(cat_data)
  run_ges_test <- function(data, score) {
    ges_method <- ges(engine = "tetrad", score = score)
    ges_result <- disco(data, method = ges_method)
    expect_equal(class(ges_result), c("knowledgeable_caugi", "knowledge"))
    ges_result
  }

  tests <- c(
    "bdeu",
    "discrete_bic"
  )

  lapply(tests, function(t) run_ges_test(cat_data, t))
})

test_that("tetrad score argument works (mixed)", {
  skip_if_no_tetrad()

  data(mix_data)
  run_ges_test <- function(data, score) {
    ges_method <- ges(engine = "tetrad", score = score)
    ges_result <- disco(data, method = ges_method)
    expect_equal(class(ges_result), c("knowledgeable_caugi", "knowledge"))
    ges_result
  }

  tests <- c(
    "conditional_gaussian",
    "degenerate_gaussian",
    "basis_function_bic",
    "mag_degenerate_gaussian_bic",
    "basis_function_blocks_bic",
    "basis_function_sem_bic"
  )

  lapply(tests, function(t) run_ges_test(mix_data, t))
})
