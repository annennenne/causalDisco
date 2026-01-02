test_that("tetrad test argument works (discrete)", {
  skip_if_no_tetrad()
  # Discrete – categorical
  disc_test_data <- make_disc_test_data(n = 2000)
  disc_test_data[] <- lapply(disc_test_data, factor)

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
    # "probabilistic", # Currently doesn't work
    "degenerate_gaussian",
    "conditional_gaussian"
  )

  lapply(tests, function(t) run_pc_test(disc_test_data, t))
})

test_that("tetrad test argument works (continious)", {
  skip_if_no_tetrad()
  data("tpc_example")

  run_pc_test <- function(data, test) {
    pc_method <- pc(engine = "tetrad", test = test, alpha = 0.05)
    pc_result <- disco(data, method = pc_method)
    expect_equal(class(pc_result), c("knowledgeable_caugi", "knowledge"))
    pc_result
  }

  tests <- c(
    # "probabilistic", # Currently doesn't work
    "fisher_z",
    # "cci",      # Currently doesn't work
    "kci"
  )

  lapply(tests, function(t) run_pc_test(tpc_example, t))
})


test_that("tetrad score argument works", {
  skip_if_no_tetrad()
  # Continious
  data("tpc_example")
  run_ges_test <- function(data, score) {
    print(score)
    ges_method <- ges(engine = "tetrad", score = score)
    ges_result <- disco(data, method = ges_method)
    expect_equal(class(ges_result), c("knowledgeable_caugi", "knowledge"))
    ges_result
  }

  tests <- c(
    "sem_bic",
    "ebic",
    "basis_function_bic",
    "conditional_gaussian",
    "degenerate_gaussian",
    "gic",
    "mag_degenerate_gaussian_bic",
    # "mixed_variable_polynomial", currently doesn't work
    "poisson_prior",
    "zhang_shen_bound"
  )

  lapply(tests, function(t) run_ges_test(tpc_example, t))

  # Discrete
  disc_test_data <- make_disc_test_data(n = 2000)
  disc_test_data[] <- lapply(disc_test_data, factor)

  tests_disc <- c(
    "bdeu",
    "discrete_bic"
  )
  lapply(tests_disc, function(t) run_ges_test(disc_test_data, t))
})
