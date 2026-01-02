test_that("bnlearn test argument works (discrete)", {
  # Discrete – categorical
  disc_test_data <- make_disc_test_data(n = 2000)
  disc_test_data[] <- lapply(disc_test_data, factor)

  run_pc_test <- function(data, test) {
    pc_method <- pc(engine = "bnlearn", test = test, alpha = 0.05)
    pc_result <- disco(data, method = pc_method)
    expect_equal(class(pc_result), c("knowledgeable_caugi", "knowledge"))
    return(pc_result)
  }

  tests <- c(
    "mi",
    "mi-adf",
    "mc-mi",
    "smc-mi",
    "sp-mi",
    "mi-sh",
    "x2",
    "x2-adf",
    "mc-x2",
    "smc-x2",
    "sp-x2"
  )

  lapply(tests, function(t) run_pc_test(disc_test_data, t))

  # Discrete - ordered factors
  disc_test_data_ord <- make_disc_test_data(n = 2000)
  disc_test_data_ord[] <- lapply(disc_test_data_ord, function(x) as.ordered(x))

  tests_ord <- c("jt", "mc-jt", "smc-jt")

  lapply(tests_ord, function(t) run_pc_test(disc_test_data_ord, t))
})

test_that("bnlearn test argument works (Gaussian)", {
  data("tpc_example")
  run_pc_test_gauss <- function(data, test) {
    pc_method <- pc(engine = "bnlearn", test = test, alpha = 0.05)
    pc_result <- disco(data, method = pc_method)
    expect_equal(class(pc_result), c("knowledgeable_caugi", "knowledge"))
    return(pc_result)
  }

  tests_gauss <- c(
    "cor",
    "mc-cor",
    "smc-cor",
    "fisher_z",
    "mc-zf",
    "smc-zf",
    "mi-g",
    "mc-mi-g",
    "smc-mi-g",
    "mi-g-sh"
  )

  expect_warning(lapply(tests_gauss, function(t) {
    run_pc_test_gauss(tpc_example, t)
  }))

  # Dataset with mixture of discrete and continuous variables
  mix_data <- tpc_example
  mix_data$X1 <- as.factor(sample(0:2, nrow(mix_data), replace = TRUE))

  tests_mix <- c(
    "mi-cg"
  )

  lapply(tests_mix, function(t) run_pc_test_gauss(mix_data, t))
})

test_that("bnlearn score argument works", {
  skip("Currently no bnlearn score algorithms are implemented")
})
