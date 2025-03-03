library(testthat)
library(rJava)

library(testthat)
# Source tetrad <-> R data conversion functions.
source("/home/fabben/BioStat/causalDisco/tetrad/tetrad_rdata.R")

# Source R <-> Java helper functions.
source("/home/fabben/BioStat/causalDisco/tetrad/java_r_functions.R")

# Source tetrad R6 class
source("/home/fabben/BioStat/causalDisco/tetrad/my_tetrad_search_r6_class.R")

# print all scores
jar_path <- "/home/fabben/BioStat/py-tetrad/pytetrad/resources/tetrad-current.jar"

jar_command <- sprintf("jar tf %s | grep edu/cmu/tetrad/algcomparison/independence", jar_path)
score_classes <- system(jar_command, intern = TRUE)
print(score_classes)

# TO DO:
# find missing tests

# Helper: Get fully qualified class name of a Java object.
get_class <- function(obj) {
  obj$getClass()$getName()
}


# --- Create a dummy data frame for tests ---
dummy_df <- data.frame(
  a = c(1.1, 2.2, 3.3),
  b = c(4.4, 5.5, 6.6),
  stringsAsFactors = FALSE
)

# --- Instantiate the tetrad_search object (assumed to follow snake_case) ---
ts_obj <- TetradSearch$new(dummy_df)

# --- Test suite for the independence test functions ---
test_that("use_basis_function_lrt sets correct test", {
  ts_obj$use_basis_function_lrt(truncation_limit = 3, alpha = 0.01, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.BasisFunctionLrt")
  
  ts_obj$use_basis_function_lrt(truncation_limit = 3, alpha = 0.01, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.BasisFunctionLrt")
})

test_that("use_basis_function_lrt_fs sets correct test", {
  ts_obj$use_basis_function_lrt_fs(truncation_limit = 3, alpha = 0.01, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.BasisFunctionLrtFullSample")
  
  ts_obj$use_basis_function_lrt_fs(truncation_limit = 3, alpha = 0.01, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.BasisFunctionLrtFullSample")
})

test_that("use_fisher_z sets correct test", {
  ts_obj$use_fisher_z(alpha = 0.01, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.FisherZ")
  
  ts_obj$use_fisher_z(alpha = 0.01, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.FisherZ")
})

test_that("use_chi_square sets correct test", {
  ts_obj$use_chi_square(min_count = 1, alpha = 0.01, cell_table_type = 1, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.ChiSquare")
  
  ts_obj$use_chi_square(min_count = 1, alpha = 0.01, cell_table_type = 1, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.ChiSquare")  # Note: ensure package separator is "."
})

test_that("use_g_square sets correct test", {
  ts_obj$use_g_square(min_count = 1, alpha = 0.01, cell_table_type = 1, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.GSquare")
  
  ts_obj$use_g_square(min_count = 1, alpha = 0.01, cell_table_type = 1, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.GSquare")
})

test_that("use_conditional_gaussian_test sets correct test", {
  ts_obj$use_conditional_gaussian_test(alpha = 0.01, discretize = TRUE,
                                       num_categories_to_discretize = 3, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.ConditionalGaussianLRT")
  
  ts_obj$use_conditional_gaussian_test(alpha = 0.01, discretize = TRUE,
                                       num_categories_to_discretize = 3, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.ConditionalGaussianLRT")
})

test_that("use_degenerate_gaussian_test sets correct test", {
  ts_obj$use_degenerate_gaussian_test(alpha = 0.01, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.DegenerateGaussianLrt")
  
  ts_obj$use_degenerate_gaussian_test(alpha = 0.01, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.DegenerateGaussianLrt")
})

test_that("use_probabilistic_test sets correct test", {
  ts_obj$use_probabilistic_test(threshold = FALSE, cutoff = 0.5, prior_ess = 10, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.ProbabilisticTest")
  
  ts_obj$use_probabilistic_test(threshold = FALSE, cutoff = 0.5, prior_ess = 10, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.ProbabilisticTest")
})

test_that("use_kci sets correct test", {
  ts_obj$use_kci(alpha = 0.01, approximate = TRUE, scalingfact_or = 1, num_bootstraps = 5000,
                 threshold = 1e-3, epsilon = 1e-3, kernel_type = 1, polyd = 5, polyc = 1, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.Kci")
  
  ts_obj$use_kci(alpha = 0.01, approximate = TRUE, scalingfact_or = 1, num_bootstraps = 5000,
                 threshold = 1e-3, epsilon = 1e-3, kernel_type = 1, polyd = 5, polyc = 1, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.Kci")
})

test_that("use_cci sets correct test", {
  ts_obj$use_cci(alpha = 0.01, scalingfact_or = 2, num_basis_functions = 3, basis_type = 4,
                 basis_scale = 0.0, use_for_mc = FALSE)
  expect_true(!is.null(ts_obj$test))
  expect_equal(get_class(ts_obj$test),
               "edu.cmu.tetrad.algcomparison.independence.CciTest")
  
  ts_obj$use_cci(alpha = 0.01, scalingfact_or = 2, num_basis_functions = 3, basis_type = 4,
                 basis_scale = 0.0, use_for_mc = TRUE)
  expect_true(!is.null(ts_obj$mc_test))
  expect_equal(get_class(ts_obj$mc_test),
               "edu.cmu.tetrad.algcomparison.independence.CciTest")
})
