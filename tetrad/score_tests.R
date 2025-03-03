library(testthat)
# Source tetrad <-> R data conversion functions.
source("/home/fabben/BioStat/causalDisco/tetrad/tetrad_rdata.R")

# Source R <-> Java helper functions.
source("/home/fabben/BioStat/causalDisco/tetrad/java_r_functions.R")

# Source tetrad R6 class
source("/home/fabben/BioStat/causalDisco/tetrad/my_tetrad_search_r6_class.R")


# print all scores
jar_path <- "/home/fabben/BioStat/py-tetrad/pytetrad/resources/tetrad-current.jar"

jar_command <- sprintf("jar tf %s | grep edu/cmu/tetrad/algcomparison/score", jar_path)
score_classes <- system(jar_command, intern = TRUE)
print(score_classes)

# TO DO: implement following scores? 
# SemBicScoreDeterministic
# MagDgBicScore
# DiscreteBicScore
# PositiveCorrScore
# FisherZScore


# Create a dummy data frame for testing.
dummy_df <- data.frame(a = c(1.1, 2.2, 3.3),
                       b = c(4.4, 5.5, 6.6),
                       stringsAsFactors = FALSE)

# Instantiate a TetradSearch object.
ts_obj <- TetradSearch$new(dummy_df)

# Define a helper to get the Java class name of a score.
get_score_class <- function(score_obj) {
  return(score_obj$getClass()$getName())
}

test_that("use_sem_bic sets correct score", {
  ts_obj$use_sem_bic(penalty_discount = 2, structure_prior = 0, sem_bic_rule = 1)
  expect_true(!is.null(ts_obj$score))
  # Expect the underlying class to be SemBicScore.
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.SemBicScore")
})

test_that("use_gic_score sets correct score", {
  ts_obj$use_gic_score(penalty_discount = 1, sem_gic_rule = 4)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.GicScores")
})

test_that("use_mixed_variable_polynomial sets correct score", {
  ts_obj$use_mixed_variable_polynomial(structure_prior = 0, f_degree = 2, discretize = TRUE)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.MVPBicScore")
})

test_that("use_poisson_prior_score sets correct score", {
  ts_obj$use_poisson_prior_score(lambda_ = 2, precompute_covariances = TRUE)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.PoissonPriorScore")
})

test_that("use_zhang_shen_bound sets correct score", {
  ts_obj$use_zhang_shen_bound(risk_bound = 0.2)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.ZhangShenBoundScore")
})

test_that("use_bdeu sets correct score", {
  ts_obj$use_bdeu(sample_prior = 10, structure_prior = 0)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.BdeuScore")
})

test_that("use_conditional_gaussian_score sets correct score", {
  ts_obj$use_conditional_gaussian_score(penalty_discount = 1, discretize = TRUE, num_categories_to_discretize = 3, structure_prior = 0)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.ConditionalGaussianBicScore")
})

test_that("use_degenerate_gaussian_score sets correct score", {
  ts_obj$use_degenerate_gaussian_score(penalty_discount = 1, structure_prior = 0)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.DegenerateGaussianBicScore")
})

test_that("use_basis_function_bic sets correct score", {
  ts_obj$use_basis_function_bic(truncation_limit = 3, penalty_discount = 2)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.BasisFunctionBicScore")
})

test_that("use_basis_function_bic_fs sets correct score", {
  ts_obj$use_basis_function_bic_fs(truncation_limit = 3, penalty_discount = 2)
  expect_true(!is.null(ts_obj$score))
  expect_equal(get_score_class(ts_obj$score),
               "edu.cmu.tetrad.algcomparison.score.BasisFunctionBicScoreFullSample")
})
