#' Test for Vanishing Partial Correlations
#'
#' This function simply calls the [pcalg::gaussCItest()] function from the \pkg{pcalg} package.
#'
#' @param x Index of x variable.
#' @param y Index of y variable.
#' @param conditioning_set Index vector of conditioning variable(s), possibly `NULL`.
#' @param suff_stat Sufficient statistic; list with data,
#' binary variables and order.
#'
#' @return A numeric, which is the p-value of the test.
#'
#' @export
cor_test <- function(x, y, conditioning_set, suff_stat) {
  .check_if_pkgs_are_installed(
    pkgs = c("pcalg"),
    function_name = "cor_test"
  )
  pcalg::gaussCItest(x, y, conditioning_set, suff_stat)
}


#' Regression-based Information Loss Test
#'
#' We test whether \code{x} and \code{y} are associated, given
#' \code{conditioning_set} using a generalized linear model.
#'
#' @details All included variables should be either numeric or binary. If
#' \code{y} is binary, a logistic regression model is fitted. If \code{y} is numeric,
#' a linear regression model is fitted. \code{x} and \code{conditioning_set} are included as
#' explanatory variables. Any numeric variables among \code{x} and \code{conditioning_set} are
#' modeled with spline expansions (natural splines, 3 df). This model is tested
#' against a numeric where \code{x} (including a possible spline expansion) has
#' been left out using a likelihood ratio test.
#' The model is fitted in both directions (interchanging the roles
#' of \code{x} and \code{y}). The final p-value is the maximum of the two
#' obtained p-values.
#'
#' @inheritParams cor_test
#'
#' @return A numeric, which is the p-value of the test.
#'
#' @export
reg_test <- function(x, y, conditioning_set, suff_stat) {
  p1 <- reg_test_each_dir(x, y, conditioning_set, suff_stat)
  p2 <- reg_test_each_dir(y, x, conditioning_set, suff_stat)

  max(p1, p2)
}

############################################################################
## Not exported below ######################################################
############################################################################

reg_test_each_dir <- function(x, y, S, suffStat) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "splines",
      "stats"
    ),
    function_name = "reg_test_each_dir"
  )

  dfs <- 3
  df_string <- paste(", df = ", dfs, ")", sep = "")

  # Unpack suffStat
  data <- suffStat$data
  binary <- suffStat$binary
  vnames <- names(data)

  # Restrict data to only complete cases of x, y, s (test-wise deletion)
  data <- stats::na.omit(data[, c(x, y, S)])

  # Choose exponential family according to ys type
  if (binary[y]) {
    fam <- "binomial"
  } else {
    fam <- "gaussian"
  }

  # Store info: is x binary?
  binx <- binary[x]

  # Store info: which S are binary?
  binS <- intersect(S, which(binary))
  numS <- setdiff(S, binS)

  # extract variable names
  x <- vnames[x]
  y <- vnames[y]
  S_bin <- vnames[binS]
  S_num <- vnames[numS]
  allS <- c(S_bin, S_num) # TODO: Not used

  # add spline to num x, factor to binary x
  if (!binx) {
    if (dfs > 0) x <- paste("splines::ns(", x, df_string, sep = "")
  } else {
    x <- paste("factor(", x, ")", sep = "")
  }

  # add spline to num S, factor to binary s
  if (length(S_num > 0)) {
    S_num <- paste("splines::ns(", S_num, df_string, sep = "")
  }
  if (length(S_bin > 0)) {
    S_bin <- paste("factor(", S_bin, ")", sep = "")
  }
  S <- c(S_bin, S_num, "1")

  # wrap factor around binary f
  if (fam == "binomial") {
    y <- paste("factor(", y, ")", sep = "")
  }

  # make formulas
  f1 <- stats::as.formula(paste(y, "~", paste(S, collapse = " + ")))
  f2 <- stats::update(f1, stats::as.formula(paste(". ~ . + ", x, sep = "")))

  # fit models
  m1 <- suppressWarnings(stats::glm(f1, data = data, family = fam))
  m2 <- suppressWarnings(stats::glm(f2, data = data, family = fam))

  # if convergence fails, output 0 (corresponds to no sep)
  if (!m1$converged || !m2$converged) {
    return(0)
  }

  # test
  stats::anova(m1, m2, test = "LRT")$`Pr(>Chi)`[2]
}

#' Gaussian L0 score computed on correlation matrix
#'
#' The score is intended to be used with score-based causal discovery algorithms
#' from the pcalg package. It is identical to the \code{\link[pcalg]{GaussL0penObsScore-class}},
#' except that it takes in a correlation matrix instead of the full data set.
#' \code{\link[pcalg]{GaussL0penObsScore-class}}.
#'
#'
#' @param cormat A correlation matrix. Needs to be symmetric.
#' @param n The number of observations in the dataset that the correlation matrix was computed from.
#' @param p The number of variables. This is inferred from the cormat if not supplied.
#' @param lambda Penalty to use for the score. If \code{NULL} (default), the BIC score penalty is used. See
#' \code{\link[pcalg]{GaussL0penObsScore-class}} for further details.
#' @param ... Other arguments passed along to \code{\link[pcalg]{GaussL0penObsScore-class}}.
#'
#' @return A \code{Score} object (S4), see \code{\link[pcalg]{Score-class}}.
#'
#' @importClassesFrom pcalg GaussL0penObsScore
#'
#' @examples
#' # Simulate data and compute correlation matrix
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' x3 <- x1 + x2 + rnorm(100)
#' d <- data.frame(x1, x2, x3)
#' cmat <- cor(d)
#'
#' # Use gaus_cor_score with pcalg::ges()
#' pcalg::ges(gaus_cor_score(cmat, n = 100))
#'
#' @keywords internal
#' @noRd
gaus_cor_score <- function(
  cormat,
  n,
  p = NULL,
  lambda = NULL,
  ...
) {
  if (is.null(lambda)) {
    lambda <- log(n) / 2
  }

  if (is.null(p)) {
    p <- dim(cormat)[1]
  }

  outscore <- methods::new(
    "GaussL0penObsScore",
    matrix(1, 1, 1),
    lambda = lambda,
    intercept = FALSE,
    ...
  )

  # drop entries not needed
  outscore$pp.dat$data <- NULL
  outscore$pp.dat$non.int <- NULL
  outscore$pp.dat$target.index <- NULL

  # fill in entries with custom calculations
  outscore$pp.dat$vertex.count <- p
  outscore$pp.dat$data.count <- rep(n, p)
  outscore$pp.dat$total.data.count <- n
  outscore$pp.dat$scatter.index <- rep(1, p)
  outscore$pp.dat$scatter <- list(
    cbind(rbind(cormat, rep(0, p)), c(rep(0, p), 1)) * (n - 1)
  )
  outscore$.nodes <- rownames(cormat)

  outscore
}
