#' Regression-based information loss test
#'
#' We test whether \code{x} and \code{y} are associated, given
#' \code{S} using a generalized linear model.
#'
#' @details All included variables should be either numeric or binary. If
#' \code{y} is binary, a logistic regression model is fitted. If \code{y} is numeric,
#' a linear regression model is fitted. \code{x} and \code{S} are included as
#' explanatory variables. Any numeric variables among \code{x} and \code{S} are
#' modeled with spline expansions (natural splines, 3 df). This model is tested
#' against a numeric where \code{x} (including a possible spline expansion) has
#' been left out using a likelihood ratio test.
#' The model is fitted in both directions (interchanging the roles
#' of \code{x} and \code{y}). The final p-value is the maximum of the two
#' obtained p-values.
#'
#' @inheritParams corTest
#'
#' @return A numeric, which is the p-value of the test.
#'
#' @export
regTest <- function(x, y, S, suffStat) {
  p1 <- regTestEachDir(x, y, S, suffStat)
  p2 <- regTestEachDir(y, x, S, suffStat)

  max(p1, p2)
}



############################################################################
## Not exported below ######################################################
############################################################################

regTestEachDir <- function(x, y, S, suffStat) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "splines", "stats"
    ),
    function_name = "regTestEachDir"
  )

  dfs <- 3
  dfString <- paste(", df = ", dfs, ")", sep = "")

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
  allS <- c(S_bin, S_num)

  # add spline to num x, factor to binary x
  if (!binx) {
    if (dfs > 0) x <- paste("splines::ns(", x, dfString, sep = "")
  } else {
    x <- paste("factor(", x, ")", sep = "")
  }

  # add spline to num S, factor to binary s
  if (length(S_num > 0)) {
    S_num <- paste("splines::ns(", S_num, dfString, sep = "")
  }
  if (length(S_bin > 0)) S_bin <- paste("factor(", S_bin, ")", sep = "")
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
  if (!m1$converged | !m2$converged) {
    return(0)
  }

  # test
  stats::anova(m1, m2, test = "LRT")$`Pr(>Chi)`[2]
}
