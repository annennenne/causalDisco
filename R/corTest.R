#' Test for vanishing partial correlations
#'
#' @param x Index of x variable
#' @param y Index of y variable
#' @param S Index of S variable(s), possibly NULL
#' @param suffStat Sufficient statistic; list with data,
#' binary variables and order.
#'
#' @importFrom pcalg gaussCItest
#'
corTest <- function(x, y, S, suffStat) {
  gaussCItest(x, y, S, suffStat)
}
