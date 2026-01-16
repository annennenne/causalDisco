#' Test for vanishing partial correlations
#' 
#' This function simply calls the \code{\link[pcalg]{gaussCItest}}
#' function from the \code{pcalg} package. 
#'
#' @param x Index of x variable
#' @param y Index of y variable
#' @param S Index of S variable(s), possibly NULL
#' @param suffStat Sufficient statistic; list with data,
#' binary variables and order.
#' 
#' @return A numeric, which is the p-value of the test. 
#'
#' @importFrom pcalg gaussCItest
#'
#' @export
corTest <- function(x, y, S, suffStat) {
  gaussCItest(x, y, S, suffStat)
}
