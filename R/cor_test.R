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
#' @export
cor_test <- function(x, y, S, suffStat) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "cor_test"
  )
  pcalg::gaussCItest(x, y, S, suffStat)
}
