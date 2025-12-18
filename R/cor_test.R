#' Test for vanishing partial correlations
#'
#' This function simply calls the \code{\link[pcalg]{gaussCItest}}
#' function from the \code{pcalg} package.
#'
#' @param x Index of x variable
#' @param y Index of y variable
#' @param conditioning_set Index vector of conditioning variable(s), possibly `NULL`
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
