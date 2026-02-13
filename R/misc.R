###############################################################################################################
# Miscellaneous functions #####################################################################################
###############################################################################################################

#' Convert adjacency matrix to graphNEL object
#'
#' @param amat An adjacency matrix
#'
#' @return A \code{graphNEL} object, see  [graph::graphNEL-class].
#'
#' @keywords internal
#' @noRd
as.graphNEL <- function(amat) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "methods"
    ),
    function_name = "as.graphNEL"
  )

  this_class <- class(amat)
  if ("tamat" %in% this_class) {
    class(amat) <- "matrix"
  }
  methods::as(t(amat), "graphNEL")
}
