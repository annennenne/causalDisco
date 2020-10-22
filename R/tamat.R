#' Make a temporal adjacency matrix
#'
#' @param amat Adjacency matrix. A square matrix of 0 or 1s. A
#' 1 in the (i,j)th entry means that there is an edge from j to i.
#' Row names and column names should be identical and be the names of
#' the variables/nodes. Variable names should be prefixed with their
#' period, e.g. "child_x" for variable "x" at period "child"
#' @param order A character vector with the periods in their order.
#'
#'
#' @export
tamat <- function(amat, order) {
  out <- list(amat = amat, order = order)
  class(out) <- "tamat"
  out
}
