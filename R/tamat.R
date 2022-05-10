#' Make a temporal adjacency matrix
#'
#' @param amat Adjacency matrix. A square matrix of 0 or 1s. A
#' 1 in the (i,j)th entry means that there is an edge from j to i.
#' Row names and column names should be identical and be the names of
#' the variables/nodes. Variable names should be prefixed with their
#' period, e.g. "child_x" for variable "x" at period "child"
#' @param order A character vector with the periods in their order.
#'
#' @return A \code{tamat} object, which is a matrix with a "order"
#' attribute(a character vector listing the temporal order of the variables 
#' in the adjacency matrix). 
#'
#' @export
tamat <- function(amat, order) {
#  out <- list(amat = amat, order = order)
  out <- amat
  attr(out, "order") <- order
  class(out) <- c("tamat")
  out
}


