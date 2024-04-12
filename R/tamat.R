#' Make a temporal adjacency matrix
#'
#' @param amat Adjacency matrix.
#' Row names and column names should be identical and be the names of
#' the variables/nodes. Variable names should be prefixed with their
#' period, e.g. "child_x" for variable "x" at period "child"
#' @param order A character vector with the periods in their order.
#' @param type The type of adjancency matrix, must be one of \code{"pdag"} or 
#' \code{"ag"}. If \code{NULL} (default), the function first checks for a \code{tamat_type}
#' attribute in the input object and makes sure the output matches that, and if no 
#' the input does not have this attribute, it is set to \code{"tpdag"}. Otherwise, 
#' the user can specify a type manually as follows: \code{"pdag"} should be used for
#'  directed graphs, namely
#'  DAG, CPDAG, MPDAG, TPDAG and PDAG adjacency matrices, i.e. adjacency matrices 
#'  where A(i,j) = A(j,i) = 1 is interpreted as an undirected edge. \code{"ag"}
#'  may be used for ADMGs, MAGs, PAGs and TPAGs, where further possible arrowhead
#'  options are available (see \link{amat})
#'
#' @return A \code{tamat} object, which is a matrix with a "order"
#' attribute(a character vector listing the temporal order of the variables 
#' in the adjacency matrix). 
#'
#' @export
tamat <- function(amat, order, type = NULL) {
#  out <- list(amat = amat, order = order)
  out <- amat
  attr(out, "order") <- order
  
  if (is.null(type)) {
    if (!is.null(attr(amat, "tamat_type"))) {
      type <- attr(amat, "tamat_type")
    } else {
      type <- "pdag"
    }
  }
  
  attr(out, "tamat_type") <- type
  class(out) <- c("tamat", "matrix")
  out
}


