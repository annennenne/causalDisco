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

#' Convert graphNEL object to adjacency matrix
#'
#' @param graph A graphNEL object.
#' @param to_from Logical indicating whether the resulting adjacency
#' matrix is "to-from" (default), or "from-to", see details.
#' @param type  The type of adjacency matrix, must be one of \code{"pdag"} or
#' \code{"ag"}. \code{"pdag"} should be used for directed graphs, namely
#'  DAG, CPDAG, MPDAG, TPDAG and PDAG adjacency matrices, i.e. adjacency matrices
#'  where A(i,j) = A(j,i) = 1 is interpreted as an undirected edge. \code{"ag"}
#'  may be used for ADMGs, MAGs, PAGs and TPAGs, where further possible arrowhead
#'  options are available (see [amat]).
#'
#' @details
#' A "to-from" \code{pdag} adjacency matrix is encoded as follows: A(i,j) = 1 and A(j,i) = 0
#' means there is an edge i -> j. A(j,i) = 1 and A(i,j) = 0 means there is an edge j -> i.
#' A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i and j, i - j.
#' A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j.
#'
#' A "from-to" adjacency matrix is the transpose of a "to-from" adjacency matrix.
#' A "from-to" \code{pdag} adjacency matrix is hence encoded as follows: A(i,j) = 1 and A(j,i) = 0
#' means there is an edge j -> i. A(j,i) = 1 and A(i,j) = 0 means there is an edge i -> j.
#' A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i and j, i - j.
#' A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j.
#'
#' See [amat] for details about how an \code{ag} adjacency matrix is encoded.
#'
#' @keywords internal
#' @noRd
graph_to_amat <- function(graph, to_from = TRUE, type = "pdag") {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "methods"
    ),
    function_name = "graph_to_amat"
  )

  res <- methods::as(graph, "matrix")
  if (to_from) {
    res <- t(res)
  }
  attr(res, "tamat_type") <- type
  res
}
