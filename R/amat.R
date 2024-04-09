#' Extract adjacency matrix from tpdag or cpdag object
#' 
#' The resulting adjacency matrix A is "from-to", i.e. encoded as follows: 
#' A(i,j) = 1 and A(j,i) = 0 means there is an edge j -> i.
#' A(j,i) = 1 and A(i,j) = 0 means there is an edge i -> j. 
#' A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i and j, i - j. 
#' A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j. 
#' 
#' @param x \code{tpdag}/\code{cpdag} object as obtained from \link{tpc} or \link{pc}
#' 
#' @export
amat <- function(x) {
  x_class <- class(x) 
  
  if (any(c("tpdag", "tpag") %in% x_class)) {
      out <- x$tamat
  } else if (any(c("cpdag", "pag") %in% x_class)) {
    out <- x$amat
  }
  
  out
}