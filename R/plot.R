#' Plot temporal partially directed acyclic graph (TPDAG)
#'
#' @param x tpdag (temporal partially directed acyclic graph) object
#'  to be plotted (as outputted from \code{\link{tpc}}).
#' @param ... Further plotting arguments passed along to \code{\link{plotTempoMech}}.
#'
#' @return No return value, the function is called for its side-effects (plotting). 
#' 
#'
#' @export
plot.tpdag <- function(x, ...) {
  plotTempoMech(x, ...)
}


#' Plot temporal skeleton
#'
#' @param x tskeleton (temporal skeleton) object to be plotted
#' (as outputted from \code{\link{tpc}}).
#' @param ... Further plotting arguments passed along to \code{\link{plotTempoMech}}.
#'
#' @return No return value, the function is called for its side-effects (plotting). 
#' 
#'
#' @export
plot.tskeleton <- function(x, ...) {
  plotTempoMech(x, ...)
}

#' Plot adjacency matrix with order information
#'
#' @param x tamat (temporal adjacency matrix) object to be plotted
#' (as outputted from \code{\link{tamat}}).
#' @param ... Further plotting arguments passed along to \code{\link{plotTempoMech}}.
#'
#' @return No return value, the function is called for its side-effects (plotting). 
#' 
#'
#' @export
plot.tamat <- function(x, ...) {
  plotTempoMech(x, ...)
}




