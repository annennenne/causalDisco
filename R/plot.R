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

#' Plot partial ancestral graph (PAG)
#' 
#' @author This code is a modification of the fciAlgo plotting method implemented 
#' in the pcalg package. 
#' 
#' @param x pag object
#'  to be plotted (as outputted from \code{\link{fci}}).
#' @param ... Currently not in use. 
#'
#' @return No return value, the function is called for its side-effects (plotting). 
#' 
#' @examples
#' # simulate linear Gaussian data w unobserved variable L1
#' n <- 100
#' L1 <- rnorm(n) 
#' X1 <- rnorm(n)
#' X2 <- L1 + X1 + rnorm(n)
#' X3 <- X1 + rnorm(n)
#' X4 <- X3 + L1 + rnorm(n)
#' d <- data.frame(p1_X1 = X1,
#'                 p1_X2 = X2,
#'                 p2_X3 = X3,
#'                 p2_X4 = X4)
#' 
#' # use FCI algorithm to recover PAG                
#' res <- fci(d, test = corTest)
#' 
#' # plot
#' plot(res)
#' 
#' @importFrom Rgraphviz renderGraph layoutGraph 
#' @importFrom graph nodes numNodes edgeRenderInfo<-
#'
#' @export
plot.pag <- function(x, ...) {
  thisamat <- t(amat(x))
  thisg <- as.graphNEL(thisamat)
  nn <- nodes(thisg)
  p <- numNodes(thisg)
  n.edges <- nedges(thisamat)
  ahs <- ats <- rep("none", n.edges)
  nms <- character(n.edges)
  cmat <- array(c("0" = "none",   "1" = "odot",
                  "2" = "normal", "3" = "none")[as.character(thisamat)],
                dim = dim(thisamat), dimnames = dimnames(thisamat))
  iE <- 0L
  for (i in seq_len(p-1)) {
    x <- nn[i]
    for (j in (i+1):p) {
      y <- nn[j]
      if (thisamat[x,y] != 0) {
        iE <- iE + 1L
        ahs[[iE]] <- cmat[x,y]
        ats[[iE]] <- cmat[y,x]
        nms[[iE]] <- paste0(x,"~",y)
      }
    }
  }
  names(ahs) <- names(ats) <- nms
  edgeRenderInfo(thisg) <- list(arrowhead = ahs, arrowtail = ats)
  renderGraph(layoutGraph(thisg))
}


#' Plot temporal partial ancestral graph (TPAG)
#' 
#' @author This code is a modification of the fciAlgo plotting method implemented 
#' in the pcalg package. 
#' 
#' @param x tpag object
#'  to be plotted (as outputted from \code{\link{tfci}}).
#' @param ... Currently not in use. 
#'
#' @return No return value, the function is called for its side-effects (plotting). 
#'
#' @examples
#' # simulate linear Gaussian data w unobserved variable L1
#' n <- 100
#' L1 <- rnorm(n) 
#' X1 <- rnorm(n)
#' X2 <- L1 + X1 + rnorm(n)
#' X3 <- X1 + rnorm(n)
#' X4 <- X3 + L1 + rnorm(n)
#' d <- data.frame(p1_X1 = X1,
#'                 p1_X2 = X2,
#'                 p2_X3 = X3,
#'                 p2_X4 = X4)
#' 
#' # use FCI algorithm to recover PAG                
#' res <- tfci(d, order = c("p1", "p2"), test = corTest)
#' 
#' # plot
#' plot(res)
#' @export
plot.tpag <- function(x, ...) {
  plot.pag(x, ...)
}
