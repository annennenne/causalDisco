#' Plot temporal data generating mechanism
#'
#' Plots tpdag, tskeleton and tamat objects.
#'
#' @param x The tpdag/tskeleton or tamat to plot. 
#' @param addTimeAxis Logical indicating whether a time axis should be
#' added to the plot.
#' @param addPsi Logical indicating whether the sparsity level should be
#' added to the plot.
#' @param varLabels A named list of variable labels.
#' @param periodLabels A character vector with labels for periods.
#' @param colors A character vector with colors to use for marking periods.
#' Should have at least as many elements as the numbers of periods.
#' @param ... Additional arguments passed to \code{\link[igraph]{plot.igraph}}. 
#' 
#' @return No return value, the function is called for its side-effects (plotting). 
#'
#' @export
plotTempoMech <- function(x, addTimeAxis = TRUE,
                          addPsi = TRUE,
                          varLabels = NULL,
                          periodLabels = NULL,
                          colors = NULL, ...) {
  
  if ("tamat" %in% class(x)) {
    x_amat <- x
    x_order <- attr(x, "order")
    x_psi <- NA
  } else if ("tskeleton" %in% class(x) | "tpdag" %in% class(x)) {
    x_amat <- x$tamat
    x_order <- attr(x_amat, "order")
    x_psi <- x$psi
  } else {
    stop("Input must by of type tamat, tpdag or tskeleton")
  }
  
  if (is.na(x_psi)) {
    addPsi <- FALSE
  }

  plotOrderedAmat(x_amat, order = x_order, psi = x_psi,
                  addTimeAxis = addTimeAxis,
                  addPsi = addPsi,
                  varLabels = varLabels,
                  periodLabels = periodLabels,
                  colors = colors, ...)

}



############################################################################
## Not exported below ######################################################
############################################################################


#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales alpha
#' @importFrom graphics axis mtext
#' @import igraph
plotOrderedAmat <- function(amat, order, 
                            psi = NULL,
                            addTimeAxis = TRUE,
                            addPsi = TRUE,
                            CPDAG = TRUE,
                            varLabels = NULL, periodLabels = NULL,
                            vertex.size = 6, jitter = 5,
                            space =  5,
                            mark.border = NA,
                            edge.arrow.size = 0.5,
                            edge.width = 2,
                            edge.curved = TRUE,
                            sep = "_",
                            colors = NULL,
                            ...) {
  if (is.null(periodLabels)) periodLabels <- order

  ncol <- length(order)
  if (is.null(colors)) {
    if (ncol < 3) ncol <- 3
    colors <- brewer.pal("Dark2", n = ncol) #RColorBrewer
  }
  cols <- colors[1:length(order)]

  vnames <- colnames(amat)

  #make sure varLabels is in the correct order
  if (is.null(varLabels)) {
    varLabels <- vnames
  } else {
    varLabels <- varLabels[vnames]
  }

  thisGraph <- graph_from_adjacency_matrix(t(amat)) #igraph

  groups <- sapply(order, function(x) getvar(vnames, x), simplify = FALSE)


  #  browser()
  mat <- orderedLayout(vnames, order, sep = sep, jitter = jitter, space = space)
  edges <- as_edgelist(thisGraph) #igraph

  #drop one copy of double edges
  makedouble <- NULL
  makedouble_new <- NULL
  duplies <- NULL
  nEdges <- nrow(edges)
  indexes <- 1:nEdges
  dontcheck <- NULL
  
#  if (type != "pdag") {
 #  dontcheck <- indexes
 # }  
  if (nEdges > 0) {
    for (i in indexes) {
      thisOne <- edges[i,]
      dontcheck <- c(dontcheck, i)
      for (j in indexes[-dontcheck]) {
        if (all(edges[j, c(2,1)] == thisOne)) {
          duplies <- c(duplies, j)
          makedouble <- c(makedouble, i)
        }
      }
    }
  }
  if (length(makedouble) > 0) {
    oldedges <- edges
    thisGraph <- delete.edges(thisGraph, duplies) #igraph
    edges <- as_edgelist(thisGraph) #igraph
    for (i in 1:length(makedouble)) {
      thisEdge <- oldedges[makedouble[i],]
      for (j in 1:nrow(edges)) {
        if (identical(thisEdge,edges[j, ])) makedouble_new <- c(makedouble_new, j)
      }
    }
  }
  groupnames <- names(groups)
  nEdges <- nrow(edges) #recalc
  edgecolors <- rep("", nEdges)
  arrowmodes <- rep(">", nEdges)
  
  ltys <- rep(1, nEdges)
  if (nEdges > 0) {
    for (i in 1:nEdges) {
      #browser()
      thisVar <- edges[i, 1]
      thisPrefix <- unlist(strsplit(thisVar, "_", 1))[1]
      edgecolors[i] <- cols[which(thisPrefix == groupnames)]
      if (i %in% makedouble_new) {
        #if (doOrderedDirect) {
        #  arrowmodes[i] <- ">"
        #} else
        arrowmodes[i] <- "-"
        ltys[i] <- 1
      }
    }
  }


  plot.igraph(thisGraph, mark.groups = groups,
                      edge.color = edgecolors,
                      edge.arrow.mode = arrowmodes,
                      layout = mat, vertex.size = vertex.size,
                      edge.arrow.size = edge.arrow.size,
                      edge.width = edge.width,
                      edge.curved = edge.curved,
                      mark.border = mark.border,
                      mark.col = alpha(cols, alpha = 0.2),
                      vertex.color = "grey",
                      vertex.frame.color = NA,
                      vertex.label.color = "black",
                      vertex.label = varLabels,
                      edge.lty = ltys,
                      vertex.label.family = "sans",
                      ...) #igraph
  if (addTimeAxis) axis(1, seq(-1, 1, 2/(length(periodLabels)-1)), periodLabels, cex.axis = 1.5)
  if (!is.null(psi) & addPsi) {
    #  mtext(bquote(psi == .(sciNotation(psi))), side = 3, line = 2)
    mtext(bquote(psi == .(psi)), side = 3, line = 2)
  }
}







#make a layout matrix from ordering
orderedLayout <- function(vnames, order, sep = "_", jitter, space) {
  outMat <- matrix(NA, nrow = length(vnames), ncol = 2,
                   dimnames = list(vnames, c("x", "y")))
  nPfs <- length(order)

  j <- 1
  for (i in 1:nPfs) {
    theseVar <- getvar(vnames, order[i])
    nThese <- length(theseVar)
    theseJitter <- rep(c(j, j+jitter), ceiling(nThese/2))[1:nThese]
    outMat[theseVar, 1] <- theseJitter
    outMat[theseVar, 2] <- c(-floor(nThese/2):floor(nThese/2))[1:nThese]
    j <- j+2*jitter+space
  }
  outMat
}



#Choose variables with a certain prefix (before _)
getvar <- function(x, prefix, sep = "_") UseMethod("getvar")

getvar.character <- function(x, prefix, sep = "_") {
  out <- x[sapply(strsplit(x, sep), function(x) x[[1]])  == prefix]
  #if(is.data.frame(out)) out <- as.list(out)
  out
}

getvar.data.frame <- function(x, prefix, sep = "_") {
  getvar(names(x), prefix = prefix, sep = sep)
}


#from https://stat.ethz.ch/pipermail/r-help/2006-January/086034.html
sciNotation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sciNotation(x[1]), sciNotation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  as.expression(substitute(base %*% 10^exponent,
                           list(base = base, exponent = exponent)))
}



# Builds on plot method from pcalg for fciAlgo objects

#plotPAG <- function(x, ...) {
#  if (any(c("tpdag", "pag") %in% class(x))) {
#    thisamat <- amat(x)
#  } else {
#    thisamat <- x
#  }
#  
#  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))
#  
#  
#}