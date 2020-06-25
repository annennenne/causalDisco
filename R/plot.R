#' Plot temporal partially directed acyclic graph (TPDAG)
#'
#' blabla.
#'
#' @param x tpdag object to be plotted (as outputted from \code{\link{tpc}}).
#'
#' @export
plot.tpdag <- function(x, ...) {
  x_amat <- x$amat
  x_order <- x$order
  x_psi <- x$psi

  plotOrderedAmat(x_amat, data = NULL, order = x_order, psi = x_psi)
}



############################################################################
## Not exported below ######################################################
############################################################################


plotOrderedAmat <- function(amat, data, order, psi = NULL,
                            addTimeAxis = TRUE,
                            addPsi = TRUE,
                            CPDAG = TRUE,
                            labels = NULL, periodLabels = NULL,
                            vertex.size = 6, jitter = 5,
                            space =  5,
                            mark.border = NA,
                            edge.arrow.size = 0.5,
                            edge.width = 2,
                            edge.curved = TRUE,
                            sep = "_",
                            colors = RColorBrewer::brewer.pal("Dark2", n = length(order)),
                            doOrderedDirect = TRUE,
                            ...) {
  if (is.null(periodLabels)) periodLabels <- order

  cols <- colors[1:length(order)]
  vnames <- colnames(amat)
  # if (CPDAG) bn <- bnlearn::cpdag(bn)

  thisGraph <- igraph::graph_from_adjacency_matrix(t(amat))

  groups <- sapply(order, function(x) getvar(vnames, x))

  data <- NULL

  mat <- orderedLayout(vnames, order, sep = sep, jitter = jitter, space = space)
  edges <- igraph::as_edgelist(thisGraph)

  #drop one copy of double edges
  makedouble <- NULL
  makedouble_new <- NULL
  duplies <- NULL
  nEdges <- nrow(edges)
  indexes <- 1:nEdges
  dontcheck <- NULL
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
    thisGraph <- igraph::delete.edges(thisGraph, duplies)
    edges <- igraph::as_edgelist(thisGraph)
    for (i in 1:length(makedouble)) {
      # browser()
      thisEdge <- oldedges[makedouble[i],]
      for (j in 1:nrow(edges)) {
        if (identical(thisEdge,edges[j, ])) makedouble_new <- c(makedouble_new, j)
      }
    }
  }
  groupnames <- colnames(groups)
  nEdges <- nrow(edges) #recalc
  edgecolors <- rep("", nEdges)
  arrowmodes <- rep(">", nEdges)
  ltys <- rep(1, nEdges)
  if (nEdges > 0) {
    for (i in 1:nEdges) {
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
 # browser()

  igraph::plot.igraph(thisGraph, mark.groups = as.data.frame(groups, stringsAsFactors = FALSE),
                      edge.color = edgecolors,
                      edge.arrow.mode = arrowmodes,
                      layout = mat, vertex.size = vertex.size,
                      edge.arrow.size = edge.arrow.size,
                      edge.width = edge.width,
                      edge.curved = edge.curved,
                      mark.border = mark.border,
                      mark.col = scales::alpha(cols, alpha = 0.2),
                      vertex.color = "grey",
                      vertex.frame.color = NA,
                      vertex.label.color = "black",
                      vertex.label = labels,
                      edge.lty = ltys,
                      vertex.label.family = "sans",
                      ...)
  if (addTimeAxis) axis(1, seq(-1, 1, 2/(length(periodLabels)-1)), periodLabels, cex.axis = 1.5)
  if (!is.null(psi) & addPsi) {
    #text(x = -0.75, y = 1.1, labels = bquote(psi == .(psi)), cex = 2)
    mtext(bquote(psi == .(sciNotation(psi))), side = 3, line = 2)
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
  x[sapply(strsplit(x, sep), function(x) x[[1]])  == prefix]
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
