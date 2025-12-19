#' Plot temporal data generating mechanism
#'
#' Plots tpdag, tskeleton and tamat objects.
#'
#' @param x The tpdag/tskeleton or tamat to plot.
#' @param add_time_axis Logical indicating whether a time axis should be
#' added to the plot.
#' @param add_psi Logical indicating whether the sparsity level should be
#' added to the plot.
#' @param var_labels A named list of variable labels.
#' @param period_labels A character vector with labels for periods.
#' @param colors A character vector with colors to use for marking periods.
#' Should have at least as many elements as the numbers of periods.
#' @param ... Additional arguments passed to \code{\link[igraph]{plot.igraph}}.
#'
#' @return No return value, the function is called for its side-effects (plotting).
#'
#' @export
plot_tempo_mech <- function(
  x,
  add_time_axis = TRUE,
  add_psi = TRUE,
  var_labels = NULL,
  period_labels = NULL,
  colors = NULL,
  ...
) {
  if ("tamat" %in% class(x)) {
    x_amat <- x
    x_order <- attr(x, "order")
    x_psi <- NA
  } else if ("tskeleton" %in% class(x) || "tpdag" %in% class(x)) {
    x_amat <- x$tamat
    x_order <- attr(x_amat, "order")
    x_psi <- x$psi
  } else {
    stop("Input must by of type tamat, tpdag or tskeleton")
  }

  if (is.na(x_psi)) {
    add_psi <- FALSE
  }

  plot_ordered_amat(x_amat,
    order = x_order, psi = x_psi,
    add_time_axis = add_time_axis,
    add_psi = add_psi,
    var_labels = var_labels,
    period_labels = period_labels,
    colors = colors, ...
  )
}

############################################################################
## Not exported below ######################################################
############################################################################

#' Plot an ordered adjacency matrix
#'
#' Helper for visualizing temporal adjacency matrices with optional grouping,
#' colored edges, and shaded period regions.
#'
#' @param amat Square binary matrix giving directed edges (rows cause columns).
#' @param order Character vector of period prefixes in temporal order.
#' @param psi Numeric sparsity level to annotate above the plot (optional).
#' @param add_time_axis Logical indicating whether to draw a time axis.
#' @param add_psi Logical indicating whether to draw \eqn{\psi}.
#' @param CPDAG Logical; ignored here but retained for back-compatibility.
#' @param var_labels Named character vector for vertex labels. Defaults to
#'   \code{colnames(amat)}.
#' @param period_labels Character vector of axis labels for periods.
#' @param vertex.size Positive numeric vertex size passed to igraph.
#' @param jitter Numeric offset separating vertices within a period.
#' @param space Numeric horizontal gap between periods.
#' @param mark.border Colour for period borders passed to igraph.
#' @param edge.arrow.size,edge.width Numeric arrow and line size parameters.
#' @param edge.curved Logical or numeric curvature for igraph edges.
#' @param sep Character separator between prefix and variable name.
#' @param colors Character vector of fill colours for period shading.
#' @param ... Further arguments forwarded to \code{\link[igraph]{plot.igraph}}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side-effect of plotting.
#' @noRd
#' @keywords internal
plot_ordered_amat <- function(
  amat,
  order,
  psi = NULL,
  add_time_axis = TRUE,
  add_psi = TRUE,
  CPDAG = TRUE,
  var_labels = NULL, period_labels = NULL,
  vertex.size = 6, jitter = 5,
  space = 5,
  mark.border = NA,
  edge.arrow.size = 0.5,
  edge.width = 2,
  edge.curved = TRUE,
  sep = "_",
  colors = NULL,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "graphics", "igraph", "RColorBrewer", "scales"
    ),
    function_name = "plot_ordered_amat"
  )

  if (is.null(period_labels)) period_labels <- order

  ncol <- length(order)
  if (is.null(colors)) {
    if (ncol < 3) ncol <- 3
    colors <- RColorBrewer::brewer.pal("Dark2", n = ncol)
  }
  cols <- colors[seq_along(order)]

  vnames <- colnames(amat)

  # make sure var_labels is in the correct order
  if (is.null(var_labels)) {
    var_labels <- vnames
  } else {
    var_labels <- var_labels[vnames]
  }

  this_graph <- igraph::graph_from_adjacency_matrix(t(amat)) # igraph

  groups <- sapply(order, function(x) getvar(vnames, x), simplify = FALSE)


  mat <- ordered_layout(vnames, order, sep = sep, jitter = jitter, space = space)
  edges <- igraph::as_edgelist(this_graph) # igraph

  # drop one copy of double edges
  makedouble <- NULL
  makedouble_new <- NULL
  duplies <- NULL
  n_edges <- nrow(edges)
  indexes <- 1:n_edges
  dont_check <- NULL

  #  if (type != "pdag") {
  #  dont_check <- indexes
  # }
  if (n_edges > 0) {
    for (i in indexes) {
      this_one <- edges[i, ]
      dont_check <- c(dont_check, i)
      for (j in indexes[-dont_check]) {
        if (all(edges[j, c(2, 1)] == this_one)) {
          duplies <- c(duplies, j)
          makedouble <- c(makedouble, i)
        }
      }
    }
  }
  oldedges <- edges
  this_graph <- igraph::delete.edges(this_graph, duplies) # igraph
  edges <- igraph::as_edgelist(this_graph) # igraph

  for (i in seq_along(makedouble)) {
    this_edge <- oldedges[makedouble[i], ]
    for (j in seq_len(nrow(edges))) {
      if (identical(this_edge, edges[j, ])) {
        makedouble_new <- c(makedouble_new, j)
      }
    }
  }

  groupnames <- names(groups)
  n_edges <- nrow(edges) # recalc
  edgecolors <- rep("", n_edges)
  arrowmodes <- rep(">", n_edges)

  ltys <- rep(1, n_edges)
  if (n_edges > 0) {
    for (i in 1:n_edges) {
      thisVar <- edges[i, 1]
      thisPrefix <- unlist(strsplit(thisVar, "_", 1))[1]
      edgecolors[i] <- cols[which(thisPrefix == groupnames)]
      if (i %in% makedouble_new) {
        # if (doOrderedDirect) {
        #  arrowmodes[i] <- ">"
        # } else
        arrowmodes[i] <- "-"
        ltys[i] <- 1
      }
    }
  }


  igraph::plot.igraph(
    this_graph,
    mark.groups = groups,
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
    vertex.label = var_labels,
    edge.lty = ltys,
    vertex.label.family = "sans",
    ...
  ) # igraph
  if (add_time_axis) {
    graphics::axis(1, seq(-1, 1, 2 / (length(period_labels) - 1)),
      period_labels,
      cex.axis = 1.5
    )
  }
  if (!is.null(psi) && add_psi) {
    #  mtext(bquote(psi == .(sci_notation(psi))), side = 3, line = 2)
    graphics::mtext(bquote(psi == .(psi)), side = 3, line = 2)
  }
}

#' Compute vertex layout for temporal plotting
#'
#' Converts a set of variable names and their temporal prefixes into an
#' \eqn{n \times 2} matrix of \eqn{(x, y)} coordinates suitable for igraph.
#'
#' @param vnames Character vector of full variable names (including prefix).
#' @param order Character vector of unique period prefixes in temporal order.
#' @param sep Character separator between prefix and the remainder of the name.
#' @param jitter Numeric horizontal jitter applied within a period.
#' @param space Numeric horizontal gap between successive periods.
#'
#' @return A numeric matrix with two columns named \code{"x"} and \code{"y"}.
#' @noRd
#' @keywords internal
ordered_layout <- function(vnames, order, sep = "_", jitter, space) {
  out_mat <- matrix(NA,
    nrow = length(vnames), ncol = 2,
    dimnames = list(vnames, c("x", "y"))
  )
  n_pfs <- length(order)

  j <- 1
  for (i in 1:n_pfs) {
    these_var <- getvar(vnames, order[i])
    n_these <- length(these_var)
    jitter_vals <- rep(c(j, j + jitter), ceiling(n_these / 2))[1:n_these]
    out_mat[these_var, 1] <- jitter_vals
    out_mat[these_var, 2] <- c(-floor(n_these / 2):floor(n_these / 2))[1:n_these]
    j <- j + 2 * jitter + space
  }
  out_mat
}


#' Extract variables that share a prefix
#'
#' Generic for selecting variables whose names start with a given prefix.
#'
#' @param x Object to search: character vector or data frame.
#' @param prefix Character prefix of interest.
#' @param sep Character separator between prefix and remainder of name.
#'
#' @return Character vector of matching variable names.
#' @noRd
#' @keywords internal
getvar <- function(x, prefix, sep = "_") UseMethod("getvar")

#' @rdname getvar
#' @noRd
#' @keywords internal
getvar.character <- function(x, prefix, sep = "_") {
  out <- x[sapply(strsplit(x, sep), function(x) x[[1]]) == prefix]
  # if(is.data.frame(out)) out <- as.list(out)
  out
}

#' @rdname getvar
#' @noRd
#' @keywords internal
getvar.data.frame <- function(x, prefix, sep = "_") {
  getvar(names(x), prefix = prefix, sep = sep)
}

#' Convert numeric to scientific-notation expression
#'
#' Produces an \code{expression} representing a single value in scientific
#' notation, useful for annotation in base graphics functions.
#'
#' from https://stat.ethz.ch/pipermail/r-help/2006-January/086034.html
#'
#' @param x Numeric vector.
#' @param digits Integer number of significant digits for the mantissa.
#'
#' @return An \code{expression} (or vector of expressions) for pretty printing.
#' @noRd
#' @keywords internal
sci_notation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sci_notation(x[1]), sci_notation(x[-1])))
  }
  if (!x) {
    return(0)
  }
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  as.expression(substitute(
    base %*% 10^exponent,
    list(base = base, exponent = exponent)
  ))
}
