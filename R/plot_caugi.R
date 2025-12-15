#' Build an `igraph` Object from a `caugi` object
#' @param x_caugi A `caugi` object.
#' @return An `igraph` object representing the causal graph.
#' @keywords internal
#' @noRd
build_caugi_igraph <- function(x_caugi) {
  nodes <- x_caugi@nodes$name
  edges_df <- x_caugi@edges

  edge_list <- c()
  arrow_mode <- c()

  for (i in seq_len(nrow(edges_df))) {
    from <- edges_df$from[i]
    to <- edges_df$to[i]
    e <- edges_df$edge[i]

    if (e == "-->") {
      # Swap to preserve arrow direction for igraph
      edge_list <- c(edge_list, to, from)
      arrow_mode <- c(arrow_mode, 1)
    } else if (e == "---") {
      edge_list <- c(edge_list, from, to)
      arrow_mode <- c(arrow_mode, 0)
    }
  }

  g <- igraph::make_empty_graph(directed = TRUE) |>
    igraph::add_vertices(length(nodes), name = nodes) |>
    igraph::add_edges(edge_list)

  igraph::E(g)$arrow.mode <- arrow_mode
  g
}

#' Plot a Causal Graph from an `igraph` object
#' @param g An `igraph` object representing the causal graph.
#' @param layout Optional layout matrix for node positions. If `NULL`, a circular layout is used.
#' @param curved Logical indicating whether to use curved edges. Default is `TRUE`.
#' @param ... Additional arguments passed to igraph `plot`.
#' @return A plot of the causal graph.
#' @keywords internal
#' @noRd
plot_caugi_graph <- function(g, layout = NULL, curved = TRUE, ...) {
  if (is.null(layout)) {
    layout <- igraph::layout_in_circle(g)
  }

  ## --------------------------
  ## Auto-scale vertex sizes to fit labels
  ## --------------------------
  labels <- igraph::V(g)$name

  widths_in <- graphics::strwidth(labels, units = "inches", cex = 0.9)
  vertex_sizes <- widths_in * 72 + 10 # inches → points

  plot(
    g,
    layout = layout,
    edge.arrow.mode = igraph::E(g)$arrow.mode,
    edge.curved = curved,
    vertex.color = "lightblue",
    vertex.label.color = "black",
    vertex.label.cex = 0.9,
    ...
  )
}


#' Plot a Causal Graph from a `knowledgeable_caugi` Object
#'
#' This function visualizes a causal graph stored within a `caugi` object.
#' It incorporates background knowledge to highlight required and forbidden edges. The required
#' edges are drawn in blue, while forbidden edges are shown in red and are dashed. If tiered
#' knowledge is provided, the nodes are arranged according to their tiers; otherwise, a circular
#' layout is used.
#'
#' @param x A `caugi` object containing the causal graph and knowledge.
#' @param ... Additional arguments passed to igraph `plot`.
#' @return A plot of the causal graph.
#' @method plot knowledgeable_caugi
#' @examples
#' data("tpc_example")
#'
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#'
#' cd_tges <- tges(engine = "causalDisco", score = "tbic")
#' disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
#'
#' plot(disco_cd_tges)
#'
#' @export
plot.knowledgeable_caugi <- function(x, ...) {
  x_caugi <- x$caugi
  x_knowledge <- x$knowledge

  g <- build_caugi_igraph(x_caugi)

  ## --------------------------
  ## Forbidden edges
  ## --------------------------
  igraph::E(g)$forbidden <- rep(FALSE, igraph::ecount(g)) # initialize

  forb <- x_knowledge$edges[x_knowledge$edges$status == "forbidden", ]

  if (nrow(forb) > 0) {
    # Apply same directional reversal as normal edges:
    forb_list <- unlist(apply(forb, 1, function(row) {
      from <- row[["from"]]
      to <- row[["to"]]
      c(to, from) # same swap as "-->"
    }))

    old_n <- igraph::ecount(g)
    g <- g |> igraph::add_edges(forb_list)
    new_ids <- (old_n + 1):igraph::ecount(g)

    # Mark forbidden edges
    igraph::E(g)$forbidden[new_ids] <- TRUE
    igraph::E(g)$required[new_ids] <- FALSE
    igraph::E(g)$arrow.mode[new_ids] <- 1
  }

  ## --------------------------
  ## Required edges
  ## --------------------------
  igraph::E(g)$required <- rep(FALSE, igraph::ecount(g))

  req <- x_knowledge$edges[x_knowledge$edges$status == "required", ]
  if (nrow(req) > 0) {
    req_pairs <- paste(req$to, req$from, sep = "->")
    g_pairs <- apply(igraph::as_edgelist(g), 1, function(x) paste(x, collapse = "->"))
    igraph::E(g)$required <- g_pairs %in% req_pairs
  } else {
    igraph::E(g)$required <- FALSE
  }

  ## --------------------------
  ## Layout: tiers vs circle
  ## --------------------------
  if (length(x_knowledge$tiers$label) > 0) {
    tiers <- x_knowledge$tiers$label
    vars <- x_knowledge$vars

    tier_index <- stats::setNames(seq_along(tiers), tiers)
    x_pos <- tier_index[vars$tier]
    names(x_pos) <- vars$var

    nodes <- igraph::V(g)$name
    x_coords <- x_pos[nodes]

    y_coords <- stats::ave(
      seq_along(nodes),
      vars$tier[match(nodes, vars$var)],
      FUN = seq_along
    )

    layout_matrix <- cbind(x_coords, y_coords)
    curved <- FALSE
  } else {
    layout_matrix <- igraph::layout_in_circle(g)
    igraph::E(g)$curved <- igraph::curve_multiple(g)
    curved <- TRUE
  }

  ## --------------------------
  ## Edge styling
  ## --------------------------
  edges <- igraph::E(g)

  edge_colors <- ifelse(edges$forbidden, "red",
    ifelse(edges$required, "blue", "black")
  )
  edge_lwd <- ifelse(edges$forbidden, 2,
    ifelse(edges$required, 3, 1)
  )
  edge_lty <- ifelse(edges$forbidden, 2, 1)

  ## --------------------------
  ## Auto-scale vertex sizes to fit labels
  ## --------------------------
  labels <- igraph::V(g)$name

  widths_in <- graphics::strwidth(labels, units = "inches", cex = 0.9)
  vertex_sizes <- widths_in * 72 + 10 # inches → points

  ## --------------------------
  ## Final plot
  ## --------------------------
  plot(
    g,
    layout = layout_matrix,
    edge.color = edge_colors,
    edge.width = edge_lwd,
    edge.lty = edge_lty,
    edge.arrow.mode = edges$arrow.mode,
    edge.curved = curved,
    vertex.color = "lightblue",
    vertex.label.color = "black",
    vertex.label.cex = 0.9,
    vertex.size = vertex_sizes,
    ...
  )
}


#' Plot a Knowledge Object
#'
#' Plot a knowledge object (tiers + required + forbidden)
#'
#' @param x A `knowledge` object.
#' @param ... Additional arguments passed to igraph `plot`.
#' @return A plot of the knowledge structure.
#' @examples
#' data("tpc_example")
#'
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#'
#' plot(kn)
#'
#' @export
plot.knowledge <- function(x, ...) {
  vars <- x$vars
  edges <- x$edges
  tiers <- x$tiers

  g <- igraph::make_empty_graph(directed = TRUE)
  g <- igraph::add_vertices(g, nrow(vars), name = vars$var)

  # ----- Required edges -----
  req <- edges[edges$status == "required", ]
  if (nrow(req) > 0) {
    req_list <- as.vector(t(req[, c("from", "to")]))
    g <- igraph::add_edges(g, req_list)
    igraph::E(g)$required <- TRUE
  } else {
    igraph::E(g)$required <- FALSE
  }

  # ----- Forbidden edges -----
  forb <- edges[edges$status == "forbidden", ]
  if (nrow(forb) > 0) {
    forb_list <- as.vector(t(forb[, c("from", "to")]))
    g <- igraph::add_edges(g, forb_list)
    new_edges <- (igraph::ecount(g) - nrow(forb) + 1):igraph::ecount(g)
    igraph::E(g)$forbidden <- FALSE
    igraph::E(g)$forbidden[new_edges] <- TRUE
  } else {
    igraph::E(g)$forbidden <- FALSE
  }

  # ----- Layout -----
  if (length(tiers$label) > 0) {
    tier_index <- stats::setNames(seq_len(nrow(tiers)), tiers$label)
    x_pos <- tier_index[vars$tier]
    names(x_pos) <- vars$var

    nodes <- igraph::V(g)$name
    x_coords <- x_pos[nodes]

    y_coords <- stats::ave(seq_along(nodes),
      vars$tier[match(nodes, vars$var)],
      FUN = seq_along
    )

    layout_matrix <- cbind(x_coords, y_coords)
    curved <- FALSE
  } else {
    layout_matrix <- igraph::layout_in_circle(g)
    igraph::E(g)$curved <- igraph::curve_multiple(g)
    curved <- TRUE
  }

  # ----- Edge styling -----
  edges <- igraph::E(g)
  edge_colors <- ifelse(edges$forbidden, "red",
    ifelse(edges$required, "blue", "black")
  )
  edge_lwd <- ifelse(edges$forbidden, 2,
    ifelse(edges$required, 3, 1)
  )
  edge_lty <- ifelse(edges$forbidden, 2, 1)

  # ----- Vertex sizes -----
  labels <- igraph::V(g)$name
  widths_in <- graphics::strwidth(labels, units = "inches", cex = 0.9)
  vertex_sizes <- widths_in * 72 + 10 # inches → points

  # ----- Plot -----
  plot(
    g,
    layout = layout_matrix,
    edge.color = edge_colors,
    edge.width = edge_lwd,
    edge.lty = edge_lty,
    vertex.color = "lightblue",
    vertex.label.color = "black",
    vertex.label.cex = 0.9,
    vertex.size = vertex_sizes,
    ...
  )
}


#
# Plots a `caugi` object using `igraph`
#
# @param x A `caugi` object.
# @param ... Additional arguments passed to igraph `plot`.
# @return A plot of the causal graph.
# @examples
# cg <- caugi::caugi(class = "PDAG")
#
# cg <- cg |>
#   caugi::add_nodes(c("A", "B", "C", "D", "E")) |> # A, B, C, D, E
#   caugi::add_edges(A %-->% B %-->% C) |> # A --> B --> C, D, E
#   caugi::set_edges(B %---% C) # A --> B --- C, D, E
# plot(cg)
#
#' @export
# Wrap in local to prevent issues (see https://github.com/RConsortium/S7/issues/390 (another related issue is #540))
local({
  S7::method(plot, caugi::caugi) <- function(x, ...) {
    g <- build_caugi_igraph(x)
    plot_caugi_graph(g, curved = FALSE, ...)
  }
})
