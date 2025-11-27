#' Plot a Causal Graph from a `knowledgeable_caugi` Object
#' This function visualizes a causal graph stored within a `caugi` object.
#' @param x A `caugi` object containing the causal graph and knowledge.
#' @param ... Additional arguments passed to the plot function.
#' @return A plot of the causal graph.
#' @method plot knowledgeable_caugi
#' @export
plot.knowledgeable_caugi <- function(x, ...) {
  x_caugi <- x$caugi
  x_knowledge <- x$knowledge

  nodes <- x_caugi@nodes$name
  edges_df <- x_caugi@edges

  edge_list <- c()
  arrow_mode <- c()

  for (i in seq_len(nrow(edges_df))) {
    from <- edges_df$from[i]
    to <- edges_df$to[i]
    e <- edges_df$edge[i]

    if (e == "-->") {
      edge_list <- c(edge_list, to, from) # Swap to from to match igraph edge direction
      arrow_mode <- c(arrow_mode, 1)
    } else if (e == "---") {
      edge_list <- c(edge_list, from, to)
      arrow_mode <- c(arrow_mode, 0)
    }
  }

  # --- Build graph ---
  g <- igraph::make_empty_graph(directed = TRUE) |>
    igraph::add_vertices(length(nodes), name = nodes) |>
    igraph::add_edges(edge_list)

  igraph::E(g)$arrow.mode <- arrow_mode
  igraph::E(g)$required <- FALSE
  igraph::E(g)$forbidden <- FALSE

  # --- Add forbidden edges ---
  forb_edges <- x_knowledge$edges[x_knowledge$edges$status == "forbidden", ]

  if (nrow(forb_edges) > 0) {
    forb_edge_list <- as.vector(t(forb_edges[, c("from", "to")]))

    old_ecount <- igraph::ecount(g)
    g <- g |> igraph::add_edges(forb_edge_list)
    new_ecount <- igraph::ecount(g)

    new_ids <- (old_ecount + 1):new_ecount

    # Forbidden attributes
    igraph::E(g)$forbidden[new_ids] <- TRUE

    arrow_mode <- c(arrow_mode, rep(1, length(new_ids)))
    igraph::E(g)$arrow.mode <- arrow_mode
  }

  # --- Required edges ---
  req_edges <- x_knowledge$edges[x_knowledge$edges$status == "required", ]
  req_pairs <- paste(req_edges$to, req_edges$from, sep = "->")

  igraph_pairs <- paste(
    igraph::as_edgelist(g)[, 1],
    igraph::as_edgelist(g)[, 2],
    sep = "->"
  )

  is_required <- igraph_pairs %in% req_pairs
  igraph::E(g)$required <- is_required | igraph::E(g)$required

  # --- Layout: tiered or circular ---
  tier_order <- x_knowledge$tiers$label
  if (length(tier_order) != 0) {
    tier_lookup <- stats::setNames(seq_along(tier_order), tier_order)
    x_pos <- tier_lookup[x_knowledge$vars$tier]
    names(x_pos) <- x_knowledge$vars$var
    x_coords <- x_pos[nodes]

    y_coords <- numeric(length(nodes))
    for (tier in tier_order) {
      nodes_in_tier <- x_knowledge$vars$var[x_knowledge$vars$tier == tier]
      idx <- which(nodes %in% nodes_in_tier)
      y_coords[idx] <- seq(from = 1, to = length(idx), length.out = length(idx))
    }

    layout_matrix <- cbind(x_coords, y_coords)
    colnames(layout_matrix) <- c("x_coords", "y_coords")
    rownames(layout_matrix) <- nodes
    curve_edges <- FALSE
  } else {
    layout_matrix <- igraph::layout_in_circle(g)
    igraph::E(g)$curved <- igraph::curve_multiple(g)
    curve_edges <- TRUE
  }

  # --- Edge styling ---
  edge_colors <- ifelse(
    igraph::E(g)$forbidden, "darkred",
    ifelse(igraph::E(g)$required, "red", "black")
  )

  edge_lwd <- ifelse(
    igraph::E(g)$forbidden, 2,
    ifelse(igraph::E(g)$required, 3, 1)
  )

  edge_lty <- ifelse(igraph::E(g)$forbidden, 2, 1) # dashed for forbidden

  # --- Plot ---
  plot(
    g,
    layout = layout_matrix,
    edge.arrow.size = 0.6,
    edge.arrow.mode = igraph::E(g)$arrow.mode,
    edge.curved = curve_edges,
    edge.color = edge_colors,
    edge.width = edge_lwd,
    edge.lty = edge_lty,
    vertex.size = 28,
    vertex.color = "lightblue",
    vertex.label.color = "black",
    vertex.label.cex = 0.9,
    ...
  )
}


#' @export
# Wrap in local to prevent issues (see https://github.com/RConsortium/S7/issues/390 (another related issue is #540))
local({
  S7::method(plot, caugi::caugi) <- function(x, ...) {
    x_caugi <- x
    nodes <- x_caugi@nodes$name
    edges_df <- x_caugi@edges

    # --- Build edge list + arrow modes ---
    edge_list <- c()
    arrow_mode <- c()

    for (i in seq_len(nrow(edges_df))) {
      from <- edges_df$from[i]
      to <- edges_df$to[i]
      e <- edges_df$edge[i]

      if (e == "-->") {
        # swap direction to match arrow head convention (same as other function)
        edge_list <- c(edge_list, to, from)
        arrow_mode <- c(arrow_mode, 1)
      } else if (e == "---") {
        edge_list <- c(edge_list, from, to)
        arrow_mode <- c(arrow_mode, 0)
      }
    }

    # --- Build igraph ---
    g <- igraph::make_empty_graph(directed = TRUE) |>
      igraph::add_vertices(length(nodes), name = nodes) |>
      igraph::add_edges(edge_list)

    igraph::E(g)$arrow.mode <- arrow_mode

    layout_matrix <- igraph::layout_in_circle(g)

    # --- Plot ---
    plot(
      g,
      layout = layout_matrix,
      edge.arrow.size = 0.6,
      edge.arrow.mode = arrow_mode,
      vertex.size = 28,
      vertex.color = "lightblue",
      vertex.label.color = "black",
      vertex.label.cex = 0.9,
      ...
    )
  }
})
