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
#' @param ... Additional arguments passed to igraph `plot` and `plot.knowledge`.
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
  x_knowledge <- x$knowledge
  x_caugi <- x$caugi

  # --------------------------
  # 1. Knowledge plot info
  # --------------------------
  k_info <- .plot_knowledge_internal(x_knowledge, ...)
  g <- k_info$graph

  vertex_sizes <- k_info$vertex_sizes
  if (length(vertex_sizes) > 0) {
    names(vertex_sizes) <- k_info$nodes
  }

  layout_matrix <- k_info$layout

  # --------------------------
  # 2. Add all CAUGI nodes
  # --------------------------
  caugi_nodes <- x_caugi@nodes$name
  missing_nodes <- setdiff(caugi_nodes, igraph::V(g)$name)

  if (length(missing_nodes) > 0) {
    g <- igraph::add_vertices(g, length(missing_nodes), name = missing_nodes)
  }

  # --------------------------
  # 3. Extend layout for new nodes
  # --------------------------
  all_nodes <- igraph::V(g)$name
  new_nodes <- setdiff(all_nodes, rownames(layout_matrix))

  if (length(new_nodes) > 0) {
    if (nrow(layout_matrix) == 0) {
      center_x <- 0
      center_y <- 0
      radius <- 1.5
    } else {
      x_range <- range(layout_matrix[, 1], na.rm = TRUE)
      y_range <- range(layout_matrix[, 2], na.rm = TRUE)
      center_x <- mean(x_range)
      center_y <- mean(y_range)
      radius <- max(diff(x_range), diff(y_range)) + 1
      if (radius == 0) radius <- 1.5
    }

    angles <- seq(0, 2 * pi, length.out = length(new_nodes) + 1)[-1]

    new_layout <- matrix(
      NA_real_,
      nrow = length(new_nodes),
      ncol = 2,
      dimnames = list(new_nodes, c("x", "y"))
    )

    new_layout[, 1] <- center_x + radius * cos(angles)
    new_layout[, 2] <- center_y + radius * sin(angles)

    if (length(new_nodes) <= 3) {
      new_layout[, 1] <- new_layout[, 1] +
        stats::runif(length(new_nodes), -0.1, 0.1)
      new_layout[, 2] <- new_layout[, 2] +
        stats::runif(length(new_nodes), -0.1, 0.1)
    }

    layout_matrix <- rbind(layout_matrix, new_layout)
  }

  # --------------------------
  # 4. Add CAUGI edges
  # --------------------------
  if (!is.null(x_caugi@edges) && nrow(x_caugi@edges) > 0) {
    req_edges <- x_knowledge$edges[x_knowledge$edges$status == "required", ]
    req_pairs <- paste(req_edges$from, req_edges$to, sep = "->")

    for (i in seq_len(nrow(x_caugi@edges))) {
      row <- x_caugi@edges[i, ]
      from <- row$from
      to <- row$to

      if (paste(from, to, sep = "->") %in% req_pairs) {
        next
      }

      g <- igraph::add_edges(g, c(to, from))
      eid <- igraph::ecount(g)

      if (row$edge == "-->") {
        igraph::E(g)$arrow.mode[eid] <- 1
        igraph::E(g)$lty[eid] <- 1
      } else {
        igraph::E(g)$arrow.mode[eid] <- 0
        igraph::E(g)$lty[eid] <- 2
      }

      igraph::E(g)$color[eid] <- "black"
      igraph::E(g)$width[eid] <- 1.5
    }
  }

  # --------------------------
  # 5. Vertex sizes
  # --------------------------
  missing_nodes <- setdiff(all_nodes, names(vertex_sizes))

  if (length(missing_nodes) > 0) {
    default_size <- if (length(vertex_sizes) > 0) {
      stats::median(vertex_sizes)
    } else {
      20
    }

    vertex_sizes <- c(
      vertex_sizes,
      stats::setNames(rep(default_size, length(missing_nodes)), missing_nodes)
    )
  }

  layout_matrix <- layout_matrix[all_nodes, , drop = FALSE]
  vertex_sizes <- vertex_sizes[all_nodes]

  # --------------------------
  # 6. Final plot
  # --------------------------
  igraph::plot.igraph(
    g,
    layout = layout_matrix,
    edge.color = igraph::E(g)$color,
    edge.width = igraph::E(g)$width,
    edge.lty = igraph::E(g)$lty,
    edge.arrow.mode = igraph::E(g)$arrow.mode,
    edge.curved = k_info$curved,
    vertex.color = "lightblue",
    vertex.frame.color = NA,
    vertex.label = igraph::V(g)$name,
    vertex.label.color = "black",
    vertex.label.cex = 0.9,
    vertex.size = vertex_sizes,
    mark.groups = k_info$groups,
    ...
  )
}


#' Plot a Knowledge Object
#'
#' Plot a knowledge object (tiers + required + forbidden)
#'
#' @param x A `knowledge` object.
#' @param x_jitter Amount of jitter to apply to x positions of tiered nodes (default 0).
#' @param vertex_size_scale Scaling factor for vertex sizes (default 1).
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
plot.knowledge <- function(x, x_jitter = 0, vertex_size_scale = 1, ...) {
  .plot_knowledge_internal(
    x,
    x_jitter,
    vertex_size_scale,
    return_info = FALSE,
    ...
  )
}

#' Internal function to plot knowledge objects
#' @inheritParams plot.knowledge
#' @param return_info Logical indicating whether to return plotting info.
#' @return If `return_info` is `TRUE`, a list containing plotting information.
#' @keywords internal
#' @noRd
.plot_knowledge_internal <- function(
  x,
  x_jitter = 0,
  vertex_size_scale = 1,
  return_info = TRUE,
  ...
) {
  vars <- x$vars
  edges_df <- x$edges
  tiers <- x$tiers

  if (nrow(vars) == 0) {
    g <- igraph::make_empty_graph(directed = TRUE)

    if (return_info) {
      return(list(
        graph = g,
        layout = matrix(
          numeric(0),
          ncol = 2,
          dimnames = list(character(0), c("x", "y"))
        ),
        nodes = character(0),
        edge_colors = character(0),
        edge_lwd = numeric(0),
        edge_lty = numeric(0),
        edge_arrow = numeric(0),
        vertex_sizes = numeric(0),
        groups = list(),
        curved = FALSE
      ))
    }

    return(invisible(g))
  }

  g <- igraph::make_empty_graph(directed = TRUE)
  g <- igraph::add_vertices(g, nrow(vars), name = vars$var)

  # ----- Add edges individually with styling -----
  if (nrow(edges_df) > 0) {
    for (i in seq_len(nrow(edges_df))) {
      row <- edges_df[i, ]
      from <- as.character(row$from)
      to <- as.character(row$to)

      g <- igraph::add_edges(g, c(to, from))
      eid <- igraph::ecount(g)

      if (row$status == "required") {
        igraph::E(g)$arrow.mode[eid] <- 1
        igraph::E(g)$lty[eid] <- 1
        igraph::E(g)$color[eid] <- "blue"
      } else {
        # forbidden
        igraph::E(g)$arrow.mode[eid] <- 1
        igraph::E(g)$lty[eid] <- 2
        igraph::E(g)$color[eid] <- "red"
      }
    }
  }

  # ----- Layout -----
  nodes <- igraph::V(g)$name
  layout_matrix <- matrix(
    NA,
    nrow = length(nodes),
    ncol = 2,
    dimnames = list(nodes, c("x", "y"))
  )

  if (length(tiers$label) > 0) {
    tier_index <- stats::setNames(seq_len(nrow(tiers)), tiers$label)
    x_pos <- sapply(vars$tier, function(t) if (is.na(t)) NA else tier_index[t])
    names(x_pos) <- vars$var

    for (tier in unique(x_pos[!is.na(x_pos)])) {
      tier_vars <- nodes[!is.na(x_pos[nodes]) & x_pos[nodes] == tier]
      n_tier <- length(tier_vars)
      layout_matrix[tier_vars, 1] <- rep(
        c(tier, tier + x_jitter),
        ceiling(n_tier / 2)
      )[1:n_tier]
      layout_matrix[tier_vars, 2] <- c(-floor(n_tier / 2):floor(n_tier / 2))[
        1:n_tier
      ]
    }

    na_vars <- nodes[is.na(x_pos[nodes])]
    if (length(na_vars) > 0) {
      center_x <- mean(layout_matrix[!is.na(layout_matrix[, 1]), 1])
      center_y <- mean(layout_matrix[!is.na(layout_matrix[, 2]), 2])
      radius <- max(
        diff(range(layout_matrix[!is.na(layout_matrix[, 1]), 1])),
        diff(range(layout_matrix[!is.na(layout_matrix[, 2]), 2]))
      ) /
        2 +
        1
      angles <- seq(0, 2 * pi, length.out = length(na_vars) + 1)[-1]
      layout_matrix[na_vars, 1] <- center_x + radius * cos(angles)
      layout_matrix[na_vars, 2] <- center_y + radius * sin(angles)
    }

    groups <- lapply(unique(vars$tier[!is.na(vars$tier)]), function(t) {
      group_vars <- vars$var[vars$tier == t]
      group_vars[group_vars %in% nodes]
    })
    curved <- FALSE
  } else {
    groups <- NULL
    layout_matrix <- igraph::layout_in_circle(g)
    rownames(layout_matrix) <- nodes
    curved <- TRUE
  }

  # ----- Vertex sizes -----
  labels <- nodes
  widths_in <- graphics::strwidth(labels, units = "inches", cex = 0.9)
  vertex_sizes <- (widths_in * 72 + 10) * vertex_size_scale

  # ----- Plot -----
  igraph::plot.igraph(
    g,
    layout = layout_matrix,
    edge.curved = curved,
    vertex.color = "lightblue",
    vertex.frame.color = NA,
    vertex.label = labels,
    vertex.label.color = "black",
    vertex.label.cex = 0.9,
    vertex.size = vertex_sizes,
    mark.groups = groups,
    ...
  )

  if (return_info) {
    return(list(
      graph = g,
      layout = layout_matrix,
      nodes = nodes,
      vertex_sizes = vertex_sizes,
      groups = groups,
      curved = curved
    ))
  } else {
    invisible(NULL)
  }
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
