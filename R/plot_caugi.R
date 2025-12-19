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
  x_knowledge <- x$knowledge
  x_caugi <- x$caugi

  # --------------------------
  # 1. Plot the knowledge graph first
  # --------------------------
  k_info <- plot.knowledge(x_knowledge, ...)
  g <- k_info$graph

  # --------------------------
  # 2. Add all caugi nodes (if missing)
  # --------------------------
  caugi_nodes <- x_caugi@nodes$name
  missing_nodes <- setdiff(caugi_nodes, igraph::V(g)$name)
  if (length(missing_nodes) > 0) {
    g <- igraph::add_vertices(g, length(missing_nodes), name = missing_nodes)
  }

  # --------------------------
  # 3. Extend layout for new caugi nodes
  # --------------------------
  all_nodes <- igraph::V(g)$name
  new_nodes <- setdiff(all_nodes, rownames(k_info$layout))

  if (length(new_nodes) > 0) {
    # Center of existing knowledge layout
    center_x <- mean(k_info$layout[, 1])
    center_y <- mean(k_info$layout[, 2])
    radius <- 1 # adjust if needed

    angles <- seq(0, 2 * pi, length.out = length(new_nodes) + 1)[-1]
    new_layout <- matrix(NA,
      nrow = length(new_nodes), ncol = 2,
      dimnames = list(new_nodes, c("x", "y"))
    )
    new_layout[, 1] <- center_x + radius * cos(angles)
    new_layout[, 2] <- center_y + radius * sin(angles)

    layout_matrix <- rbind(k_info$layout, new_layout)
  } else {
    layout_matrix <- k_info$layout
  }

  # --------------------------
  # 4. Add caugi edges (unstyled), skipping required edges
  # --------------------------
  if (!is.null(x_caugi@edges) && nrow(x_caugi@edges) > 0) {
    req_edges <- x_knowledge$edges[x_knowledge$edges$status == "required", ]
    req_pairs <- paste(req_edges$from, req_edges$to, sep = "->")

    for (i in seq_len(nrow(x_caugi@edges))) {
      row <- x_caugi@edges[i, ]
      from <- row$from
      to <- row$to
      edge_type <- row$edge

      # Skip if already required
      if (paste(from, to, sep = "->") %in% req_pairs) next

      # Swap from/to to match knowledge graph convention
      g <- igraph::add_edges(g, c(to, from))
      eid <- igraph::ecount(g)

      if (edge_type == "-->") {
        igraph::E(g)$arrow.mode[eid] <- 1
        igraph::E(g)$lty[eid] <- 1
        igraph::E(g)$color[eid] <- "black"
        igraph::E(g)$width[eid] <- 1.5
      } else if (edge_type == "---") {
        igraph::E(g)$arrow.mode[eid] <- 0
        igraph::E(g)$lty[eid] <- 2
        igraph::E(g)$color[eid] <- "black"
        igraph::E(g)$width[eid] <- 1.5
      }
    }
  }

  # --------------------------
  # 5. Robustly extend layout to include any new nodes
  # --------------------------
  all_nodes <- igraph::V(g)$name
  layout_matrix <- k_info$layout
  if (is.null(rownames(layout_matrix))) rownames(layout_matrix) <- k_info$nodes

  missing_nodes <- setdiff(all_nodes, rownames(layout_matrix))
  if (length(missing_nodes) > 0) {
    # Bounding box of existing layout
    x_range <- range(layout_matrix[, 1], na.rm = TRUE)
    y_range <- range(layout_matrix[, 2], na.rm = TRUE)

    # Compute radius: max spread + padding
    radius <- max(diff(x_range), diff(y_range)) + 1
    if (radius == 0) radius <- 1.5 # fallback for tiny graphs

    # Center of existing nodes
    center_x <- mean(x_range)
    center_y <- mean(y_range)

    # Angles for new nodes
    angles <- seq(0, 2 * pi, length.out = length(missing_nodes) + 1)[-1]

    # Create layout for missing nodes
    new_layout <- matrix(NA,
      nrow = length(missing_nodes), ncol = 2,
      dimnames = list(missing_nodes, c("x", "y"))
    )
    new_layout[, 1] <- center_x + radius * cos(angles)
    new_layout[, 2] <- center_y + radius * sin(angles)

    if (length(missing_nodes) <= 3) {
      new_layout[, 1] <- new_layout[, 1] + runif(length(missing_nodes), -0.1, 0.1)
      new_layout[, 2] <- new_layout[, 2] + runif(length(missing_nodes), -0.1, 0.1)
    }

    layout_matrix <- rbind(layout_matrix, new_layout)
  }

  # Reorder to match g vertices exactly
  layout_matrix <- layout_matrix[all_nodes, , drop = FALSE]


  # --------------------------
  # 5. Final plot
  # --------------------------
  igraph::plot.igraph(
    g,
    layout = layout_matrix,
    edge.color = c(
      k_info$edge_colors,
      igraph::E(g)$color[(length(k_info$edge_colors) + 1):igraph::ecount(g)]
    ),
    edge.width = c(
      k_info$edge_lwd,
      igraph::E(g)$width[(length(k_info$edge_lwd) + 1):igraph::ecount(g)]
    ),
    edge.lty = c(
      k_info$edge_lty,
      igraph::E(g)$lty[(length(k_info$edge_lty) + 1):igraph::ecount(g)]
    ),
    edge.arrow.mode = c(
      k_info$edge_arrow,
      igraph::E(g)$arrow.mode[(length(k_info$edge_arrow) + 1):igraph::ecount(g)]
    ),
    edge.curved = k_info$curved,
    vertex.color = "lightblue",
    vertex.frame.color = NA,
    vertex.label = igraph::V(g)$name,
    vertex.label.color = "black",
    vertex.label.cex = 0.9,
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
  vars <- x$vars
  edges_df <- x$edges
  tiers <- x$tiers

  g <- igraph::make_empty_graph(directed = TRUE)
  g <- igraph::add_vertices(g, nrow(vars), name = vars$var)

  # ----- Add required / forbidden edges -----
  if (nrow(edges_df) > 0) {
    for (status in c("required", "forbidden")) {
      sub_edges <- edges_df[edges_df$status == status, ]
      if (nrow(sub_edges) > 0) {
        # Swap from/to to match igraph direction
        edge_list <- as.vector(t(sub_edges[, c("to", "from")]))
        g <- igraph::add_edges(g, edge_list)
        edges_to_mark <- (igraph::ecount(g) - nrow(sub_edges) + 1):igraph::ecount(g)
        g <- igraph::set_edge_attr(g, name = status, value = TRUE, index = edges_to_mark)
      }
    }
  }
  if (is.null(igraph::E(g)$required)) igraph::E(g)$required <- FALSE
  if (is.null(igraph::E(g)$forbidden)) igraph::E(g)$forbidden <- FALSE

  # ----- Layout: tiered with jitter and NA nodes around groups -----
  nodes <- igraph::V(g)$name
  layout_matrix <- matrix(NA,
    nrow = length(nodes), ncol = 2,
    dimnames = list(nodes, c("x", "y"))
  )

  if (length(tiers$label) > 0) {
    tier_index <- stats::setNames(seq_len(nrow(tiers)), tiers$label)
    x_pos <- sapply(vars$tier, function(t) if (is.na(t)) NA else tier_index[t])
    names(x_pos) <- vars$var

    # Tiered nodes layout
    for (tier in unique(x_pos[!is.na(x_pos)])) {
      tier_vars <- nodes[!is.na(x_pos[nodes]) & x_pos[nodes] == tier]
      n_tier <- length(tier_vars)
      layout_matrix[tier_vars, 1] <- rep(c(tier, tier + x_jitter), ceiling(n_tier / 2))[1:n_tier]
      layout_matrix[tier_vars, 2] <- c(-floor(n_tier / 2):floor(n_tier / 2))[1:n_tier]
    }

    # NA nodes around center
    na_vars <- nodes[is.na(x_pos[nodes])]
    if (length(na_vars) > 0) {
      center_x <- mean(layout_matrix[!is.na(layout_matrix[, 1]), 1])
      center_y <- mean(layout_matrix[!is.na(layout_matrix[, 2]), 2])
      radius <- max(
        diff(range(layout_matrix[!is.na(layout_matrix[, 1]), 1])),
        diff(range(layout_matrix[!is.na(layout_matrix[, 2]), 2]))
      ) / 2 + 1
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
    rownames(layout_matrix) <- nodes # ensure rownames exist
    curved <- TRUE
  }

  # ----- Edge styling -----
  if (is.null(igraph::E(g)$required)) igraph::E(g)$required <- rep(FALSE, igraph::ecount(g))
  if (is.null(igraph::E(g)$forbidden)) igraph::E(g)$forbidden <- rep(FALSE, igraph::ecount(g))

  edge_colors <- ifelse(igraph::E(g)$forbidden, "red",
    ifelse(igraph::E(g)$required, "blue", "black")
  )
  edge_lwd <- ifelse(igraph::E(g)$forbidden, 2,
    ifelse(igraph::E(g)$required, 3, 1)
  )
  edge_lty <- ifelse(igraph::E(g)$forbidden, 2, 1)
  edge_arrow <- ifelse(igraph::E(g)$required, 1, 0)

  # ----- Vertex sizes -----
  labels <- nodes
  widths_in <- graphics::strwidth(labels, units = "inches", cex = 0.9)
  vertex_sizes <- (widths_in * 72 + 10) * vertex_size_scale

  # ----- Plot -----
  igraph::plot.igraph(
    g,
    layout = layout_matrix,
    edge.color = edge_colors,
    edge.width = edge_lwd,
    edge.lty = edge_lty,
    edge.arrow.mode = edge_arrow,
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

  # TODO refactor to make internal function that returns, so public one doesn't ...
  # ----- Return full info including rownames for robust use -----
  return(list(
    graph = g,
    layout = layout_matrix,
    nodes = nodes,
    edge_colors = edge_colors,
    edge_lwd = edge_lwd,
    edge_lty = edge_lty,
    edge_arrow = edge_arrow,
    vertex_sizes = vertex_sizes,
    groups = groups,
    curved = curved
  ))
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
