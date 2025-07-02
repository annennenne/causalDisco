#' discography — a tidy tibble for causal graphs
#'
#' @description
#' A lightweight S3 generic that converts many causal-discovery outputs into a
#' tibble with three columns:
#' * **from** — tail-end node
#' * **to**   — head-end node (or the other endpoint for symmetric edges)
#' * **edge_type** — one of `"-->"`, `"---"`, `"<->"`, `"o-o"`, `"--o"`, `"o->"`
#'
#' Symmetric relations (`"---"`, `"<->"`, `"o-o"`) appear **once**,
#' ordered by the supplied `nodes` vector (or alphabetically if `nodes` is `NULL`).
#' Self-loops, if present, are kept.
#'
#' @param x     A causal-graph object (see methods below).
#' @param nodes Optional character vector giving the desired node order.
#'              If `NULL`, the unique node names are sorted alphabetically.
#' @param ...   Passed on to methods.
#'
#' @return A tibble with class `"discography"`.
#' @export
discography <- function(x, nodes = NULL, ...) {
  UseMethod("discography")
}

# ──────────────────────────────────────────────────────────────────────────────
# helpers
# ──────────────────────────────────────────────────────────────────────────────

as_tibble_edges <- function(from, to, type, nodes = NULL, cpdag = FALSE) {
  # -----------------------------------------------------------------
  out <- tibble::tibble(from = from, to = to, edge_type = type)

  ## early-return for a graph with *no* edges ------------------------
  if (nrow(out) == 0L) {
    return(
      tibble::new_tibble(
        list(from = character(), to = character(), edge_type = character()),
        nrow = 0L,
        class = c("discography", "tbl_df", "tbl", "data.frame")
      )
    )
  }

  # -----------------------------------------------------------------
  symmetric_flag <- out$edge_type %in% c("---", "<->", "o-o")

  if (is.null(nodes)) {
    nodes <- sort(unique(c(out$from, out$to)))
  }

  directed <- dplyr::filter(out, !symmetric_flag)
  symmetric <- dplyr::filter(out, symmetric_flag)

  ## only normalise if *some* symmetric edges exist -----------------
  if (nrow(symmetric) > 0L) {
    symmetric <-
      symmetric |>
      dplyr::mutate(
        from = factor(from, levels = nodes),
        to   = factor(to, levels = nodes)
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        pair = list(sort(c(as.integer(from), as.integer(to)))),
        from = nodes[pair[[1]]],
        to   = nodes[pair[[2]]]
      ) |>
      dplyr::ungroup() |>
      dplyr::distinct(from, to, edge_type)
  }

  out <- tibble::new_tibble(
    dplyr::bind_rows(directed, symmetric),
    nrow  = nrow(directed) + nrow(symmetric),
    class = c("discography", "tbl_df", "tbl", "data.frame")
  )
  if (cpdag) {
    # convert to cpdag
    out |>
      mutate(
        idx_from = match(from, nodes),
        idx_to = match(to, nodes),
        node1 = if_else(idx_from < idx_to, from, to),
        node2 = if_else(idx_from < idx_to, to, from)
      ) |>
      group_by(node1, node2) |>
      summarise(
        edge_type = if (n() > 1 && all(edge_type == "-->")) {
          "---"
        } else {
          first(edge_type)
        },
        .groups = "drop"
      ) |>
      select(from = node1, to = node2, edge_type)
  } else {
    out
  }
}

mark_cpdag <- function(m_ij, m_ji, i, j) {
  # codes: 0 none | 1 arrowhead (row)
  if (m_ij == 0 && m_ji == 1) {
    list(from = i, to = j, type = "-->")
  } else if (m_ij == 1 && m_ji == 0) {
    list(from = j, to = i, type = "-->")
  } else if (m_ij == 1 && m_ji == 1) {
    list(from = i, to = j, type = "---") # undirected
  } else {
    NULL # no edge
  }
}

mark_pag <- function(m_ij, m_ji, i, j) {
  # codes: 0 none | 1 circle | 2 arrowhead | 3 tail   (column oriented)
  decode <- function(code) {
    switch(as.character(code),
      "1" = "circle",
      "2" = "arrow",
      "3" = "tail",
      "none"
    )
  }
  left <- decode(m_ji) # mark at i
  right <- decode(m_ij) # mark at j

  make <- function(f, t, tail, head) {
    type <- dplyr::case_when(
      tail == "tail" && head == "arrow" ~ "-->",
      tail == "tail" && head == "circle" ~ "--o",
      tail == "circle" && head == "arrow" ~ "o->",
      tail == "tail" && head == "tail" ~ "---",
      tail == "circle" && head == "circle" ~ "o-o",
      tail == "arrow" && head == "arrow" ~ "<->",
      TRUE ~ NA_character_
    )
    if (is.na(type)) NULL else list(from = f, to = t, type = type)
  }

  out <- make(i, j, left, right)
  if (!is.null(out)) {
    out
  } else {
    # try opposite orientation
    out <- make(j, i, right, left)
    out
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# methods
# ──────────────────────────────────────────────────────────────────────────────
#' Convert bnlearn object to a discography object.
#' @export
discography.bn <- function(x, nodes = names(x$nodes), ...) {
  arcs <- x$arcs |>
    tibble::as_tibble() |>
    rlang::set_names(c("from", "to"))

  # mark symmetric arcs as undirected
  undirected <-
    arcs |>
    dplyr::inner_join(arcs, by = c(from = "to", to = "from")) |>
    dplyr::distinct()

  directed <-
    arcs |>
    dplyr::anti_join(undirected, by = c("from", "to"))

  edges <-
    dplyr::bind_rows(
      directed |> dplyr::mutate(type = "-->"),
      undirected |> dplyr::mutate(type = "---")
    )

  as_tibble_edges(edges$from, edges$to, edges$type, nodes)
}

#' Convert graphNEL object to a discography object.
#' @export
discography.graphNEL <- function(x, nodes = graph::nodes(x), ...) {
  ig <- igraph::igraph.from.graphNEL(x)
  edge_df <- igraph::as_data_frame(ig, what = "edges")

  types <-
    if (igraph::is_directed(ig)) rep("-->", nrow(edge_df)) else rep("---", nrow(edge_df))
  cpdag <- igraph::is_directed(ig)
  as_tibble_edges(edge_df$from, edge_df$to, types, nodes, cpdag = cpdag)
}

#' Convert pcAlgo object to a discography object.
#' @export
discography.pcAlgo <- function(x, nodes = x@graph@nodes, ...) {
  discography(x@graph, nodes = nodes)
}

#' Convert gAlgo object to a discography object.
#' @export
discography.gAlgo <- function(x, nodes = x@graph@nodes, ...) {
  discography(x@graph, nodes = nodes)
}

#' #' Convert EssGraph object (used by pcalg::ges) to a discography object.
#' @export
discography.EssGraph <- function(x, nodes = x$.nodes, ...) {
  parents <- purrr::map2_dfr(
    seq_along(x$.in.edges), # numeric child-index
    x$.in.edges, # its list of parent-indices
    \(child_idx, parent_vec) {
      if (length(parent_vec) == 0L) {
        return(NULL)
      }

      tibble::tibble(
        from       = nodes[parent_vec],
        to         = nodes[child_idx],
        edge_type  = "-->"
      )
    }
  )

  as_tibble_edges(parents$from,
    parents$to,
    parents$edge_type,
    nodes,
    cpdag = TRUE
  )
}


#' Convert fciAlgo object to a discography object.
#' @export
discography.fciAlgo <- function(x, nodes = rownames(x@amat), ...) {
  amat <- methods::as(x, "amat")

  if (is.null(nodes)) {
    nodes <- rownames(amat)
    if (is.null(nodes)) nodes <- colnames(amat)
  }
  discography(amat, nodes = nodes, ...)
}

#' Convert amat object to a discography object.
#' @export
discography.amat <- function(x, nodes = NULL, ...) {
  if (inherits(x, "amat.pag")) {
    discography.amat.pag(x, nodes = nodes, ...)
  } else {
    discography.amat.cpdag(x, nodes = nodes, ...)
  }
}

#' Convert amat.pag object to a discography object.
#' @export
discography.amat.pag <- function(x, nodes = NULL, ...) {
  if (is.null(nodes)) {
    nodes <- rownames(x)
    if (is.null(nodes)) nodes <- colnames(x)
    if (is.null(nodes)) nodes <- paste0("V", seq_len(nrow(x)))
  }

  idx <- expand.grid(
    i = seq_len(nrow(x)),
    j = seq_len(ncol(x))
  )
  idx <- idx[idx$i < idx$j, ] # only one orientation per pair

  edges <- purrr::pmap_dfr(
    idx,
    function(i, j) {
      mark_pag(x[i, j], x[j, i], nodes[i], nodes[j])
    }
  )

  as_tibble_edges(edges$from, edges$to, edges$type, nodes)
}

#' Convert amat.cpdag object to a discography object.
#' @export
discography.amat.cpdag <- function(x, nodes = NULL, ...) {
  if (is.null(nodes)) {
    nodes <- rownames(x)
    if (is.null(nodes)) nodes <- colnames(x)
    if (is.null(nodes)) nodes <- paste0("V", seq_len(nrow(x)))
  }

  idx <- expand.grid(
    i = seq_len(nrow(x)),
    j = seq_len(ncol(x))
  )
  idx <- idx[idx$i < idx$j, ]

  edges <- purrr::pmap_dfr(
    idx,
    function(i, j) {
      mark_cpdag(x[i, j], x[j, i], nodes[i], nodes[j])
    }
  )

  as_tibble_edges(edges$from, edges$to, edges$type, nodes)
}

#' Convert tetrad_graph object to a discography object.
#' @export
discography.tetrad_graph <- function(x, nodes = x$nodes, ...) {
  discography(x$amat, nodes = nodes, ...)
}

#' Default method for discography
#'
#' If the input is not recognized, throw error.
#' @export
discography.default <- function(x, ...) {
  cli::cli_abort(c(
    "Don't know how to convert {.cls {class(x)[1]}} to a discography."
  ))
}

#' Print method for discography objects.
#' @export
print.discography <- function(x, ...) {
  NextMethod()
}
