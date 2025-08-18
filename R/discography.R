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
#'  If `NULL`, the unique node names are sorted alphabetically.
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

#' Build a discography tibble from edge vectors
#'
#' Create a correctly classed tibble from parallel `from`, `to`, and `type`
#' vectors, normalising symmetric edges and optionally converting bidirected
#' pairs into undirected edges for CPDAGs.
#'
#' @param from,to,type Character vectors of equal length.
#' @param nodes Optional character vector of node order used for normalisation
#'  and final sorting.
#' @param cpdag Logical. If `TRUE`, opposite directed edges are collapsed
#'  into undirected `"---"` edges.
#' @return A tibble of class `"discography"`.
#' @keywords internal
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
    nrow = nrow(directed) + nrow(symmetric)
  )
  if (cpdag) {
    # convert to cpdag
    undirected <- dplyr::inner_join(out, out,
      by = c("from" = "to", "to" = "from")
    ) |>
      # put the smaller-index node first so each pair is unique
      dplyr::rowwise() |>
      dplyr::mutate(
        node1 = pmin(from, to),
        node2 = pmax(from, to)
      ) |>
      dplyr::ungroup() |>
      dplyr::distinct(node1, node2) |>
      dplyr::mutate(edge_type = "---") |>
      dplyr::select(from = node1, to = node2, edge_type)

    ## 3. keep all the single arrows that were *not* bidirectional
    out <- out |>
      dplyr::anti_join(undirected, by = c("from", "to")) |>
      dplyr::anti_join(undirected, by = c("from" = "to", "to" = "from")) |>
      dplyr::bind_rows(undirected)
  }
  nrows <- nrow(out)
  out |>
    dplyr::arrange(
      factor(from, levels = nodes),
      factor(to, levels = nodes)
    ) |>
    tibble::new_tibble(
      nrow  = nrows,
      class = c("discography", "tbl_df", "tbl", "data.frame")
    )
}
#' Mark a CPDAG edge from pairwise marks
#'
#' Translate two binary marks into a directed or undirected edge for CPDAGs.
#'
#' @param m_ij,m_ji Integers (`0` = none, `1` = arrowhead).
#' @param i,j Node names corresponding to matrix indices.
#'
#' @return A list with `from`, `to`, and `type`, or `NULL` if no edge.
#' @keywords internal
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

#' Mark a PAG edge from pairwise marks
#'
#' Translate two sided marks into a PAG edge, considering circle/arrow/tail.
#'
#' @param m_ij,m_ji Integers (`0` = none, `1` = circle, `2` = arrowhead, `3` = tail).
#' @param i,j Node names corresponding to matrix indices.
#'
#' @return A list with `from`, `to`, and `type`, or `NULL` if no valid edge.
#' @keywords internal
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

#' Minimal constructor for discography objects
#'
#' @param x A tibble (or data.frame) containing at least
#'          `from`, `to`, and `edge_type` columns.
#'
#' @return A tibble with class `c("discography", "tbl_df", "tbl", "data.frame")`.
#' @export
new_discography <- function(x) {
  x <- tibble::as_tibble(x)

  required <- c("from", "to", "edge_type")
  if (!all(required %in% names(x))) {
    cli::cli_abort(
      "Input must contain {.code from}, {.code to}, and {.code edge_type} columns."
    )
  }

  # ensure the three key columns are characters and come first
  x <- x |>
    dplyr::select(dplyr::all_of(required), dplyr::everything()) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(required), as.character)
    )

  tibble::new_tibble(
    x,
    nrow  = nrow(x),
    class = "discography"
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# methods
# ──────────────────────────────────────────────────────────────────────────────
#' Convert a bnlearn network to a discography tibble
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
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
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @export
discography.graphNEL <- function(x, nodes = graph::nodes(x), ...) {
  ig <- igraph::graph_from_graphnel(x)
  edge_df <- igraph::as_data_frame(ig, what = "edges")

  types <-
    if (igraph::is_directed(ig)) rep("-->", nrow(edge_df)) else rep("---", nrow(edge_df))
  cpdag <- igraph::is_directed(ig)
  as_tibble_edges(edge_df$from, edge_df$to, types, nodes, cpdag = cpdag)
}

#' Convert pcAlgo object to a discography object.
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @export
discography.pcAlgo <- function(x, nodes = x@graph@nodes, ...) {
  discography(x@graph, nodes = nodes)
}

#' #' Convert EssGraph object (used by pcalg::ges) to a discography object.
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @importFrom purrr map2_dfr
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
  if (nrow(parents) == 0L) {
    return(as_tibble_edges(character(), character(), character(),
      nodes,
      cpdag = TRUE
    ))
  }

  as_tibble_edges(parents$from,
    parents$to,
    parents$edge_type,
    nodes,
    cpdag = TRUE
  )
}


#' Convert fciAlgo object to a discography object.
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @export
discography.fciAlgo <- function(x, nodes = rownames(x@amat), ...) {
  amat <- methods::as(x, "amat")
  discography(amat, nodes = nodes, ...)
}

#' Convert amat object to a discography object.
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @export
discography.amat <- function(x, nodes = NULL, ...) {
  if (inherits(x, "amat.pag")) {
    discography.amat.pag(x, nodes = nodes, ...)
  } else {
    discography.amat.cpdag(x, nodes = nodes, ...)
  }
}

#' Convert amat.pag object to a discography object.
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @importFrom purrr pmap_dfr
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

  if (nrow(edges) == 0L) {
    return(as_tibble_edges(character(), character(), character(), nodes))
  }

  as_tibble_edges(edges$from, edges$to, edges$type, nodes)
}

#' Convert amat.cpdag object to a discography object.
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @importFrom purrr pmap_dfr
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

  if (nrow(edges) == 0L) {
    return(as_tibble_edges(character(), character(), character(), nodes))
  }

  as_tibble_edges(edges$from, edges$to, edges$type, nodes)
}

#' Convert tetrad_graph object to a discography object.
#'
#' @inheritParams discography
#' @return A `"discography"` tibble.
#' @export
discography.tetrad_graph <- function(x, nodes = x$nodes, ...) {
  discography(x$amat, nodes = nodes, ...)
}

#' Convert a temporal adjacency matrix (tamat) to a discography tibble
#'
#' @inheritParams discography
#' @return A "discography" tibble.
#' @export
discography.tamat <- function(x, nodes = NULL, ...) {
  type <- tolower(attr(x, "tamat_type", exact = TRUE) %||% "pdag")

  if (is.null(nodes)) {
    nodes <- rownames(x)
    if (is.null(nodes)) nodes <- colnames(x)
    if (is.null(nodes)) nodes <- paste0("V", seq_len(nrow(x)))
  }

  if (type %in% c("pag", "ag")) {
    class(x) <- c("amat.pag", "matrix")
    return(discography.amat.pag(x, nodes = nodes, ...))
  }

  if (type == "pdag") {
    class(x) <- c("amat.cpdag", "matrix")
    return(discography.amat.cpdag(x, nodes = nodes, ...))
  }

  stop(
    "Unknown `tamat_type`: ", type,
    ". Expected '`pdag`' or '`ag`'."
  )
}

#' Default method for discography
#'
#' If the input is not recognized, throw error.
#' @param x An unsupported object.
#' @param ... Unused, passed for extensibility.
#'
#' @return Never returns; errors.
#' @export
discography.default <- function(x, ...) {
  cli::cli_abort(c(
    "Don't know how to convert {.cls {class(x)[1]}} to a discography."
  ))
}

# nocov start

#' Print method for discography objects.
#'
#' @param x A `discography` object.
#' @param ... Additional arguments.
#' @export
print.discography <- function(x, ...) {
  NextMethod()
}
# nocov end
