#' @title disco Object
#'
#' @description
#' This S3 class wraps `caugi` graph object and a `knowledge` object. It is the
#' output object of causal discovery methods used in \pkg{causalDisco}.
#'
#' @details
#' The conversion from any graph type to a `caugi` is handled by the \pkg{caugi}
#' package.
#'
#' @param graph A causal graph object
#' @param kn A `knowledge` object. Default is empty knowledge object.
#' @param class A string describing the graph class.
#'
#' @returns A `disco` object containg a `caugi` and a `knowledge` object in a list.
#'
#' @seealso [caugi::caugi()]
#' @keywords internal
#' @noRd
as_disco <- function(graph, kn = knowledge(), class = "PDAG") {
  UseMethod("as_disco")
}

# delegate field names used by `knowledge` methods
.knowledge_fields <- c("vars", "tiers", "edges", "frozen")

#' @title Create a disco Object
#'
#' @param cg A `caugi` object
#' @param kn A `knowledge` object
#' @returns A `disco` object containing the `caugi` and `knowledge` objects.
#' @keywords internal
#' @noRd
new_disco <- function(cg, kn) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  caugi::is_caugi(cg, throw_error = TRUE)
  structure(
    list(
      caugi = cg,
      knowledge = kn
    ),
    class = "disco"
  )
}

#' @inheritParams as_disco
#' @export
as_disco.default <- function(
  graph,
  kn = knowledge(),
  class = "PDAG"
) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  if (caugi::is_caugi(graph)) {
    cg <- graph
  } else {
    cg <- caugi::as_caugi(graph, collapse = TRUE, class = class)
  }
  new_disco(cg, kn)
}

#' @inheritParams as_disco
#' @export
as_disco.pcAlgo <- function(
  graph,
  kn = knowledge(),
  class = "PDAG"
) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  cg <- caugi::as_caugi(graph@graph, collapse = TRUE, class = class)
  new_disco(cg, kn)
}

#' @inheritParams as_disco
#' @export
as_disco.fciAlgo <- function(
  graph,
  kn = knowledge(),
  class = "PAG"
) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  amat <- methods::as(graph, "matrix")
  cg <- caugi::as_caugi(amat, class = class)
  new_disco(cg, kn)
}

#' @inheritParams as_disco
#' @export
as_disco.tetrad_graph <- function(
  graph,
  kn = knowledge(),
  class = "PDAG"
) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  cg <- caugi::as_caugi(graph$amat, collapse = TRUE, class)
  new_disco(cg, kn)
}

#' @inheritParams as_disco
#' @importFrom rlang .data
#' @export
as_disco.EssGraph <- function(
  graph,
  kn = knowledge(),
  class = "PDAG"
) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  nodes <- graph$.nodes

  edges <- purrr::map2_dfr(
    seq_along(graph$.in.edges),
    graph$.in.edges,
    \(child_idx, parent_vec) {
      if (length(parent_vec) == 0L) {
        return(tibble::tibble(
          from = character(),
          to = character(),
          edge = character()
        ))
      }
      tibble::tibble(
        from = nodes[parent_vec],
        to = rep(nodes[child_idx], length(parent_vec)),
        edge = rep("-->", length(parent_vec))
      )
    }
  )

  if (nrow(edges) == 0L) {
    cg <- caugi::caugi(nodes = nodes, class = "PDAG")
    return(new_disco(cg, kn))
  }

  collapsed <- edges |>
    dplyr::mutate(
      canon_from = pmin(.data$from, .data$to),
      canon_to = pmax(.data$from, .data$to)
    ) |>
    dplyr::group_by(.data$canon_from, .data$canon_to) |>
    dplyr::summarise(
      has_fw = any(
        .data$from == .data$canon_from & .data$to == .data$canon_to
      ),
      has_bw = any(
        .data$from == .data$canon_to & .data$to == .data$canon_from
      ),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      from = dplyr::case_when(
        has_fw & has_bw ~ canon_from,
        has_fw ~ canon_from,
        TRUE ~ canon_to
      ),
      to = dplyr::case_when(
        has_fw & has_bw ~ canon_to,
        has_fw ~ canon_to,
        TRUE ~ canon_from
      ),
      edge = dplyr::if_else(.data$has_fw & .data$has_bw, "---", "-->")
    )

  cg <- caugi::caugi(
    from = collapsed$from,
    edge = collapsed$edge,
    to = collapsed$to,
    nodes = nodes,
    class = class
  )
  new_disco(cg, kn)
}

#' @title Print a disco Object
#' @param x A `disco` object.
#' @inheritParams print.knowledge
#' @returns Invisibly returns the `disco` object.
#' @examples
#' data(tpc_example)
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#' cd_tges <- tpc(engine = "causalDisco", test = "fisher_z")
#' disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
#' print(disco_cd_tges)
#' print(disco_cd_tges, wide_vars = TRUE)
#' print(disco_cd_tges, compact = TRUE)
#'
#' @exportS3Method print disco
print.disco <- function(
  x,
  compact = FALSE,
  wide_vars = FALSE,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c("cli", "tibble"),
    function_name = "print.disco"
  )

  cli::cli_h1("caugi graph")

  # Graph info
  graph_class <- x$caugi@graph_class

  cli::cli_text("Graph class: {.strong {graph_class}}")

  cg <- x$caugi
  if (compact) {
    cli::cli_text("{nrow(edges(cg))} edges, {nrow(nodes(cg))} nodes")
  } else {
    print_section("Edges", edges(cg))
    print_section("Nodes", nodes(cg))
  }

  # Knowledge info
  print.knowledge(x$knowledge, compact = compact, wide_vars = wide_vars, ...)

  invisible(x)
}

#' @title Summarize a disco Object
#' @param object A `disco` object.
#' @param ... Additional arguments (not used).
#' @returns Invisibly returns the `disco` object.
#' @examples
#' data(tpc_example)
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#' cd_tges <- tpc(engine = "causalDisco", test = "fisher_z")
#' disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
#' summary(disco_cd_tges)
#'
#' @exportS3Method summary disco
summary.disco <- function(object, ...) {
  cg <- object$caugi
  # Graph info
  cli::cli_h1("caugi graph summary")
  cli::cli_text("Graph class: {.strong {cg@graph_class}}")
  cli::cli_text("Nodes: {.strong {nrow(nodes(cg))}}")
  cli::cli_text("Edges: {.strong {nrow(edges(cg))}}")

  # Knowledge info
  summary.knowledge(object$knowledge, ...)

  invisible(object)
}

#' @export
set_knowledge.disco <- function(method, knowledge) {
  if (!is_knowledge(knowledge)) {
    stop("The knowledge must be a knowledge object.", call. = FALSE)
  }
  method$knowledge <- knowledge
  method
}

#' @title Extract Knowledge from a disco Object
#'
#' @description
#' S3 method to extract the `knowledge` object from a `disco`.
#'
#' @param x A `disco` object.
#'
#' @return The nested `knowledge` object.
#'
#' @keywords internal
#' @noRd
knowledge.disco <- function(x) {
  x$knowledge
}

#' @title Is it a `disco`?
#'
#' @param x An object
#'
#' @returns `TRUE` if the object is of class `disco`, `FALSE` otherwise.
#' @keywords internal
#' @noRd
is_disco <- function(x) {
  inherits(x, "disco")
}


# delegate accessors so `knowledge` verbs operate on the nested object

#' @export
`$.disco` <- function(x, name) {
  ux <- unclass(x)
  if (name %in% names(ux)) {
    return(ux[[name]])
  }
  if (name %in% .knowledge_fields) {
    return(ux$knowledge[[name]])
  }
  NULL
}

#' @export
`$<-.disco` <- function(x, name, value) {
  ux <- unclass(x)
  if (name %in% names(ux) && !(name %in% .knowledge_fields)) {
    ux[[name]] <- value
    x <- ux
  } else if (name %in% .knowledge_fields) {
    ux$knowledge[[name]] <- value
    x <- ux
  } else {
    ux[[name]] <- value
    x <- ux
  }
  class(x) <- "disco"
  x
}

#' @export
`[[.disco` <- function(x, name, ...) {
  ux <- unclass(x)
  if (is.character(name)) {
    if (name %in% names(ux)) {
      return(ux[[name]])
    }
    if (name %in% .knowledge_fields) {
      return(ux$knowledge[[name]])
    }
  }
  ux[[name, ...]]
}

#' @export
`[[<-.disco` <- function(x, name, value) {
  ux <- unclass(x)
  if (is.character(name) && (name %in% .knowledge_fields)) {
    ux$knowledge[[name]] <- value
    x <- ux
  } else {
    ux[[name]] <- value
    x <- ux
  }
  class(x) <- "disco"
  x
}
