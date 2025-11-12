#' @title A `caugi` with an attached `knowledge` object
#'
#' @description
#' This S3 class wraps `caugi` graph object and a `knowledge` object. It is the
#' output object of causal discovery methods used in `causalDisco`.
#'
#' @details
#' The conversion from any graph type to a `caugi` is handled by the `caugi`
#' package.
#'
#' @param graph A causal graph object
#' @param kn A `knowledge` object. Default is empty knowledge object.
#' @param class A string describing the graph class.
#'
#' @returns A `caugi` and a `knowledge` object in a list.
#'
#' @seealso \code{\link[caugi:caugi]{caugi::caugi()}}
#' @export
knowledgeable_caugi <- function(graph, kn = knowledge(), class = "PDAG") {
  UseMethod("knowledgeable_caugi")
}

# delegate field names used by `knowledge` methods
.knowledge_fields <- c("vars", "tiers", "edges", "frozen")

#' @title Create a new `knowledgeable_caugi` object
#'
#' @param cg A `caugi` object
#' @param kn A `knowledge` object
#' @export
new_knowledgeable_caugi <- function(cg, kn) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  caugi::is_caugi(cg, throw_error = TRUE)
  structure(
    list(
      caugi = cg,
      knowledge = kn
    ),
    class = c("knowledgeable_caugi", "knowledge")
  )
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.default <- function(graph, kn = knowledge(), class = "PDAG") {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  if (caugi::is_caugi(graph)) {
    cg <- graph
  } else {
    cg <- caugi::as_caugi(graph, collapse = TRUE, class = class)
  }
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.pcAlgo <- function(graph, kn = knowledge(), class = "PDAG") {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  cg <- caugi::as_caugi(graph@graph, collapse = TRUE, class = class)
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.fciAlgo <- function(graph, kn = knowledge(), class = "PAG") {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  amat <- methods::as(graph, "matrix")
  cg <- caugi::as_caugi(amat, class = class)
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.tetrad_graph <- function(graph, kn = knowledge(), class = "PDAG") {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  cg <- caugi::as_caugi(graph$amat, collapse = TRUE, class)
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.EssGraph <- function(graph, kn = knowledge(), class = "PDAG") {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  nodes <- graph$.nodes

  edges <- purrr::map2_dfr(
    seq_along(graph$.in.edges),
    graph$.in.edges,
    \(child_idx, parent_vec) {
      if (length(parent_vec) == 0L) {
        return(tibble::tibble(from = character(), to = character(), edge = character()))
      }
      tibble::tibble(
        from = nodes[parent_vec],
        to   = rep(nodes[child_idx], length(parent_vec)),
        edge = rep("-->", length(parent_vec))
      )
    }
  )

  if (nrow(edges) == 0L) {
    cg <- caugi::caugi(nodes = nodes, class = "PDAG")
    return(new_knowledgeable_caugi(cg, kn))
  }

  collapsed <- edges |>
    dplyr::mutate(
      canon_from = pmin(from, to),
      canon_to   = pmax(from, to)
    ) |>
    dplyr::group_by(canon_from, canon_to) |>
    dplyr::summarise(
      has_fw = any(from == canon_from & to == canon_to),
      has_bw = any(from == canon_to & to == canon_from),
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
      edge = dplyr::if_else(has_fw & has_bw, "---", "-->")
    )

  cg <- caugi::caugi(
    from = collapsed$from,
    edge = collapsed$edge,
    to = collapsed$to,
    nodes = nodes,
    class = class
  )
  new_knowledgeable_caugi(cg, kn)
}

#' @export
set_knowledge.knowledgeable_caugi <- function(method, knowledge) {
  if (!is_knowledge(knowledge)) {
    stop("The knowledge must be a knowledge object.", call. = FALSE)
  }
  method$knowledge <- knowledge
  method
}

#' @export
knowledge.knowledgeable_caugi <- function(x) {
  x$knowledge
}

#' @title Is it a `knowledgeable_caugi`?
#'
#' @param x An object
#'
#' @returns `TRUE` if the object is of class `knowledgeable_caugi`, `FALSE` otherwise.
#' @export
is_knowledgeable_caugi <- function(x) {
  inherits(x, "knowledgeable_caugi")
}

# delegate accessors so `knowledge` verbs operate on the nested object

#' @export
`$.knowledgeable_caugi` <- function(x, name) {
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
`$<-.knowledgeable_caugi` <- function(x, name, value) {
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
  class(x) <- c("knowledgeable_caugi", "knowledge")
  x
}

#' @export
`[[.knowledgeable_caugi` <- function(x, name, ...) {
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
`[[<-.knowledgeable_caugi` <- function(x, name, value) {
  ux <- unclass(x)
  if (is.character(name) && (name %in% .knowledge_fields)) {
    ux$knowledge[[name]] <- value
    x <- ux
  } else {
    ux[[name]] <- value
    x <- ux
  }
  class(x) <- c("knowledgeable_caugi", "knowledge")
  x
}
