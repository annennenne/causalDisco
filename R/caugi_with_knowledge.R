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
#' @param nodes Optional character vector all the node names.
#'
#' @returns A `caugi` and a `knowledge` object in a list.
#'
#' @seealso \code{\link[caugi:caugi]{caugi::caugi()}}
#' @export
knowledgeable_caugi <- function(graph, kn = knowledge()) {
  UseMethod("knowledgeable_caugi")
}

#' @rdname knowledgeable_caugi
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
    class = c("knowledgeable_caugi", "list")
  )
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.default <- function(graph, kn = knowledge()) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  if (caugi::is_caugi(graph)) {
    cg <- graph
  } else {
    cg <- caugi::as_caugi(graph, collapse = TRUE, class = "PDAG")
  }
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.pcAlgo <- function(graph, kn = knowledge()) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  cg <- caugi::as_caugi(graph@graph, collapse = TRUE, class = "PDAG")
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.fciAlgo <- function(graph, kn = knowledge()) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  amat <- methods::as(graph, "matrix")
  cg <- caugi::as_caugi(amat, class = "PAG")
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.tetrad_graph <- function(graph, kn = knowledge()) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  cg <- caugi::as_caugi(graph$amat, collapse = TRUE, class = "PDAG")
  new_knowledgeable_caugi(cg, kn)
}

#' @inheritParams knowledgeable_caugi
#' @export
knowledgeable_caugi.EssGraph <- function(graph, kn = knowledge()) {
  if (!is_knowledge(kn)) {
    stop("`kn` must be a knowledge object.", call. = FALSE)
  }
  parents <- purrr::map2_dfr(
    seq_along(graph$.in.edges), # numeric child-index
    graph$.in.edges, # its list of parent-indices
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
    cg <- caugi(
      nodes = graph$.nodes,
      class = "PDAG"
    )
  } else {
    cg <- caugi::caugi(
      from = lst$from,
      edge = lst$edges,
      to = lst$to,
      nodes = graph$.nodes,
      class = "PDAG"
    )
  }
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
