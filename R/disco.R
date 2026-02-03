#' @title Disco!!
#'
#' @description
#' Run a causal discovery method on a data frame.
#'
#' @param data A data frame.
#' @param method A `disco_method` object representing a causal discovery
#' algorithm. Available methods are
#' \itemize{
#'  \item [pc()] - PC algorithm,
#'  \item [fci()] - FCI algorithm,
#'  \item [ges()] - GES algorithm,
#'  \item [tges()] - TGES algorithm,
#'  \item [tpc()] - TPC algorithm.
#' }
#' @param knowledge A `knowledge` object to be incorporated into the disco method. If `NULL` (default), the method is
#'   applied without additional knowledge.
#'
#' @details
#' For specific details on the supported algorithms, scores, tests, and parameters for each engine, see:
#' \itemize{
#'  \item \code{\link{TetradSearch}} for \pkg{Tetrad},
#'  \item \code{\link{PcalgSearch}} for \pkg{pcalg},
#'  \item \code{\link{BnlearnSearch}} for \pkg{bnlearn},
#'  \item \code{\link{CausalDiscoSearch}} for \pkg{causalDisco}.
#' }
#'
#' @example inst/roxygen-examples/disco-example.R
#'
#' @returns A `caugi` and a `knowledge` (`knowledgeable_caugi`) object.
#'
#' @export
disco <- function(data, method, knowledge = NULL) {
  engine <- attr(method, "engine")
  graph_class <- attr(method, "graph_class")

  if (is.null(graph_class)) {
    graph_class <- "UNKNOWN"
  }

  if (graph_class == "PAG") {
    # Caugi currently does not support PAGs
    graph_class <- "UNKNOWN"
  }

  if (!inherits(method, "disco_method")) {
    stop("The method must be a disco method object.", call. = FALSE)
  }

  if (engine == "causalDisco" && any(knowledge$edges$status == "required")) {
    warning(
      "causalDisco engine does not support required edges in knowledge. ",
      "These will be ignored.",
      call. = FALSE
    )
  }

  # inject knowledge via S3 generic
  if (!is.null(knowledge)) {
    is_knowledge(knowledge)
    tryCatch(
      {
        method <- set_knowledge(method, knowledge)
      },
      error = function(e) {
        # extra precaution to catch errors in setting knowledge
        stop("Error in setting knowledge: ", e$message, call. = FALSE) # nocov
      }
    )
  }
  out <- method(data)

  if (!is.null(out$caugi)) {
    out$caugi <- tryCatch(
      {
        caugi::mutate_caugi(out$caugi, graph_class)
      },
      error = function(e) {
        cycle_msg <- ""
        if (identical(graph_class, "PDAG")) {
          cycle_msg <- " The graph contains a directed cycle."
        }
        warning(
          sprintf(
            "Cannot mutate graph to class '%s'.%s",
            graph_class,
            cycle_msg
          ),
          call. = FALSE
        )
        out$caugi
      }
    )
  }

  if (!is.null(knowledge)) {
    out <- set_knowledge(out, knowledge)
  }

  out
}
