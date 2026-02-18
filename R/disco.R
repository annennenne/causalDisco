#' @title Perform Causal Discovery
#'
#' @description
#' Apply a causal discovery method to a data frame to infer causal relationships on observational data.
#' Supports multiple algorithms and optionally incorporates prior knowledge.
#'
#' @param data A data frame.
#' @param method A `disco_method` object representing a causal discovery
#' algorithm. Available methods are
#' \itemize{
#'  \item [boss()] - BOSS algorithm,
#'  \item [boss_fci()] - BOSS-FCI algorithm,
#'  \item [fci()] - FCI algorithm,
#'  \item [gfci()] - GFCI algorithm,
#'  \item [ges()] - GES algorithm,
#'  \item [grasp()] - GRaSP algorithm,
#'  \item [grasp_fci()] - GRaSP-FCI algorithm,
#'  \item [gs()] - GS algorithm,
#'  \item [iamb()], [iamb_fdr()], [fast_iamb()], [inter_iamb()] - IAMB algorithms,
#'  \item [pc()] - PC algorithm,
#'  \item [sp_fci()] - SP-FCI algorithm,
#'  \item [tfci()] - TFCI algorithm,
#'  \item [tges()] - TGES algorithm,
#'  \item [tpc()] - TPC algorithm.
#' }
#' @param knowledge A `knowledge` object to be incorporated into the disco method. If `NULL` (default), the method is
#'   applied without additional knowledge.
#'
#' @details
#' For specific details on the supported algorithms, scores, tests, and parameters for each engine, see:
#' \itemize{
#'  \item [BnlearnSearch] for \pkg{bnlearn},
#'  \item [CausalDiscoSearch] for \pkg{causalDisco},
#'  \item [PcalgSearch] for \pkg{pcalg},
#'  \item [TetradSearch] for \pkg{Tetrad}.
#' }
#'
#' @example inst/roxygen-examples/disco-example.R
#'
#' @returns A `caugi` and a `knowledge` (`disco`) object.
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
