#' @title TABU Algorithm for Causal Discovery
#'
#' @description
#' Run the TABU (Tabu search) algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"bnlearn"}}{\pkg{bnlearn} R package.}
#'   }
#' @param score Character; name of the scoring function to use.
#' @param ... Additional arguments passed to the chosen engine (e.g. score or algorithm parameters).
#'
#' @details
#' For specific details on the supported scores and parameters for each engine, see:
#' \itemize{
#'  \item [BnlearnSearch] for \pkg{bnlearn}.
#' }
#'
#' Note that this function **always returns a fully directed DAG**.
#' In cases where the data do not provide enough information to identify the direction
#' of some edges (i.e., edges that are part of a Markov equivalence class),
#' the algorithm will choose a direction arbitrarily.
#' Therefore, some edges in the returned DAG may not reflect the true causal direction.
#'
#' @example inst/roxygen-examples/tabu-example.R
#'
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_dag Returns
#'
# #' @family causal discovery algorithms
# #' @concept cd_algorithms
#' @keywords internal
#' @noRd
tabu <- function(
  engine = c("bnlearn"),
  score,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "tabu",
    engine = engine,
    engine_fns = list(
      bnlearn = function(...) {
        make_runner(engine = "bnlearn", alg = "tabu", ...)
      }
    ),
    score = score,
    graph_class = "PDAG",
    ...
  )
}
