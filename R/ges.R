#' @title GES Algorithm for Causal Discovery
#'
#' @description
#' Run the Greedy Equivalent Search algorithm for causal discovery using
#' one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'     \item{\code{"pcalg"}}{\pkg{pcalg} R package.}
#'   }
#' @param score Character; name of the scoring function to use.
#' @param ... Additional arguments passed to the chosen engine (e.g. score and
#' algorithm parameters).
#'
#' @details
#' For specific details on the supported scores, and parameters for each engine, see:
#' \itemize{
#'  \item [TetradSearch] for \pkg{Tetrad} (note, Tetrad refers to it as "fges"),
#'  \item [PcalgSearch] for \pkg{pcalg}.
#' }
#'
#' @example inst/roxygen-examples/ges-example.R
#' @references
#' Chickering, D. M. (2002). Optimal structure identification with greedy search.
#' Journal of Machine Learning Research 3, 507-554.
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pdag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
ges <- function(
  engine = c("tetrad", "pcalg"),
  score,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "ges",
    engine = engine,
    engine_fns = list(
      pcalg = function(...) make_runner(engine = "pcalg", alg = "ges", ...),
      # Note: Tetrad refers to GES as "fges" (fast GES), but it's the same algorithm
      tetrad = function(...) make_runner(engine = "tetrad", alg = "fges", ...)
    ),
    score = score,
    graph_class = "PDAG",
    ...
  )
}
