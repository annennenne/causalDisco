#' @title TGES Algorithm for Causal Discovery
#'
#' @description
#' Run the Temporal Greedy Equivalent Search algorithm for causal discovery using one of
#' several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"causalDisco"}}{causalDisco library.}
#'   }
#' @param score Character; name of the scoring function to use.
#' @param ... Additional arguments passed to the chosen engine
#' (e.g. test or algorithm parameters).
#'
#' @details
#' For specific details on the supported scores, see [CausalDiscoSearch]. For additional parameters
#' passed via \code{...}, see [tges_run()].
#'
#' @example inst/roxygen-examples/tges-example.R
#'
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pdag Value
#' @references
#' Larsen TE, Ekstrøm CT, and Petersen AH. Score-Based Causal Discovery with Temporal
#' Background Information, 2025. <doi:10.48550/arXiv.2502.06232>.
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
tges <- function(
  engine = c("causalDisco"),
  score,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "tges",
    engine = engine,
    engine_fns = list(
      causalDisco = function(...) {
        make_runner(engine = "causalDisco", alg = "tges", ...)
      }
    ),
    score = score,
    graph_class = "PDAG",
    ...
  )
}
