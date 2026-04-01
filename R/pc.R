#' @title PC Algorithm for Causal Discovery
#'
#' @description
#' Run the Peter-Clark algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'     \item{\code{"pcalg"}}{\pkg{pcalg} R package.}
#'     \item{\code{"bnlearn"}}{\pkg{bnlearn} R package.}
#'   }
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine (e.g. test or algorithm parameters).
#'
#' @details
#' For specific details on the supported tests and parameters for each engine, see:
#' \itemize{
#'  \item [TetradSearch] for \pkg{Tetrad},
#'  \item [PcalgSearch] for \pkg{pcalg},
#'  \item [BnlearnSearch] for \pkg{bnlearn}.
#' }
#'
#' @example inst/roxygen-examples/pc-example.R
#' @references
#' Spirtes P, Glymour C, and Scheines R. Causation, Prediction, and Search. MIT Press, 2000.
#'
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pdag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
pc <- function(
  engine = c("tetrad", "pcalg", "bnlearn", "causalDisco"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "pc",
    engine = engine,
    engine_fns = list(
      pcalg = function(...) make_runner(engine = "pcalg", alg = "pc", ...),
      tetrad = function(...) make_runner(engine = "tetrad", alg = "pc", ...),
      bnlearn = function(...) make_runner(engine = "bnlearn", alg = "pc", ...),
      causalDisco = function(...) {
        make_runner(engine = "causalDisco", alg = "pc", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}
