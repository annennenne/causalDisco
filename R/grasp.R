#' @title GRaSP Algorithm for Causal Discovery
#'
#' @description
#' Run the Greedy Relaxations of the Sparsest Permutation algorithm for
#' causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'   }
#' @param score Character; name of the scoring function to use.
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine (e.g. score and
#' algorithm parameters).
#'
#' @details
#' For specific details on the supported scores, and parameters for each engine, see:
#' \itemize{
#'  \item [TetradSearch] for \pkg{Tetrad}.
#' }
#'
#' @example inst/roxygen-examples/grasp-example.R
#'
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pdag Value
#' @references
#' Lam, W.-Y., Andrews, B., & Ramsey, J. (2022). Greedy Relaxations of the Sparsest Permutation Algorithm.
#' In The 38th Conference on Uncertainty in Artificial Intelligence.
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
grasp <- function(
  engine = "tetrad",
  score,
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "grasp",
    engine = engine,
    engine_fns = list(
      tetrad = function(...) {
        make_runner(engine = "tetrad", alg = "grasp", ...)
      }
    ),
    score = score,
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}
