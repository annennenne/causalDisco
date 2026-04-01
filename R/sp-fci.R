#' @title SP-FCI Algorithm for Causal Discovery
#'
#' @description
#' Run the Sparsest Permutation–based Fast Causal Inference algorithm for causal discovery using
#' one of several engines.
#' Can be computationally intensive.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'   }
#' @inheritParams grasp
#'
#' @details
#' For specific details on the supported scores, and parameters for each engine, see:
#' \itemize{
#'  \item [TetradSearch] for \pkg{Tetrad}.
#' }
#'
#' @example inst/roxygen-examples/boss-fci-example.R
#'
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
sp_fci <- function(
  engine = "tetrad",
  score,
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)
  make_method(
    method_name = "sp_fci",
    engine = engine,
    engine_fns = list(
      tetrad = function(...) {
        make_runner(engine = "tetrad", alg = "sp_fci", ...)
      }
    ),
    test = test,
    alpha = alpha,
    score = score,
    graph_class = "PAG",
    ...
  )
}
