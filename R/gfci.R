#' @title GFCI Algorithm for Causal Discovery
#'
#' @description
#' Run the Greedy Fast Causal Inference algorithm for causal discovery using
#' one of several engines.
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
#' @example inst/roxygen-examples/gfci-example.R
#' @references
#' Ogarrio, J. M., Spirtes, P., and Ramsey, J. (2016). A hybrid causal search algorithm
#' for latent variable models. In Conference on probabilistic graphical models, pages 368–379.
#' PMLR.
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
gfci <- function(
  engine = "tetrad",
  score,
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "gfci",
    engine = engine,
    engine_fns = list(
      tetrad = function(...) make_runner(engine = "tetrad", alg = "gfci", ...)
    ),
    score = score,
    test = test,
    alpha = alpha,
    graph_class = "PAG",
    ...
  )
}
