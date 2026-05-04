#' @title RFCI Algorithm for Causal Discovery
#'
#' @description
#' Run the Really Fast Causal Inference algorithm for causal discovery using one of
#' several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'     \item{\code{"pcalg"}}{\pkg{pcalg} R package.}
#'   }
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine
#' (e.g. test or algorithm parameters).
#'
#' @details
#' For specific details on the supported tests and parameters for each engine, see:
#' \itemize{
#'  \item [TetradSearch] for \pkg{Tetrad},
#'  \item [PcalgSearch] for \pkg{pcalg}.
#' }
#'
#' @example inst/roxygen-examples/rfci-example.R
#' @references
#' Colombo, D., Maathuis, M. H., Kalisch, M., & Richardson, T. S. (2012).
#' Learning high-dimensional directed acyclic graphs with latent and selection variables.
#' The Annals of Statistics, 294-321.
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_rfci_pag Value
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
rfci <- function(
  engine = c("tetrad", "pcalg"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "rfci",
    engine = engine,
    engine_fns = list(
      pcalg = function(...) make_runner(engine = "pcalg", alg = "rfci", ...),
      tetrad = function(...) make_runner(engine = "tetrad", alg = "rfci", ...)
    ),
    test = test,
    alpha = alpha,
    graph_class = "RFCI-PAG",
    ...
  )
}
