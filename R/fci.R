#' @title FCI Algorithm for Causal Discovery
#'
#' @description
#' Run the Fast Causal Inference algorithm for causal discovery using one of
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
#' @example inst/roxygen-examples/fci-example.R
#' @references
#' Spirtes, P., Meek, C., & Richardson, T. (1995, August). Causal inference in
#' the presence of latent variables and selection bias. In Proceedings of
#' the Eleventh conference on Uncertainty in artificial intelligence
#' (pp. 499-506).
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
fci <- function(
  engine = c("tetrad", "pcalg"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "fci",
    engine = engine,
    engine_fns = list(
      pcalg = function(...) make_runner(engine = "pcalg", alg = "fci", ...),
      tetrad = function(...) make_runner(engine = "tetrad", alg = "fci", ...)
    ),
    test = test,
    alpha = alpha,
    graph_class = "PAG",
    ...
  )
}
