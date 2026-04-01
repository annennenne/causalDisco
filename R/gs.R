#' @title GS Algorithm for Causal Discovery
#'
#' @description
#' Run the Grow-Shrink algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"bnlearn"}}{\pkg{bnlearn} R package.}
#'   }
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine (e.g. test or algorithm parameters).
#'
#' @details
#' For specific details on the supported tests and parameters for each engine, see:
#' \itemize{
#'  \item [BnlearnSearch] for \pkg{bnlearn}.
#' }
#'
#' @example inst/roxygen-examples/gs-example.R
#' @references
#' Margaritis, D., Thrun, S.: Bayesian network induction via local neighborhoods.
#' Tech. rep., DTIC Document (2000).
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pdag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
gs <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "gs",
    engine = engine,
    engine_fns = list(
      bnlearn = function(...) {
        make_runner(engine = "bnlearn", alg = "gs", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}
