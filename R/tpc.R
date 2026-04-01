#' @title TPC Algorithm for Causal Discovery
#'
#' @description
#' Run the Temporal Peter-Clark algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"causalDisco"}}{causalDisco library.}
#'   }
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine
#' (e.g. test or algorithm parameters).
#'
#' @details
#' For specific details on the supported tests, see [CausalDiscoSearch]. For additional parameters
#' passed via \code{...}, see [tpc_run()].
#'
#' @example inst/roxygen-examples/tpc-example.R
#'
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pdag Value
#' @references
#' Petersen AH, Osler M, and Ekstrøm CT. Data-Driven Model Building for Life-Course Epidemiology.
#' American Journal of Epidemiology 2021 Mar; 190:1898–907, <doi:10.1093/aje/kwab087>.
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
tpc <- function(
  engine = c("causalDisco"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "tpc",
    engine = engine,
    engine_fns = list(
      causalDisco = function(...) {
        make_runner(engine = "causalDisco", alg = "tpc", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}
