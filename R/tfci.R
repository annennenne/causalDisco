#' @title TFCI Algorithm for Causal Discovery
#'
#' @description
#' Run the temporal FCI algorithm for causal discovery using causalDisco.
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
#' For specific details on the supported tests, see [CausalDiscoSearch]. For additional parameters passed
#' via \code{...}, see [tfci_run()].
#'
#' @example inst/roxygen-examples/tfci-example.R
#'
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
tfci <- function(
  engine = c("causalDisco"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "tfci",
    engine = engine,
    engine_fns = list(
      causalDisco = function(...) {
        make_runner(engine = "causalDisco", alg = "tfci", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PAG",
    ...
  )
}
