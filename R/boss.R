#' @title BOSS Algorithm for Causal Discovery
#'
#' @description
#' Run the BOSS (Best Order Score Search) algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'   }
#' @inheritParams ges
#'
#' @details
#' For specific details on the supported scores, and parameters for each engine, see:
#' \itemize{
#'  \item [TetradSearch] for \pkg{Tetrad}.
#' }
#'
#' @example inst/roxygen-examples/boss-example.R
#' @references
#' Andrews, B., Ramsey, J., Sánchez-Romero, R., Camchong, J., & Kummerfeld, E. (2023, December).
#' Fast scalable and accurate discovery of DAGs using the Best Order Score Search and Grow-Shrink Trees.
#' Advances in Neural Information Processing Systems, 36, 63945-63956.
#' Epub 2024 May 30. PMID: 39280091; PMCID: PMC11393735.
#' @inheritSection disco_note Recommendation
#' @inheritSection disco_algs_return_doc_pdag Value
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
boss <- function(
  engine = "tetrad",
  score,
  ...
) {
  engine <- match.arg(engine)
  make_method(
    method_name = "boss",
    engine = engine,
    engine_fns = list(
      tetrad = function(...) {
        make_runner(engine = "tetrad", alg = "boss", ...)
      }
    ),
    score = score,
    graph_class = "PDAG",
    ...
  )
}
