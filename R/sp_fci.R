#' @title SP-FCI Algorithm for Causal Discovery
#'
#' @description
#' Run the SP-FCI (Sparsest Permutation–based Fast Causal Inference) algorithm for causal discovery using one of
#' several engines. This algorithm wraps the SP (Sparsest Permutation) score-based search in an
#' FCI-style. Can be computationally intensive.
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
#' @return
#' A function of class \code{"sp_fci"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` (of class "UNKNOWN") and a `knowledge`
#' (`knowledgeable_caugi`) object.
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
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "sp_fci"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      tetrad = rlang::exec(sp_fci_tetrad_runner, score, test, alpha, !!!args)
    )
    runner
  }

  method <- disco_method(builder, "sp_fci")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PAG"
  method
}

#' @keywords internal
sp_fci_tetrad_runner <- function(score, test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava",
      "rlang"
    ),
    function_name = "sp_fci_tetrad_runner"
  )

  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "tetrad",
    "sp_fci",
    score = score,
    test = test
  )

  if (length(args_to_pass$score_args) > 0) {
    rlang::exec(search$set_score, score, !!!args_to_pass$score_args)
  } else {
    search$set_score(score)
  }

  if (length(args_to_pass$test_args) > 0) {
    rlang::exec(
      search$set_test,
      test,
      alpha = alpha,
      !!!args_to_pass$test_args
    )
  } else {
    search$set_test(test, alpha = alpha)
  }

  if (length(args_to_pass$alg_args) > 0) {
    rlang::exec(search$set_alg, "sp_fci", !!!args_to_pass$alg_args)
  } else {
    search$set_alg("sp_fci")
  }

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}
