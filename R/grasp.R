#' @title GRaSP Algorithm for Causal Discovery
#'
#' @description
#' Run the GRaSP algorithm for causal discovery using one of several engines.
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
#' @return
#' A function of class \code{"grasp"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` (of class "PDAG") and a `knowledge`
#' (`disco`) object.
#'
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
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "grasp"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      tetrad = rlang::exec(grasp_tetrad_runner, score, test, alpha, !!!args)
    )
    runner
  }

  method <- disco_method(builder, "grasp")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @keywords internal
grasp_tetrad_runner <- function(score, test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava",
      "rlang"
    ),
    function_name = "grasp_tetrad_runner"
  )

  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "tetrad",
    "grasp",
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
    rlang::exec(search$set_alg, "grasp", !!!args_to_pass$alg_args)
  } else {
    search$set_alg("grasp")
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
