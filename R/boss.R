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
#'  \item \code{\link{TetradSearch}} for \pkg{Tetrad}.
#' }
#'
#' @example inst/roxygen-examples/boss-example.R
#'
#' @return
#' A function of class \code{"boss"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` (of class "PDAG") and a `knowledge`
#' (`knowledgeable_caugi`) object.
#'
#' @export
boss <- function(
  engine = "tetrad",
  score,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "boss"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      tetrad = rlang::exec(boss_tetrad_runner, score, !!!args)
    )
    runner
  }

  method <- disco_method(builder, "boss")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @keywords internal
boss_tetrad_runner <- function(score, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava",
      "rlang"
    ),
    function_name = "boss_tetrad_runner"
  )

  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "tetrad",
    "boss",
    score = score
  )

  if (length(args_to_pass$score_args) > 0) {
    rlang::exec(search$set_score, score, !!!args_to_pass$score_args)
  } else {
    search$set_score(score)
  }

  if (length(args_to_pass$alg_args) > 0) {
    rlang::exec(search$set_alg, "boss", !!!args_to_pass$alg_args)
  } else {
    search$set_alg("boss")
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
