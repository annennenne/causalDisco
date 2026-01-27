#' @title PC Algorithm for Causal Discovery
#'
#' @description
#' Run the PC (Peter-Clark) algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'     \item{\code{"pcalg"}}{\pkg{pcalg} R package.}
#'     \item{\code{"bnlearn"}}{\pkg{bnlearn} R package.}
#'   }
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine (e.g. test or algorithm parameters).
#'
#' @details
#' For specific details on the supported scores, tests, and parameters for each engine, see:
#' \itemize{
#'  \item \code{\link{TetradSearch}} for \pkg{Tetrad},
#'  \item \code{\link{PcalgSearch}} for \pkg{pcalg},
#'  \item \code{\link{BnlearnSearch}} for \pkg{bnlearn}.
#' }
#'
#' @example inst/roxygen-examples/pc-example.R
#'
#' @return
#' A function of class \code{"pc"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` and a `knowledge` object.
#'
#' @export
pc <- function(
  engine = c("tetrad", "pcalg", "bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "pc"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      tetrad = rlang::exec(
        pc_tetrad_runner,
        test = test,
        alpha = alpha,
        !!!args
      ),
      pcalg = rlang::exec(pc_pcalg_runner, test = test, alpha = alpha, !!!args),
      bnlearn = rlang::exec(
        pc_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "pc")
  attr(method, "engine") <- engine
  method
}

#' @keywords internal
pc_tetrad_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava",
      "rlang"
    ),
    function_name = "pc_tetrad_runner"
  )

  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "tetrad",
    "pc",
    test = test
  )

  if (length(args_to_pass$test_args) > 0) {
    if (test == "probabilistic") {
      # probabilistic test in Tetrad does not use alpha
      rlang::exec(
        search$set_test,
        method = test,
        !!!args_to_pass$test_args
      )
    } else {
      rlang::exec(
        search$set_test,
        method = test,
        alpha = alpha,
        !!!args_to_pass$test_args
      )
    }
  } else {
    if (test == "probabilistic") {
      # probabilistic test in Tetrad does not use alpha
      search$set_test(method = test)
    } else {
      search$set_test(method = test, alpha = alpha)
    }
  }

  if (length(args_to_pass$alg_args) > 0) {
    # splice
    rlang::exec(search$set_alg, "pc", !!!args_to_pass$alg_args)
  } else {
    search$set_alg("pc")
  }

  runner <- list(
    set_knowledge = function(knowledge) search$set_knowledge(knowledge),
    run = function(data) search$run_search(data)
  )
  runner
}


#' @keywords internal
pc_pcalg_runner <- function(
  test,
  alpha,
  ...,
  directed_as_undirected_knowledge = FALSE
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "pc_pcalg_runner"
  )

  search <- PcalgSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "pcalg",
    "pc",
    test = test
  )

  search$set_params(args_to_pass$alg_args)
  search$set_test(test, alpha)
  search$set_alg("pc")

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(
        knowledge,
        directed_as_undirected = directed_as_undirected_knowledge
      )
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}

#' @keywords internal
pc_bnlearn_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "pc_bnlearn_runner"
  )

  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "bnlearn",
    "pc.stable"
  )

  search$set_test(test, alpha)
  search$set_alg("pc", args_to_pass)

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
