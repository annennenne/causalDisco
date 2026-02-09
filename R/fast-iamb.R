#' @title Fast-IAMB Algorithm for Causal Discovery
#'
#' @description
#' Run the Fast-IAMB (Fast Incremental Association) algorithm for causal discovery using one of several engines.
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
#' For specific details on the supported scores, tests, and parameters for each engine, see:
#' \itemize{
#'  \item \code{\link{BnlearnSearch}} for \pkg{bnlearn}.
#' }
#'
#' @example inst/roxygen-examples/fast-iamb-example.R
#'
#' @return
#' A function of class \code{"fast_iamb"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` (of class "PDAG") and a `knowledge` object.
#'
#' @family causal discovery algorithms
#' @concept cd_algorithms
#' @export
fast_iamb <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "fast_iamb"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        fast_iamb_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "fast_iamb")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @keywords internal
fast_iamb_bnlearn_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "fast_iamb_bnlearn_runner"
  )

  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "bnlearn",
    "fast_iamb"
  )

  search$set_test(test, alpha)
  search$set_alg("fast_iamb", args_to_pass)

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
