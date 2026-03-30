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
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "gs"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        gs_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "gs")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PDAG"
  method
}

#' @keywords internal
gs_bnlearn_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "gs_bnlearn_runner"
  )

  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "bnlearn",
    "gs"
  )

  if (is.function(test)) {
    args_to_pass$test <- "custom-test"
    args_to_pass$fun <- test
  }

  search$set_test(test, alpha)
  search$set_alg("gs", args_to_pass)

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
