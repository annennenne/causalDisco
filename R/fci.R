#' @title FCI Algorithm for Causal Discovery
#'
#' @description
#' Run the FCI algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{\pkg{Tetrad} Java library.}
#'     \item{\code{"pcalg"}}{\pkg{pcalg} R package.}
#'   }
#' @param test Character; name of the conditional‐independence test.
#' @param alpha Numeric; significance level for the CI tests.
#' @param ... Additional arguments passed to the chosen engine
#' (e.g. test or algorithm parameters).
#'
#' @details
#' For specific details on the supported scores, tests, and parameters for each engine, see:
#' \itemize{
#'  \item \code{\link{TetradSearch}} for \pkg{Tetrad},
#'  \item \code{\link{PcalgSearch}} for \pkg{pcalg}.
#' }
#'
#' @example inst/roxygen-examples/fci-example.R
#'
#' @return
#' A function of class \code{"fci"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` (of class "UNKNOWN") and a `knowledge`
#' (`knowledgeable_caugi`) object.
#'
#' @export
fci <- function(
  engine = c("tetrad", "pcalg"),
  test,
  alpha = 0.05,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "fci"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      tetrad = rlang::exec(
        fci_tetrad_runner,
        test = test,
        alpha = alpha,
        !!!args
      ),
      pcalg = rlang::exec(fci_pcalg_runner, test = test, alpha = alpha, !!!args)
    )
    runner
  }
  method <- disco_method(builder, "fci")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PAG"
  method
}

# Set available engines
attr(fci, "engines") <- c("tetrad", "pcalg")

fci_tetrad_runner <- function(test, alpha, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang",
      "rJava"
    ),
    function_name = "fci_tetrad_runner"
  )

  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "tetrad",
    "fci",
    test = test
  )

  if (length(args_to_pass$test_args) > 0) {
    rlang::exec(
      search$set_test,
      method = test,
      alpha = alpha,
      !!!args_to_pass$test_args
    )
  } else {
    search$set_test(method = test, alpha = alpha)
  }

  if (length(args_to_pass$alg_args) > 0) {
    rlang::exec(search$set_alg, "fci", !!!args_to_pass$alg_args)
  } else {
    search$set_alg("fci")
  }

  runner <- list(
    set_knowledge = function(knowledge) search$set_knowledge(knowledge),
    run = function(data) search$run_search(data)
  )
  runner
}

fci_pcalg_runner <- function(
  test,
  alpha,
  ...,
  directed_as_undirected_knowledge = FALSE
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "fci_pcalg_runner"
  )
  args <- list(...)
  search <- PcalgSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search,
    args,
    "pcalg",
    "fci",
    test = test
  )
  search$set_params(args_to_pass$alg_args)
  search$set_test(test, alpha)
  search$set_alg("fci")

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
