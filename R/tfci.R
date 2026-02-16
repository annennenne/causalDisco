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
#' @return
#' A function of class \code{"tfci"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` (of class "UNKNOWN") and `knowledge` (`knowledgeable_caugi`)
#' object.
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
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "tfci"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  # build a `runner builder` that knows how to make a runner given knowledge
  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      causalDisco = rlang::exec(
        tfci_causalDisco_runner,
        test,
        alpha,
        !!!args
      )
    )
    runner
  }

  method <- disco_method(builder, "tfci")
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- "PAG"
  method
}

#' @keywords internal
tfci_causalDisco_runner <- function(
  test,
  alpha,
  ...,
  directed_as_undirected_knowledge = FALSE
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "pc_causalDisco_runner"
  )

  search <- CausalDiscoSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(
    search = search,
    args = args,
    engine = "causalDisco",
    alg = "tfci",
    test = test
  )

  search$set_params(args_to_pass$alg_args)
  search$set_test(test, alpha)
  search$set_alg("tfci")

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
