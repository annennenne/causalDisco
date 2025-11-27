#' @title The Temporal GES algorithm for causal discovery
#'
#' @description
#' Run the Temporal GES algorithm for causal discovery using the causalDisco
#' engine.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"causalDisco"}}{causalDisco library.}
#'   }
#' @param score Character; name of the scoring function to use.
#' @param ... Additional arguments passed to the chosen engine
#' (e.g. test or algorithm parameters).
#'
#' @example inst/roxygen-examples/tges_example.R
#'
#' @return
#' A function of class \code{"tges"} that takes a single argument \code{data}
#' (a data frame) and returns a `caugi` and `knowledge` (`knowledgeable_caugi`)
#' object.
#'
#' @export
tges <- function(
  engine = c("causalDisco"),
  score,
  ...
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang"
    ),
    function_name = "tges"
  )

  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(engine,
      causalDisco = rlang::exec(
        tges_causalDisco_runner,
        score,
        !!!args
      )
    )
    runner
  }

  disco_method(builder, "tges")
}

#' @keywords internal
tges_causalDisco_runner <- function(score, ...,
                                    directed_as_undirected_knowledge = FALSE) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "tges_causalDisco_runner"
  )

  args <- list(...)
  search <- causalDiscoSearch$new()
  args_to_pass <- check_args_and_distribute_args(
    search = search,
    args = args,
    engine = "causalDisco",
    alg = "tges",
    score = score
  )
  search$set_params(args_to_pass$alg_args)
  search$set_score(score, args_to_pass$score_args)
  search$set_alg("tges")

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge,
        directed_as_undirected = directed_as_undirected_knowledge
      )
    },
    run = function(data) {
      search$run_search(data, set_suff_stat = FALSE)
    }
  )

  runner
}
