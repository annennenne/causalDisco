#' @title The GES algorithm for causal discovery
#'
#' @description
#' Run the GES algorithm for causal discovery using one of several engines.
#'
#' @param engine Character; which engine to use. Must be one of:
#'   \describe{
#'     \item{\code{"tetrad"}}{Tetrad Java library.}
#'     \item{\code{"pcalg"}}{\pkg{pcalg} R package.}
#'     \item{\code{"bnlearn"}}{\pkg{bnlearn} R package.}
#'   }
#' @param score Character; name of the scoring function to use.
#' @param ... Additional arguments passed to the chosen engine (e.g. test or algorithm parameters).
#'
#' @return
#' A function of class \code{"ges"} that takes a single argument \code{data}
#' (a data frame) and returns an igraph_party object.
#'
#' @export
ges <- function(
    engine = c("tetrad", "pcalg", "bnlearn"),
    score,
    ...) {
  engine <- match.arg(engine)

  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(engine,
      tetrad = rlang::exec(ges_tetrad_runner, score, !!!args),
      pcalg = rlang::exec(ges_pcalg_runner, score, !!!args),
      bnlearn = rlang::exec(ges_bnlearn_runner, score, !!!args)
    )
    if (!is.null(knowledge)) {
      runner$set_knowledge(knowledge)
    }
    runner
  }

  disco_method(builder, "ges")
}


#' @keywords internal
ges_tetrad_runner <- function(score, ...) {
  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(search, args, "tetrad", "fges", score = score)
  if (length(args_to_pass$score_args) != 0) {
    search$set_score(score, args_to_pass$score_args)
  } else {
    search$set_score(score)
  }
  if (length(args_to_pass$alg_args) != 0) {
    search$set_alg("fges", args_to_pass$alg_args)
  } else {
    search$set_alg("fges")
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

#' @keywords internal
ges_pcalg_runner <- function(score, ..., directed_as_undirected_knowledge = FALSE) {
  args <- list(...)
  search <- pcalgSearch$new()
  args_to_pass <- check_args_and_distribute_args(search, args, "pcalg", "ges", score = score)
  search$set_params(args_to_pass$alg_args)
  search$set_score(score, args_to_pass$score_args)
  search$set_alg("ges")

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



ges_bnlearn_runner <- function(score, ...) {
  stop("Not implemented yet.")
}
