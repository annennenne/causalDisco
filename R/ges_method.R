#' @export
ges <- function(engine = "tetrad", score, ...) {
  engine <- tolower(engine)

  switch(engine,
    tetrad = ges_tetrad(score, ...),
    pcalg = ges_pcalg(score, ...),
    bnlearn = ges_bnlearn(score, ...),
    stop("Unsupported engine: ", engine)
  )
}


# Set available engines
attr(ges, "engines") <- c("tetrad")

pc_tetrad <- function(score, ...) {
  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(search, args, "tetrad", "fges", score = score)
  if (length(args_to_pass$score_args) != 0) {
    search$set_score(score, args_to_pass$score_args)
  } else {
    search$set_score(score)
  }
  if (length(args_to_pass$alg_args) != 0) {
    search$set_alg("ges", args_to_pass$alg_args)
  } else {
    search$set_alg("ges")
  }
  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
      search$get_dot() # todo: change to igraph later
    }
  )
  return(runner)
}

ges_pcalg <- function(score, ...) {
  stop("Not implemented yet.")
}


ges_bnlearn <- function(score, ...) {
  stop("Not implemented yet.")
}
