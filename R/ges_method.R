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

ges_tetrad <- function(score, ...) {
  # to do: fix arguments passing correctly
  search <- TetradSearch$new()
  # to do: set_params(args_to_pass)
  search$set_score(score, ...)
  search$set_alg("fges")
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
