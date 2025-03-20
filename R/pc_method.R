#' @export
pc <- function(engine = "tetrad", test, alpha = 0.05, ...) {
  engine <- tolower(engine)

  switch(engine,
    tetrad = pc_tetrad(test, alpha, ...),
    pcalg = pc_pcalg(test, alpha, ...),
    bnlearn = pc_bnlearn(test, alpha, ...),
    stop("Unsupported engine: ", engine)
  )
}

pc_tetrad <- function(test, alpha, ...) {
  ts <- TetradSearch$new()
  ts$set_test(test, alpha, ...)
  ts$set_alg("pc")
  runner <- list(
    set_knowledge = function(knowledge) {
      ts$set_knowledge(knowledge)
    },
    run = function(data) {
      ts$run_search(data)
      ts$get_dot() # todo: change to igraph later
    }
  )
  return(runner)
}
