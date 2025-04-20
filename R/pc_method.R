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
# Set available engines
attr(pc, "engines") <- c("tetrad", "pcalg")

pc_tetrad <- function(test, alpha, ...) {
  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(search, args, "tetrad", "pc", test = test)
  if (length(args_to_pass$test_args) != 0) {
    search$set_test(test, alpha, args_to_pass$test_args)
  } else {
    search$set_test(test, alpha)
  }
  if (length(args_to_pass$alg_args) != 0) {
    search$set_alg("pc", args_to_pass$alg_args)
  } else {
    search$set_alg("pc")
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

pc_pcalg <- function(test, alpha, ...) {
  args <- list(...)
  search <- pcalgSearch$new()
  args_to_pass <- check_args_and_distribute_args(search, args, "pcalg", "pc", test = test)
  search$set_params(args_to_pass)
  search$set_test(test, alpha)
  search$set_alg("pc")
  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
}
