#' @export
fci <- function(engine = "tetrad", test, alpha = 0.05, ...) {
  engine <- tolower(engine)

  switch(engine,
    tetrad = fci_tetrad(test, alpha, ...),
    pcalg = fci_pcalg(test, alpha, ...),
    bnlearn = fci_bnlearn(test, alpha, ...),
    stop("Unsupported engine: ", engine, .call = FALSE)
  )
}
# Set available engines
attr(fci, "engines") <- c("tetrad", "pcalg")

fci_tetrad <- function(test, alpha, ...) {
  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_distribute_args(search, args, "tetrad", "fci", test = test)
  if (length(args_to_pass$test_args) != 0) {
    search$set_test(test, alpha, args_to_pass$test_args)
  } else {
    search$set_test(test, alpha)
  }
  if (length(args_to_pass$alg_args) != 0) {
    search$set_alg("fci", args_to_pass$alg_args)
  } else {
    search$set_alg("fci")
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

fci_pcalg <- function(test, alpha, ...) {
  args <- list(...)
  search <- pcalgSearch$new()
  args_to_pass <- check_args_and_return_passable_args(args, "pcalg", test, search)
  search$set_params(args_to_pass$alg_args)
  search$set_test(test, alpha)
  search$set_alg("fci")
  print(search)
  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
}
