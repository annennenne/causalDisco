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
  # to do: fix arguments passing correctly
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

pc_pcalg <- function(test, alpha, ...) {
  args <- list(...)
  args_to_pass <- check_args_and_return_passable_args(args, "pcalg")
  search <- pcalgSearch$new()
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


check_args_and_return_passable_args <- function(args, engine) {
  # Check if the engine is supported
  if (!(engine %in% engine_registry)) {
    stop(
      "Engine ", paste(engine), "is not supported. Supported engines are: ",
      paste(engine_registry, collapse = ", ")
    )
  }

  if (engine == "pcalg") {
    # Match ... arguments to pcalg::pc
    pcalg_args <- names(formals(pcalg::pc))
    args_to_pass_to_pcalg <- args[names(args) %in% pcalg_args]

    # Check if any arguments are not in pcalg::pc
    args_not_in_pcalg <- setdiff(names(args), pcalg_args)
    if (length(args_not_in_pcalg) > 0) {
      warning(
        "The following arguments are not used in pcalg::pc: ",
        paste(args_not_in_pcalg, collapse = ", ")
      )
    }
    return(args)
  }

  if (engine == "bnlearn") {
    # Match ... arguments to bnlearn::pc
    bnlearn_args <- names(formals(bnlearn::pc))
    args_to_pass_to_bnlearn <- args[names(args) %in% bnlearn_args]

    # Check if any arguments are not in bnlearn::pc
    args_not_in_bnlearn <- setdiff(names(args), bnlearn_args)
    if (length(args_not_in_bnlearn) > 0) {
      warning(
        "The following arguments are not used in bnlearn::pc: ",
        paste(args_not_in_bnlearn, collapse = ", ")
      )
    }
    return(args_to_pass_to_bnlearn)
  }

  if (engine == "tetrad") {
    # to do
    warning("Args check needs to be implemented for tetrad")
  }
}
