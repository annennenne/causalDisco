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
  args_to_pass <- check_args_and_return_passable_args(args, "tetrad", test, search)
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
  args_to_pass <- check_args_and_return_passable_args(args, "pcalg", test, search)
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


check_args_and_return_passable_args <- function(args, engine, test = NULL, search = NULL) {
  # Check if the engine is supported
  if (!(engine %in% engine_registry)) {
    stop(
      "Engine ", paste(engine), "is not supported. Supported engines are: ",
      paste(engine_registry, collapse = ", ")
    )
  }
  if (engine %in% c("pcalg", "bnlearn")) {
    if (engine == "pcalg") {
      engine_args <- names(formals(pcalg::pc))
    } else if (engine == "bnlearn") {
      stop("bnlearn engine is not supported yet.")
      engine_args <- names(formals(bnlearn::pc))
    }

    args_to_pass_to_engine <- args[names(args) %in% engine_args]

    # Check if any arguments are not in pcalg::pc
    args_not_in_engine_args <- setdiff(names(args), engine_args)
    if (length(args_not_in_engine_args) > 0) {
      warning(
        "The following arguments are not used in pcalg::pc: ",
        paste(args_not_in_engine_args, collapse = ", ")
      )
    }
    return(args_to_pass_to_engine)
  } else if (engine == "tetrad") {
    if (is.null(search)) {
      stop("Search object must be provided for checks.")
    }
    if (is.null(test)) {
      stop("Test must be provided for Tetrad engine checks.")
    }
    engine_args_alg <- search$get_parameters_for_function("pc")
    engine_args_test <- search$get_parameters_for_function(test)

    args_to_pass_to_engine_alg <- args[names(args) %in% engine_args_alg]
    args_to_pass_to_engine_test <- args[names(args) %in% engine_args_test]

    # Check if any arguments are not in tetrad's pc or test
    args_not_in_engine_args <- setdiff(
      names(args),
      c(engine_args_alg, engine_args_test)
    )
    if (length(args_not_in_engine_args) > 0) {
      warning(
        "The following arguments are not used in Tetrad's pc or test: ",
        paste(args_not_in_engine_args, collapse = ", ")
      )
    }
    return(list(
      alg_args = args_to_pass_to_engine_alg,
      test_args = args_to_pass_to_engine_test
    ))
  } else {
    stop("Unsupported engine: ", engine)
  }
}
