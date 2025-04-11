#' @export
fci <- function(engine = "tetrad", test, alpha = 0.05, ...) {
  engine <- tolower(engine)

  switch(engine,
    tetrad = fci_tetrad(test, alpha, ...),
    pcalg = fci_pcalg(test, alpha, ...),
    bnlearn = fci_bnlearn(test, alpha, ...),
    stop("Unsupported engine: ", engine)
  )
}
# Set available engines
attr(fci, "engines") <- c("tetrad", "pcalg")

fci_tetrad <- function(test, alpha, ...) {
  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- check_args_and_return_passable_args(args, "tetrad", test, search)
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
  search$set_params(args_to_pass)
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

check_args_and_return_passable_args <- function(args, engine, test = NULL, search = NULL) {
  # Check if the engine is supported
  if (!(engine %in% engine_registry)) {
    stop(
      "Engine ", paste(engine), " is not supported. Supported engines are: ",
      paste(engine_registry, collapse = ", ")
    )
  }

  if (engine %in% c("pcalg", "bnlearn")) {
    if (engine == "pcalg") {
      engine_args <- names(formals(pcalg::fci))
    } else if (engine == "bnlearn") {
      stop("bnlearn engine is not supported yet.")
    }

    # Filter out recognized arguments
    args_to_pass_to_engine <- args[names(args) %in% engine_args]

    # Identify unrecognized arguments
    args_not_in_engine_args <- setdiff(names(args), engine_args)

    if (length(args_not_in_engine_args) > 0) {
      if ("..." %in% engine_args) {
        warning(
          paste0(
            "The following arguments might not be used in ", engine, "::fci : ",
            paste(args_not_in_engine_args, collapse = ", ")
          )
        )
      } else {
        stop(
          paste0(
            "The following arguments are not recognized by ", engine, "::fci : ",
            paste(args_not_in_engine_args, collapse = ", ")
          )
        )
      }
    }

    return(args_to_pass_to_engine)
  } else if (engine == "tetrad") {
    if (is.null(search)) {
      stop("Search object must be provided for checks.")
    }
    if (is.null(test)) {
      stop("Test must be provided for Tetrad engine checks.")
    }

    engine_args_alg <- search$get_parameters_for_function("fci")
    engine_args_test <- search$get_parameters_for_function(test)

    args_to_pass_to_engine_alg <- args[names(args) %in% engine_args_alg]
    args_to_pass_to_engine_test <- args[names(args) %in% engine_args_test]

    valid_tetrad_args <- c(engine_args_alg, engine_args_test)
    args_not_in_engine_args <- setdiff(names(args), valid_tetrad_args)

    if (length(args_not_in_engine_args) > 0) {
      if ("..." %in% valid_tetrad_args) {
        warning(
          paste0(
            "The following arguments might not be used in ", engine, "::fci : ",
            paste(args_not_in_engine_args, collapse = ", ")
          )
        )
      } else {
        stop(
          paste0(
            "The following arguments are not recognized by ", engine, "::fci : ",
            paste(args_not_in_engine_args, collapse = ", ")
          )
        )
      }
    }

    return(list(
      alg_args = args_to_pass_to_engine_alg,
      test_args = args_to_pass_to_engine_test
    ))
  } else {
    stop("Unsupported engine: ", engine)
  }
}
