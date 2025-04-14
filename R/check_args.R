check_args_and_distribute_args <- function(search, args, engine, alg, test = NULL, score = NULL) {
  # This function will throw an error if arguments are given that are not in the underlying algorithm call.
  # If '...' in given algorithm/test is an argument, it will throw a warning rather than an error.
  # For tetrad, it will distribute arguments to test, score, and algorithm if needed.
  # Check if the engine is supported
  if (!(engine %in% engine_registry)) {
    stop(
      "Engine ", paste(engine), "is not supported. Supported engines are: ",
      paste(engine_registry, collapse = ", ")
    )
  }
  switch(engine,
    tetrad = check_args_and_distribute_args_tetrad(search, args, alg, test, score),
    pcalg = check_args_and_distribute_args_pcalg(search, args, alg, test, score),
    bnlearn = check_args_and_distribute_args_bnlearn(search, args, alg, test, score),
    stop("Unsupported engine: ", engine)
  )
}

check_args_and_distribute_args_tetrad <- function(search, args, alg, test = NULL, score = NULL) {
  if (is.null(test) && is.null(score)) {
    stop("Neither test or score is specified.")
  }
  engine_args_test <- c()
  engine_args_score <- c()
  if (!is.null(test)) {
    engine_args_test <- search$get_parameters_for_function(test)
  }
  if (!is.null(score)) {
    engine_args_score <- search$get_parameters_for_function(score)
  }
  engine_args_score_test <- c(engine_args_test, engine_args_score)
  engine_args_alg <- search$get_parameters_for_function(alg)

  args_to_pass_to_engine_alg <- args[names(args) %in% engine_args_alg]
  args_to_pass_to_engine_score_test <- args[names(args) %in% engine_args_score_test]

  # Check if any arguments are not in tetrad's pc or test
  args_not_in_engine_args <- setdiff(
    names(args),
    c(engine_args_alg, engine_args_test)
  )
  if (length(args_not_in_engine_args) > 0) {
    stop(
      "The following arguments are not used in Tetrad algorithm or test: ",
      paste(args_not_in_engine_args, collapse = ", ")
    )
  }
  return(list(
    alg_args = args_to_pass_to_engine_alg,
    test_args = args_to_pass_to_engine_score_test
  ))
}

check_args_and_distribute_args_pcalg <- function(search, args, alg, test = NULL, score = NULL) {
  # todo: test
  switch(alg,
    pc = engine_args <- names(formals(pcalg::pc)),
    fci = engine_args <- names(formals(pcalg::fci)),
    ges = engine_args <- names(formals(pcalg::ges)),
    stop("Unsupported algorithm:", alg)
  )

  args_to_pass_to_engine <- args[names(args) %in% engine_args]

  # Check if any arguments are not in pcalg::
  args_not_in_engine_args <- setdiff(names(args), engine_args)
  if (length(args_not_in_engine_args) > 0) {
    warning(
      paste0("The following arguments are not used in ", engine, "::", alg, ": "),
      paste(args_not_in_engine_args, collapse = ", ")
    )
  }
  return(args_to_pass_to_engine)
}

check_args_and_distribute_args_bnlearn <- function() {
  stop("Not implemented yet for bnlearn")
}
