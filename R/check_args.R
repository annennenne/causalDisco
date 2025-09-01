check_args_and_distribute_args <- function(search,
                                           args,
                                           engine,
                                           alg,
                                           test = NULL,
                                           score = NULL) {
  # Check if the engine is supported
  if (!(engine %in% engine_registry)) {
    stop(
      "Engine ", paste(engine), " is not supported. Supported engines are: ",
      paste(engine_registry, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(search) && engine == "tetrad") {
    stop("TetradSearch object must be provided for Tetrad engine.",
      call. = FALSE
    )
  }
  switch(engine,
    tetrad = check_args_and_distribute_args_tetrad(
      search, args, alg, test, score
    ),
    pcalg = check_args_and_distribute_args_pcalg(
      args, alg, test, score
    ),
    bnlearn = check_args_and_distribute_args_bnlearn(
      args, alg
    ),
    causalDisco = check_args_and_distribute_args_causalDisco(
      args, alg, test, score
    )
  )
}

#' Check arguments and distribute them to TetradSearch class functions
#'
#' @keywords internal
check_args_and_distribute_args_tetrad <- function(search,
                                                  args,
                                                  alg,
                                                  test = NULL,
                                                  score = NULL) {
  if (is.null(test) && is.null(score)) {
    stop("Neither test or score is specified.", call. = FALSE)
  }

  # Verbose will be passed directly to TetradSearch class.
  if ("verbose" %in% names(args)) {
    search$set_verbose(args[["verbose"]])
    args$verbose <- NULL
  }

  engine_args_test <- c()
  engine_args_score <- c()
  if (!is.null(test)) {
    engine_args_test <- search$get_parameters_for_function(test, test = TRUE)
  }
  if (!is.null(score)) {
    engine_args_score <- search$get_parameters_for_function(score, score = TRUE)
  }
  engine_args_score_test <- c(engine_args_test, engine_args_score)
  engine_args_alg <- search$get_parameters_for_function(alg, alg = TRUE)

  args_to_pass_to_engine_alg <- args[names(args) %in% engine_args_alg]
  args_to_pass_to_engine_score <- args[names(args) %in% engine_args_score]
  args_to_pass_to_engine_test <- args[names(args) %in% engine_args_test]

  args_not_in_engine_args <- setdiff(
    names(args),
    c(engine_args_alg, engine_args_test, engine_args_score)
  )
  if (length(args_not_in_engine_args) > 0) {
    stop(
      "The following arguments are not used in Tetrad algorithm, test, or score: ",
      paste(args_not_in_engine_args, collapse = ", "),
      call. = FALSE
    )
  }
  return(list(
    alg_args = args_to_pass_to_engine_alg,
    test_args = args_to_pass_to_engine_test,
    score_args = args_to_pass_to_engine_score
  ))
}

check_args_and_distribute_args_pcalg <- function(args,
                                                 alg,
                                                 test = NULL,
                                                 score = NULL) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "methods", "pcalg"
    ),
    function_name = "check_args_and_distribute_args_pcalg"
  )

  # Note that the pcalg package does not have args that are sent
  # directly to the test itself, but it is rather sent to the algorithm.
  switch(alg,
    pc = engine_args_alg <- names(formals(pcalg::pc)),
    fci = engine_args_alg <- names(formals(pcalg::fci)),
    ges = engine_args_alg <- names(formals(pcalg::ges)),
    stop("Unsupported algorithm: ", alg, call. = FALSE)
  )

  args_to_pass_to_engine_alg <- args[names(args) %in% engine_args_alg]
  engine_args_score <- list()
  if (!is.null(score)) {
    engine_args_score <- methods::getRefClass("GaussL0penIntScore")$
      methods("initialize") |>
      methods::formalArgs()
    args_to_pass_to_engine_score <- args[names(args) %in% engine_args_score]
  } else {
    args_to_pass_to_engine_score <- list()
  }
  # Check if any arguments are not in pcalg::
  args_not_in_engine_args <- setdiff(
    names(args),
    c(engine_args_alg, engine_args_score)
  )
  # If '...' in given algorithm/test is an argument, it will throw a warning
  # rather than an error.
  if (length(args_not_in_engine_args) > 0) {
    if ("..." %in% c(engine_args_alg, engine_args_score)) {
      warning(
        paste0("The following arguments are not used in pcalg::", alg, ": "),
        paste(args_not_in_engine_args, collapse = ", "),
        call. = FALSE
      )
    } else {
      stop(
        paste0("The following arguments are not used in pcalg::", alg, ": "),
        paste(args_not_in_engine_args, collapse = ", "),
        call. = FALSE
      )
    }
  }
  return(list(
    alg_args = args_to_pass_to_engine_alg,
    score_args = args_to_pass_to_engine_score
  ))
}


check_args_and_distribute_args_causalDisco <- function(args,
                                                       alg,
                                                       test = NULL,
                                                       score = NULL) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "methods", "pcalg"
    ),
    function_name = "check_args_and_distribute_args_causalDisco"
  )

  # Note that the causalDisco package does not have args that are sent
  # directly to the test itself
  switch(alg,
    tpc = engine_args_alg <- names(formals(tpc)),
    tfci = engine_args_alg <- names(formals(tfci)),
    tges = engine_args_alg <- names(formals(tges)),
    stop("Unsupported algorithm: ", alg, call. = FALSE)
  )

  args_to_pass_to_engine_alg <- args[names(args) %in% engine_args_alg]
  engine_args_score <- list()
  if (!is.null(score)) {
    score <- tolower(score)
    switch(score,
      "tbic" = score <- "TemporalBIC",
      "tbdeu" = score <- "TemporalBDeu"
    )
    engine_args_score <- methods::getRefClass(score)$
      methods("initialize") |>
      methods::formalArgs()
    args_to_pass_to_engine_score <- args[names(args) %in% engine_args_score]
  } else {
    args_to_pass_to_engine_score <- list()
  }
  # Check if any arguments are not in engine args
  args_not_in_engine_args <- setdiff(
    names(args),
    c(engine_args_alg, engine_args_score)
  )
  # If '...' in given algorithm/test is an argument, it will throw a warning
  # rather than an error.
  if (length(args_not_in_engine_args) > 0) {
    if ("..." %in% c(engine_args_alg, engine_args_score)) {
      warning(
        paste0("The following arguments are not used in causalDisco::", alg, ": "),
        paste(args_not_in_engine_args, collapse = ", "),
        call. = FALSE
      )
    } else {
      stop(
        paste0("The following arguments are not used in causalDisco::", alg, ": "),
        paste(args_not_in_engine_args, collapse = ", "),
        call. = FALSE
      )
    }
  }
  return(list(
    alg_args = args_to_pass_to_engine_alg,
    score_args = args_to_pass_to_engine_score
  ))
}

#' @title Check arguments for bnlearnSearch class functions
#'
#' @keywords internal
check_args_and_distribute_args_bnlearn <- function(args,
                                                   alg,
                                                   allow_dots = FALSE) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "bnlearn"
    ),
    function_name = "check_args_and_distribute_args_bnlearn"
  )

  # find bnlearn function
  if (!exists(alg, envir = asNamespace("bnlearn"))) {
    stop("Unsupported algorithm: ", alg, call. = FALSE)
  }

  # get the formal arguments of the function
  bn_fun <- get(alg, envir = asNamespace("bnlearn"))
  alg_formals <- names(formals(bn_fun))
  dots_allowed <- "..." %in% alg_formals

  # which user supplied arguments are valid?
  unclaimed <- setdiff(names(args), alg_formals)

  if (length(unclaimed) > 0) {
    if (!dots_allowed) {
      # learner has no '...' : throw error
      stop(
        "The following arguments are not valid for bnlearn::", alg, ": ",
        paste(unclaimed, collapse = ", "),
        call. = FALSE
      )
    }

    if (dots_allowed && !allow_dots) {
      # learner has '...' but caller did not allow extras
      stop(
        "bnlearn::", alg, " has a '...' formal, but these arguments are not ",
        "recognised: ", paste(unclaimed, collapse = ", "),
        ".  Set allow_dots = TRUE if you really want to forward them."
      )
    }
  }

  # we do not distribute arguments here, as it is not needed
  args
}
