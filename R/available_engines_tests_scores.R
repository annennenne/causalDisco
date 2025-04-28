#' @export
available_engines <- function(method) {
  available <- attr(method, "engines")
  if (is.null(available)) {
    stop("No engine information available.", .call = FALSE)
  }
  method_name <- deparse(substitute(method))
  numbered_engines <- paste0(seq_along(available), ". ", available)
  cat(cat("Available engines for ", method_name, ":", sep = ""),
    paste(numbered_engines, collapse = "\n"),
    sep = "\n"
  )
}

available_tests <- function(engine) {
  engine <- tolower(engine)
  if (!engine %in% engine_registry) {
    stop("Unknown engine: ", engine, "\nThe engines available are: ",
      paste(engine_registry, collapse = ", "),
      .call = FALSE
    )
  }
  if (engine == "tetrad") {
    tests <- c(
      "chi_square",
      "fisher_z",
      "cci",
      "basis_function_lrt",
      "basis_function_lrt_fs",
      "conditional_gaussian",
      "degenerate_gaussian",
      "g_square",
      "kci",
      "probabilistic"
    )
  } else if (engine == "bnlearn") {
    stop("bnlearn is not supported yet.")
    tests <- c()
  } else if (engine == "pcalg") {
    tests <- c(
      "fisher_z",
      "g_square"
    )
  } else {
    stop("Test registry not made for: ", engine, .call = FALSE)
  }
  numbered_tests <- paste0(seq_along(tests), ". ", tests)
  cat(cat("Available tests for ", engine, ":", sep = ""), paste(numbered_tests, collapse = "\n"), sep = "\n")
}

available_scores <- function(engine) {
  engine <- tolower(engine)
  if (!engine %in% engine_registry) {
    stop("Unknown engine: ", engine, "\nThe engines available are: ", paste(engine_registry, collapse = ", "))
  }
  if (engine == "tetrad") {
    scores <- c(
      "sem_bic",
      "ebic",
      "bdeu",
      "basis_function_bic",
      "basis_function_bic_fs",
      "conditional_gaussian",
      "degenerate_gaussian",
      "gic",
      "mixed_variable_polynomial",
      "poisson_prior",
      "zhang_shen_bound"
    )
  } else if (engine == "bnlearn") {
    stop("bnlearn is not supported yet.", .call = FALSE)
    scores <- c()
  } else if (engine == "pcalg") {
    scores <- c(
      "bic",
      "bdeu"
    )
  } else {
    stop("Score registry not made for: ", engine, .call = FALSE)
  }
  numbered_scores <- paste0(seq_along(scores), ". ", scores)
  cat(cat("Available scores for ", engine, ":", sep = ""),
    paste(numbered_scores, collapse = "\n"),
    sep = "\n"
  )
}
