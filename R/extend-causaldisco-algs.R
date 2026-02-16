#' Add a New causalDisco Method
#'
#' This function allows you to create a new causal discovery method that can be used with the [disco()] function.
#' You provide a builder function that constructs a runner object, along with metadata about the algorithm, and it
#' returns a closure that can be called with a data frame to perform causal discovery and return a `caugi` object.
#'
#' @param builder A function returning a runner
#' @param name Algorithm name
#' @param engine Engine identifier
#' @param graph_class Output graph class
#' @return A function of class \code{"disco_method"} that takes a single argument
#' \code{data} (a data frame) and returns a `caugi` object.
#'
#' @family Extending causalDisco
#' @concept extending_causalDisco
#' @export
new_disco_method <- function(builder, name, engine, graph_class) {
  method <- disco_method(builder, name)
  attr(method, "engine") <- engine
  attr(method, "graph_class") <- graph_class
  method
}

#' Distribute and Validate Engine Arguments
#'
#' This function checks the provided arguments against the expected arguments for the specified engine and algorithm,
#' and distributes them appropriately to the search object. It ensures that the arguments are valid for the given
#' engine and algorithm, and then sets them on the search object.
#'
#' @param search R6 object, either `TetradSearch`, `BnlearnSearch`, `PcalgSearch`, or `CausalDiscoSearch`.
#' @param args List of arguments to distribute
#' @param engine Engine identifier, either "tetrad", "bnlearn", "pcalg", or "causalDisco"
#' @param alg Algorithm name
#'
#' @family Extending causalDisco
#' @concept extending_causalDisco
#' @export
distribute_engine_args <- function(search, args, engine, alg) {
  check_args_and_distribute_args(search, args, engine, alg)
}

tetrad_alg_registry <- new.env(parent = emptyenv())

#' Register a New Tetrad Algorithm
#'
#' Registers a new Tetrad algorithm by adding it to the internal registry. The `setup_fun()` should be a function that
#' takes the same arguments as the runner function for the algorithm and sets up the Tetrad search object accordingly.
#' This allows you to extend the set of Tetrad algorithms that can be used with causalDisco.
#'
#' @param name Algorithm name (string)
#' @param setup_fun A function that sets up the Tetrad search object for the
#' algorithm. It should take the same arguments as the runner function for the algorithm.
#'
#' @family Extending causalDisco
#' @concept extending_causalDisco
#' @export
register_tetrad_algorithm <- function(name, setup_fun) {
  name <- tolower(name)
  if (!is.function(setup_fun)) {
    stop("`setup_fun` must be a function")
  }
  tetrad_alg_registry[[name]] <- setup_fun
}

#' Reset the Tetrad Algorithm Registry
#'
#' Clears all registered algorithms.
#'
#' @family Extending causalDisco
#' @concept extending_causalDisco
#'
#' @export
reset_tetrad_alg_registry <- function() {
  rm(
    list = ls(tetrad_alg_registry, all.names = TRUE),
    envir = tetrad_alg_registry
  )
}
