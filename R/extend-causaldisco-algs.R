# Provides the public api for extending causalDisco with new algorithms.
#' Create a causalDisco method
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

#' Validate and distribute engine arguments
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

#' Register a new Tetrad algorithm
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

#' Reset the Tetrad algorithm registry
#'
#' Clears all registered algorithms.
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
