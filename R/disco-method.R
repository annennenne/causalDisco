#' @title Wrap a runner list into a `disco_method` closure
#'
#' @description
#' A `disco_method` is a closure that wraps a builder function.
#' The builder function is expected to return a runner object.
#' The closure can be called with a dataframe, and it will
#' build a fresh runner with the empty knowledge. Use `set_knowledge`
#' to set knowledge into the method.
#'
#' Every `disco_method` can be used in combination with `disco`. If you want
#' to build your own method, you can use this function to create a closure
#' that will run with `disco`.
#'
#' @example inst/roxygen-examples/disco_method-example.R
#' @noRd
#' @keywords internal
disco_method <- function(builder, method_class) {
  f <- function(data) {
    if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
    }
    # build a fresh runner with the current knowledge
    runner <- builder(environment(f)$knowledge)
    runner$run(data)
  }

  # each closure gets its own private env
  env <- environment(f)
  env$builder <- builder
  env$knowledge <- NULL # no knowledge by default

  structure(f, class = c(method_class, "disco_method", "function"))
}


#' Set background knowledge into a disco_method
#'
#' @param method A \code{"disco_method"} function.
#' @param knowledge A knowledge object appropriate for the engine.
#' @export
set_knowledge <- function(method, knowledge) {
  UseMethod("set_knowledge")
}

#' @rdname set_knowledge
#' @export
set_knowledge.disco_method <- function(method, knowledge) {
  old_builder <- environment(method)$builder
  method_class <- class(method)[1]

  # wrap the old builder so it always injects this `knowledge`
  new_builder <- function(k_unused) {
    runner <- old_builder(NULL)
    runner$set_knowledge(knowledge)
    runner
  }

  disco_method(new_builder, method_class)
}
