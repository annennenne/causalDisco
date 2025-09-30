#' @title Disco!!
#'
#' @description
#' Run a causal discovery method on a data frame.
#'
#' @param data A data frame
#' @param method An `disco_method` object representing a causal discovery
#' algorithm.
#' @param knowledge A `knowledge` object
#'
#' @example inst/roxygen-examples/disco_example.R
#'
#' @returns A `discography` object.
#'
#' @export
disco <- function(data, method, knowledge = NULL) {
  if (!inherits(method, "disco_method")) {
    stop("The method must be a disco method object.", call. = FALSE)
  }
  # inject knowledge via S3 generic
  if (!is.null(knowledge)) {
    is_knowledge(knowledge)
    tryCatch(
      {
        method <- set_knowledge(method, knowledge)
      },
      error = function(e) {
        # extra precaution to catch errors in setting knowledge
        stop("Error in setting knowledge: ", e$message, call. = FALSE) # nocov
      }
    )
  }
  method(data)
}
