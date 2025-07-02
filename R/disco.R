#' disco function
#' @param data a data frame
#' @param method an algorithm/method object
#' @param knowledge a knowledge object
#' @export
disco <- function(data, method, knowledge = NULL) {
  if (!inherits(method, "disco_method")) {
    stop("The method must be a disco method object.", call. = FALSE)
  }
  # inject knowledge via S3 generic
  if (!is.null(knowledge)) {
    tryCatch(
      {
        method <- set_knowledge(method, knowledge)
      },
      error = function(e) {
        stop("Error in setting knowledge: ", e$message, call. = FALSE)
      }
    )
  }
  method(data)
}
