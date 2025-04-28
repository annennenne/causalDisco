#' disco function
#' @param data a data frame
#' @param method an algorithm/method object
#' @param knowledge a knowledge object
#' @export
disco <- function(data, method, knowledge = NULL) {
  if (!is.null(knowledge)) {
    if (!is.null(method$set_knowledge)) {
      method$set_knowledge(knowledge)
    } else {
      stop("Method", method, "does not support knowledge.", .call = FALSE)
    }
  }

  result <- method$run(data)
  return(result)
}
