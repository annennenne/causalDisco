#' disco function
#' @param data a data frame
#' @param method an algorithm/method object
#' @param knowledge a knowledge object
#' @param engine a character describing the underlying engine to use=
#' @export
disco <- function(data, method, knowledge = NULL) {
  if (!is.null(knowledge)) {
    if (!is.null(method$set_knowledge)) {
      method$set_knowledge(knowledge)
    } else {
      warning("Method", method, "does not support knowledge.")
    }
  }

  result <- method$run(data)
  return(result)
}
