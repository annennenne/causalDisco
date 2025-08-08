#' Cast a Java object to a superclass
#'
#' This function takes a Java object and casts it to a superclass, so it can be
#' used in the Tetrad Search class.
#'
#' @importFrom rJava .jcast .jinstanceof
#' @param obj A Java object to be cast.
#' @return A Java object cast to a superclass that Tetrad takes as input.
cast_obj <- function(obj) {
  # If the object is a score, cast it to ScoreWrapper
  if (.jinstanceof(obj, "edu/cmu/tetrad/algcomparison/score/ScoreWrapper")) {
    obj <- .jcast(obj, "edu/cmu/tetrad/algcomparison/score/ScoreWrapper")
    return(obj)
    # If the object is a data object, cast it to DataModel
  } else if (.jinstanceof(obj, "edu/cmu/tetrad/data/DataModel")) {
    obj <- .jcast(obj, "edu/cmu/tetrad/data/DataModel")
    return(obj)
    # If the object is a test, cast it to IndependenceWrapper
  } else if (
    .jinstanceof(
      obj,
      "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper"
    )
  ) {
    obj <- .jcast(
      obj,
      "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper"
    )
    return(obj)
    # If the object is an EdgeListGraph, cast it to Graph
  } else if (.jinstanceof(obj, "edu/cmu/tetrad/graph/Graph")) {
    obj <- .jcast(obj, "edu/cmu/tetrad/graph/Graph")
    return(obj)
  } else {
    # Cast error
    stop("The Java object cannot be cast to a superclass by cast_obj.",
      call. = FALSE
    )
  }
}
