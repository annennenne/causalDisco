library(rJava)

# function that takes a java object and casts it to some superclass 
cast_obj <- function(obj) {
  # If the object is a score, cast it to ScoreWrapper
  if (.jinstanceof(obj, "edu/cmu/tetrad/algcomparison/score/ScoreWrapper")){
    obj <- .jcast(obj, "edu/cmu/tetrad/algcomparison/score/ScoreWrapper")
    return(obj)
  # If the object is a data object, cast it to DataModel
  } else if (.jinstanceof(obj, "edu/cmu/tetrad/data/DataModel")) {
    obj <- .jcast(obj, "edu/cmu/tetrad/data/DataModel")
    return(obj)
  } else {
    # Cast error
    stop("Object cannot be cast to a superclass.")
  }
}

set_params <- function(params, ...) {
  # Capture the named arguments as a list.
  arg_list <- list(...)
  for (param_name in names(arg_list)) {
    value <- arg_list[[param_name]]
    # Get the key (static field) from Params using the field name.
    key <- .jfield("edu/cmu/tetrad/util/Params", "S", param_name)
    
    # Wrap the value based on its type.
    wrapped <- if (is.numeric(value)) {
      .jcast(.jnew("java/lang/Double", as.double(value)), "java/lang/Object")
    } else if (is.logical(value)) {
      .jcast(.jnew("java/lang/Boolean", as.logical(value)), "java/lang/Object")
    } else if (is.character(value)) {
      .jcast(value, "java/lang/Object") # not used right now, but maybe later
    } else {
      .jcast(value, "java/lang/Object")
    }
    
    # Set the parameter using the key and wrapped value.
    params$set(key, wrapped)
  }
  invisible(NULL)
}

