#' @title Tetrad R Data Utilities
#'
#' @description \code{tetrad_rdata_utils} provides functions to convert between
#' R data frames and Tetrad Java objects.
#'
#' @details This function is made to be used internally with the TetradSearch
#' class. The function will copy the data into the Java heap, so be careful with
#' larger data frames. This function was provided by Joseph Ramsey, and slightly
#' modified by Frederik Fabricius-Bjerre.
#'
#' @param df A data frame to be converted to a Tetrad Java object.
#'
#' @example inst/roxygen-examples/rdata_to_tetrad_data_example.R
#'
#' @return A Tetrad Java object representing the data frame.
#'
#' @export
rdata_to_tetrad <- function(df) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava", "stats"
    ),
    function_name = "rdata_to_tetrad"
  )

  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  nrows <- nrow(df)
  ncols <- ncol(df)

  # Create Java ArrayList<Node>
  var_list <- rJava::.jnew("java/util/ArrayList")

  # Prepare empty double[][] and int[][] (as Java arrays)
  cont_data <- vector("list", ncols)
  disc_data <- vector("list", ncols)

  # Sort numetric and integer columns
  numeric_cols <- sapply(df, is.numeric)
  integer_cols <- sapply(df, is.integer)
  numeric_cols <- !integer_cols & numeric_cols
  if (!all(numeric_cols | integer_cols)) {
    stop(
      "Data frame contains non-numeric columns or something went wrong with",
      "the identification of discrete columns."
    )
  }
  for (j in seq_len(ncols)) {
    name <- colnames(df)[j]
    col <- df[[j]]

    if (numeric_cols[j]) {
      variable <- rJava::.jnew("edu/cmu/tetrad/data/ContinuousVariable", name)
      node <- rJava::.jcast(variable, "edu/cmu/tetrad/graph/Node")
      rJava::.jcall(
        var_list, "Z", "add", rJava::.jcast(node, "java/lang/Object")
      )
      cont_data[[j]] <- rJava::.jarray(as.numeric(col), dispatch = TRUE)
      disc_data[[j]] <- rJava::.jnull("[I") # null int[] for discrete
    } else if (integer_cols[j]) {
      num_categories <- length(unique(stats::na.omit(col)))
      variable <- rJava::.jnew(
        "edu/cmu/tetrad/data/DiscreteVariable", name, as.integer(num_categories)
      )
      node <- rJava::.jcast(variable, "edu/cmu/tetrad/graph/Node")
      rJava::.jcall(
        var_list, "Z", "add", rJava::.jcast(node, "java/lang/Object")
      )
      cont_data[[j]] <- rJava::.jnull("[D") # null double[] for continuous
      disc_data[[j]] <- rJava::.jarray(as.integer(col), dispatch = TRUE)
    } else {
      # extra safety precaution
      stop(paste("Unsupported column:", name, "with type: ", class(col))) # nocov
    }
  }

  # Convert R lists of arrays to Java double[][] and int[][]
  j_cont_data <- rJava::.jarray(cont_data, dispatch = TRUE)
  j_disc_data <- rJava::.jarray(disc_data, dispatch = TRUE)

  # Call static Java helper method
  ds <- rJava::.jcall(
    "edu.cmu.tetrad.util.DataSetHelper",
    "Ledu/cmu/tetrad/data/DataSet;",
    "fromR",
    rJava::.jcast(var_list, "java.util.List"),
    as.integer(nrows),
    rJava::.jcast(j_cont_data, "[[D"),
    rJava::.jcast(j_disc_data, "[[I")
  )

  return(ds)
}

#' Convert a Tetrad Java DataSet to an R data frame
#'
#' @description
#' Converts a Tetrad `DataSet` (Java) into a base R data frame, preserving
#' variable names and mapping variable kinds to matching R types.
#'
#' @details
#' Continuous variables become `double`, discrete variables become `integer`.
#' Missing values (`null`, `Double.NaN`, or `Integer.MIN_VALUE`) are converted
#' to the corresponding typed `NA`. Unknown variable kinds fall back to
#' character with `NA_character_` for missing entries.
#'
#' The JVM must be initialized and Tetrad classes available on the class path.
#'
#' @param data A Java object of class `edu.cmu.tetrad.data.DataSet`.
#'
#' @example inst/roxygen-examples/rdata_to_tetrad_data_example.R
#'
#' @return
#' A data frame with the same dimensions and names as `data`.
#'
#' @export
tetrad_data_to_rdata <- function(data) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava", "stats"
    ),
    function_name = "tetrad_data_to_rdata"
  )

  # names
  names_list <- rJava::.jcall(data, "Ljava/util/List;", "getVariableNames")
  num_vars <- rJava::.jcall(data, "I", "getNumColumns")
  var_names <- character(num_vars)

  for (i in seq_len(num_vars) - 1L) {
    jstr <- rJava::.jcall(
      names_list, "Ljava/lang/Object;", "get", as.integer(i)
    )
    var_names[i + 1L] <- as.character(
      rJava::.jcall(jstr, "Ljava/lang/String;", "toString")
    )
  }

  n <- rJava::.jcall(data, "I", "getNumRows")
  cols <- vector("list", num_vars)

  for (j in seq_len(num_vars) - 1L) {
    node <- rJava::.jcall(
      data,
      "Ledu/cmu/tetrad/graph/Node;", "getVariable",
      as.integer(j)
    )
    is_discrete <- rJava::.jinstanceof(
      node,
      "edu/cmu/tetrad/data/DiscreteVariable"
    )
    is_cont <- rJava::.jinstanceof(
      node,
      "edu/cmu/tetrad/data/ContinuousVariable"
    )

    # preallocate target R vector by type to keep classes correct
    if (is_cont) {
      v <- numeric(n)
    } else if (is_discrete) {
      v <- integer(n)
    } else {
      # unknown type fallback
      v <- character(n) # nocov
    }

    for (r in seq_len(n) - 1L) {
      obj <- rJava::.jcall(
        data, "Ljava/lang/Object;", "getObject", as.integer(r), as.integer(j)
      )

      is_null <- isTRUE(rJava::is.jnull(obj))
      is_double <- isTRUE(rJava::.jinstanceof(
        obj, "java/lang/Double"
      ))
      is_integer <- isTRUE(rJava::.jinstanceof(
        obj, "java/lang/Integer"
      ))

      dbl_val <- if (is_double) {
        rJava::.jcall(obj, "D", "doubleValue")
      } else {
        NA_real_
      }
      int_val <- if (is_integer) {
        rJava::.jcall(obj, "I", "intValue")
      } else {
        NA_integer_
      }

      miss_double <- is_double && isTRUE(is.nan(dbl_val))
      miss_integer <- is_integer &&
        isTRUE(is.na(int_val) ||
          int_val == .Machine$integer.min)

      is_missing <- isTRUE(is_null || miss_double || miss_integer)

      if (is_missing) {
        if (is_cont) {
          v[r + 1L] <- NA_real_
        } else if (is_discrete) {
          v[r + 1L] <- NA_integer_
        } else {
          # extra safety precaution
          v[r + 1L] <- NA_character_ # nocov
        }
        next
      }

      if (is_double) {
        v[r + 1L] <- dbl_val
      } else if (is_integer) {
        v[r + 1L] <- int_val
      } else {
        # extra safety precaution
        # nocov start
        s <- rJava::.jcall(obj, "Ljava/lang/String;", "toString")
        v[r + 1L] <- if (isTRUE(is.na(s))) NA_character_ else as.character(s)
        # nocov end
      }
    }
    cols[[j + 1L]] <- v
  }
  stats::setNames(as.data.frame(cols, optional = TRUE), var_names)
}
