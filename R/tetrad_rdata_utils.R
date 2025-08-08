#' @title Tetrad R Data Utilities
#'
#' @description \code{tetrad_rdata_utils} provides functions to convert between R data frames and Tetrad Java objects.
#'
#' @details This function is made to be used internally with the TetradSearch class.
#' The function will copy the data into the Java heap, so be careful with larger data frames.
#' This function was provided by Joseph Ramsey, and slightly modified by Frederik Fabricius-Bjerre.
#' @importFrom rJava .jnew .jcall .jarray .jnull .jcast
#' @param df A data frame to be converted to a Tetrad Java object.
#' @return A Tetrad Java object representing the data frame.
rdata_to_tetrad <- function(df) {
  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  nrows <- nrow(df)
  ncols <- ncol(df)

  # Create Java ArrayList<Node>
  var_list <- .jnew("java/util/ArrayList")

  # Prepare empty double[][] and int[][] (as Java arrays)
  cont_data <- vector("list", ncols)
  disc_data <- vector("list", ncols)

  # Sort numetric and integer columns
  numeric_cols <- sapply(df, is.numeric)
  integer_cols <- sapply(df, is.integer)
  numeric_cols <- !integer_cols & numeric_cols
  if (!all(numeric_cols | integer_cols)) {
    stop("Data frame contains non-numeric columns or something went wrong with the identification of discrete columns.")
  }
  for (j in seq_len(ncols)) {
    name <- colnames(df)[j]
    col <- df[[j]]

    if (numeric_cols[j]) {
      variable <- .jnew("edu/cmu/tetrad/data/ContinuousVariable", name)
      node <- .jcast(variable, "edu/cmu/tetrad/graph/Node")
      .jcall(var_list, "Z", "add", .jcast(node, "java/lang/Object"))
      cont_data[[j]] <- .jarray(as.numeric(col), dispatch = TRUE)
      disc_data[[j]] <- .jnull("[I") # null int[] for discrete
    } else if (integer_cols[j]) {
      num_categories <- length(unique(na.omit(col)))
      variable <- .jnew("edu/cmu/tetrad/data/DiscreteVariable", name, as.integer(num_categories))
      node <- .jcast(variable, "edu/cmu/tetrad/graph/Node")
      .jcall(var_list, "Z", "add", .jcast(node, "java/lang/Object"))
      cont_data[[j]] <- .jnull("[D") # null double[] for continuous
      disc_data[[j]] <- .jarray(as.integer(col), dispatch = TRUE)
    } else {
      stop(paste("Unsupported column:", name, "with type: ", class(col)))
    }
  }

  # Convert R lists of arrays to Java double[][] and int[][]
  j_cont_data <- .jarray(cont_data, dispatch = TRUE)
  j_disc_data <- .jarray(disc_data, dispatch = TRUE)

  # Call static Java helper method
  ds <- .jcall(
    "edu.cmu.tetrad.util.DataSetHelper",
    "Ledu/cmu/tetrad/data/DataSet;",
    "fromR",
    .jcast(var_list, "java.util.List"),
    as.integer(nrows),
    .jcast(j_cont_data, "[[D"),
    .jcast(j_disc_data, "[[I")
  )

  return(ds)
}

# doesn't work
tetrad_data_to_rdata <- function(data) {
  namesList <- .jcall(data, "Ljava/util/List;", "getVariableNames")
  numVars <- .jcall(data, "I", "getNumColumns")
  varNames <- character(numVars)
  for (i in 0:(numVars - 1)) {
    varNames[i + 1] <- as.character(.jcall(
      namesList,
      "Ljava/lang/Object;",
      "get",
      as.integer(i)
    )$toString())
  }

  n <- .jcall(data, "I", "getNumRows")
  df <- data.frame(matrix(nrow = n, ncol = numVars))
  colnames(df) <- varNames

  for (row in 0:(n - 1)) {
    for (col in 0:(numVars - 1)) {
      df[row + 1, col + 1] <- .jcall(
        data,
        "Ljava/lang/Object;",
        "getObject",
        as.integer(row),
        as.integer(col)
      )
    }
  }
  return(df)
}
