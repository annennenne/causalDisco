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

# Everything below does not work as of now.
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

## Convert a Java Tetrad graph to an R matrix (data.frame) with endpoints.
graph_to_matrix <- function(
    g,
    null_ept = 0,
    circle_ept = 1,
    arrow_ept = 2,
    tail_ept = 3) {
  endpoint_map <- list(
    "NULL" = null_ept,
    "CIRCLE" = circle_ept,
    "ARROW" = arrow_ept,
    "TAIL" = tail_ept
  )

  # Get nodes and number of nodes.
  nodes <- .jcall(g, "Ljava/util/List;", "getNodes")
  p <- .jcall(g, "I", "getNumNodes")

  # Create an integer matrix of zeros.
  A <- matrix(0L, nrow = p, ncol = p)

  # Get the list of edges.
  edges <- .jcall(g, "Ljava/util/List;", "getEdges")
  numEdges <- .jcall(edges, "I", "size")

  # For each edge, determine its endpoints.
  for (i in 0:(numEdges - 1)) {
    edge <- .jcall(edges, "Ljava/lang/Object;", "get", as.integer(i))
    # Get the nodes for the edge.
    node1 <- .jcall(edge, "Ljava/lang/Object;", "getNode1")
    node2 <- .jcall(edge, "Ljava/lang/Object;", "getNode2")
    # Find their positions in the nodes list.
    i_node <- .jcall(nodes, "I", "indexOf", node1)
    j_node <- .jcall(nodes, "I", "indexOf", node2)
    # Get endpoints (assume endpoints have a method name() returning a string)
    ep1 <- as.character(.jcall(
      edge,
      "Ljava/lang/Object;",
      "getEndpoint1"
    )$name())
    ep2 <- as.character(.jcall(
      edge,
      "Ljava/lang/Object;",
      "getEndpoint2"
    )$name())
    A[j_node + 1, i_node + 1] <- endpoint_map[[ep1]]
    A[i_node + 1, j_node + 1] <- endpoint_map[[ep2]]
  }

  # Build column names from the nodes.
  colNames <- character(p)
  for (i in 0:(p - 1)) {
    node <- .jcall(nodes, "Ljava/lang/Object;", "get", as.integer(i))
    colNames[i + 1] <- as.character(.jcall(
      node,
      "Ljava/lang/Object;",
      "toString"
    ))
  }

  return(as.data.frame(A, col.names = colNames))
}

tetrad_matrix_to_matrix <- function(array) {
  nrows <- .jcall(array, "I", "getNumRows")
  ncols <- .jcall(array, "I", "getNumColumns")
  mat <- matrix(0, nrow = nrows, ncol = ncols)
  for (i in 0:(nrows - 1)) {
    for (j in 0:(ncols - 1)) {
      mat[i + 1, j + 1] <- .jcall(
        array,
        "D",
        "get",
        as.integer(i),
        as.integer(j)
      )
    }
  }
  return(mat)
}

tetrad_matrix_to_rdata <- function(array, variables) {
  mat <- tetrad_matrix_to_matrix(array)
  ncols <- .jcall(array, "I", "getNumColumns")
  cols <- character(ncols)
  for (i in 0:(ncols - 1)) {
    cols[i + 1] <- as.character(.jcall(
      variables,
      "Ljava/lang/Object;",
      "get",
      as.integer(i)
    )$toString())
  }
  df <- as.data.frame(mat)
  colnames(df) <- cols
  return(df)
}

adj_matrix_to_graph <- function(adjMatrix) {
  dims <- dim(adjMatrix)
  if (dims[1] != dims[2]) {
    stop("The matrix is not square. Rows and columns must be equal.")
  }
  n <- dims[1]
  variable_names <- paste0("X", 1:n)
  variables <- .jnew("java/util/ArrayList")
  for (i in 1:n) {
    node <- .jnew("edu/cmu/tetrad/graph/GraphNode", variable_names[i])
    .jcall(variables, "V", "add", node)
  }
  graph <- .jnew("edu/cmu/tetrad/graph/EdgeListGraph", variables)

  for (i in 1:n) {
    for (j in 1:n) {
      if (adjMatrix[i, j] != 0) {
        .jcall(
          graph,
          "V",
          "addDirectedEdge",
          .jcall(variables, "Ljava/lang/Object;", "get", as.integer(i - 1)),
          .jcall(variables, "Ljava/lang/Object;", "get", as.integer(j - 1))
        )
      }
    }
  }
  return(graph)
}

## Given a GraphViz graph object (e.g. from DiagrammeR or other R package)
## write the graph by adding nodes and edges based on the Tetrad graph.
## This function expects 'g' to be a Tetrad graph object and 'gdot' to be a GraphViz object.
write_gdot <- function(g, gdot) {
  endpoint_map <- list("TAIL" = "none", "ARROW" = "empty", "CIRCLE" = "odot")

  nodes <- .jcall(g, "Ljava/util/List;", "getNodes")
  numNodes <- .jcall(g, "I", "getNumNodes")
  for (i in 0:(numNodes - 1)) {
    node <- .jcall(nodes, "Ljava/lang/Object;", "get", as.integer(i))
    name <- as.character(.jcall(node, "Ljava/lang/Object;", "getName"))
    gdot$node(
      name,
      shape = "circle",
      fixedsize = "true",
      style = "filled",
      color = "lightgray"
    )
  }

  # Add edges.
  edges <- .jcall(g, "Ljava/util/List;", "getEdges")
  numEdges <- .jcall(edges, "I", "size")
  for (i in 0:(numEdges - 1)) {
    edge <- .jcall(edges, "Ljava/lang/Object;", "get", as.integer(i))
    node1 <- as.character(.jcall(
      .jcall(edge, "Ljava/lang/Object;", "getNode1"),
      "Ljava/lang/Object;",
      "getName"
    ))
    node2 <- as.character(.jcall(
      .jcall(edge, "Ljava/lang/Object;", "getNode2"),
      "Ljava/lang/Object;",
      "getName"
    ))
    endpoint1 <- as.character(.jcall(
      .jcall(edge, "Ljava/lang/Object;", "getEndpoint1"),
      "Ljava/lang/Object;",
      "name"
    ))
    endpoint2 <- as.character(.jcall(
      .jcall(edge, "Ljava/lang/Object;", "getEndpoint2"),
      "Ljava/lang/Object;",
      "name"
    ))
    arrowtail <- endpoint_map[[endpoint1]]
    arrowhead <- endpoint_map[[endpoint2]]
    color <- "blue"
    if (arrowtail == "empty" && arrowhead == "empty") color <- "red"

    gdot$edge(
      node1,
      node2,
      arrowtail = arrowtail,
      arrowhead = arrowhead,
      dir = "both",
      color = color
    )
  }
  return(gdot)
}

## Print a Java graph object (using its toString() method)
print_java <- function(java_graph) {
  cat(.jcall(java_graph, "S", "toString"), "\n")
}
