library(rJava)
# source("/home/fabben/BioStat/causalDisco/tetrad/java_r_functions.R")
if (!.jniInitialized) {
  jar_path <- "tetrad/tetrad-current.jar"
  .jinit(parameters = "-Xmx2g", classpath = jar_path)
}

rdata_to_tetrad <- function(df, int_as_cont = FALSE) {
  # Divide into different types if useful for later.
  # For now, we'll treat all non-numeric columns as discrete/categorical.
  numeric_cols <- sapply(df, is.numeric)
  integer_cols <- sapply(df, is.integer)
  logical_cols <- sapply(df, is.logical) # Not used in this function as of now.
  complex_cols <- sapply(df, is.complex) # Not used in this function as of now.

  # If int_as_cont is TRUE, treat integer columns as continuous.
  if (int_as_cont) {
    numeric_cols <- numeric_cols | integer_cols
    integer_cols <- integer_cols & !integer_cols
  }
  discrete_cols <- names(df)[!(numeric_cols)]

  # For each discrete column, map its unique values to integer codes.
  category_map <- lapply(discrete_cols, function(col) {
    vals <- unique(df[[col]])
    setNames(seq_along(vals) - 1, vals)
  })

  names(category_map) <- discrete_cols

  df_conv <- df
  # Convert discrete columns to integers.
  for (col in discrete_cols) {
    df_conv[[col]] <- as.integer(category_map[[col]][as.character(df[[col]])])
  }

  values <- as.matrix(df_conv)
  n <- nrow(df) |> as.integer()
  p <- ncol(df) |> as.integer()

  # Java tetrad string
  tetrad_data_dir <- "edu/cmu/tetrad/data/"

  variables <- .jnew("java/util/ArrayList")
  for (col in colnames(df)) {
    if (col %in% discrete_cols) {
      categories <- .jnew("java/util/ArrayList")
      for (category in names(category_map[[col]])) {
        .jcall(categories, "V", "add", as.character(category))
      }
      var <- .jnew(paste0(tetrad_data_dir, "DiscreteVariable"), col, categories)
    } else {
      var <- .jnew(paste0(tetrad_data_dir, "ContinuousVariable"), col)
    }
    # Cast var to java/lang/Object so that the signature matches
    .jcall(variables, "Z", "add", .jcast(var, "java/lang/Object"))
  }

  if (length(discrete_cols) == p) {
    databox_type <- paste0(tetrad_data_dir, "IntDataBox")
    databox <- .jnew(databox_type, n, p)
  } else if (length(discrete_cols) == 0) {
    databox_type <- paste0(tetrad_data_dir, "DoubleDataBox")
    databox <- .jnew(databox_type, n, p)
  } else {
    databox_type <- paste0(tetrad_data_dir, "MixedDataBox")
    databox <- .jnew(databox_type, variables, n)
  }

  for (col in 1:p) {
    for (row in 1:n) {
      valueObj <- .jnew("java/lang/Double", as.double(values[[row, col]]))
      valueObj <- .jcast(valueObj, "java/lang/Number")
      .jcall(
        databox,
        "V",
        "set",
        as.integer(row - 1),
        as.integer(col - 1),
        valueObj
      )
    }
  }

  # Ensure your variables list is cast to java.util.List
  variablesList <- .jcast(variables, "java/util/List")
  # Also, cast databox to the DataBox interface.
  databox <- .jcast(databox, "edu/cmu/tetrad/data/DataBox")
  dataset <- .jnew("edu/cmu/tetrad/data/BoxDataSet", databox, variablesList)
  dataset <- cast_obj(dataset)
  return(dataset)
}

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
    nullEpt = 0,
    circleEpt = 1,
    arrowEpt = 2,
    tailEpt = 3) {
  endpoint_map <- list(
    "NULL" = nullEpt,
    "CIRCLE" = circleEpt,
    "ARROW" = arrowEpt,
    "TAIL" = tailEpt
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

## Convert a Tetrad matrix (a Java array-like object) to an R numeric matrix.
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

## Convert a Tetrad matrix to an R data.frame. 'variables' is assumed to be a Java List of variables.
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

## Convert an R square integer matrix (with only 0's and 1's, where a[i,j]==1 means j -> i)
## into a Tetrad Java graph object.
adj_matrix_to_graph <- function(adjMatrix) {
  dims <- dim(adjMatrix)
  if (dims[1] != dims[2]) {
    stop("The matrix is not square. Rows and columns must be equal.")
  }
  n <- dims[1]
  variable_names <- paste0("X", 1:n)
  variables <- .jnew("java/util/ArrayList")
  for (i in 1:n) {
    # Create a GraphNode (assumed to be the correct class in Tetrad)
    node <- .jnew("edu/cmu/tetrad/graph/GraphNode", variable_names[i])
    .jcall(variables, "V", "add", node)
  }
  # Create an EdgeListGraph given the variables.
  graph <- .jnew("edu/cmu/tetrad/graph/EdgeListGraph", variables)

  for (i in 1:n) {
    for (j in 1:n) {
      if (adjMatrix[i, j] != 0) {
        # Add a directed edge from variables[i] to variables[j].
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

  # Add nodes: iterate over nodes of the Tetrad graph.
  nodes <- .jcall(g, "Ljava/util/List;", "getNodes")
  numNodes <- .jcall(g, "I", "getNumNodes")
  for (i in 0:(numNodes - 1)) {
    node <- .jcall(nodes, "Ljava/lang/Object;", "get", as.integer(i))
    name <- as.character(.jcall(node, "Ljava/lang/Object;", "getName"))
    # Here we assume gdot has a method 'node' that accepts a name and additional attributes.
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

    # Assuming gdot has an 'edge' method.
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
