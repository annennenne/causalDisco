#' @export
knowledge <- function(engine) {
  engine <- tolower(engine)

  switch(engine,
    tetrad = knowledge_tetrad(test, alpha, ...),
    pcalg = knowledge_pcalg(test, alpha, ...),
    bnlearn = knowledge_bnlearn(test, alpha, ...),
    stop("The knowledge constructor is unsupported for the engine: ", engine)
  )
}

# tetrad part
tier_tetrad <- function(...) {
  # Retrieve all the arguments as a list.
  args <- list(...)

  # Ensure that the number of arguments is even.
  if (length(args) %% 2 != 0) {
    stop("The tier function requires an even number of arguments: each tier number must be paired with its variable(s).")
  }

  # Loop through the arguments pairwise.
  for (i in seq(1, length(args), by = 2)) {
    # Convert the tier value to an integer.
    tier_val <- as.integer(args[[i]])

    # Get the corresponding variables and ensure they are treated as a character vector.
    vars <- as.character(args[[i + 1]])

    # For each variable in the provided vector, add it to the specified tier.
    for (v in vars) {
      knowledge$addToTier(tier_val, v)
    }
  }
}

# Function to add forbidden edges to the knowledge object.
forbidden_tetrad <- function(...) {
  # Combine all arguments into a single character vector.
  args <- unlist(list(...))

  # Check that the number of elements is even.
  if (length(args) %% 2 != 0) {
    stop("The 'forbidden' function requires an even number of strings to form pairs (source and target).")
  }

  # Process each pair and add as a forbidden edge.
  for (i in seq(1, length(args), by = 2)) {
    # Call the Tetrad API method setForbidden(source, target)
    knowledge$setForbidden(args[i], args[i + 1])
  }
}

# Function to add required edges to the knowledge object.
required_tetrad <- function(...) {
  # Combine all arguments into a single character vector.
  args <- unlist(list(...))

  # Check that the number of elements is even.
  if (length(args) %% 2 != 0) {
    stop("The 'required' function requires an even number of strings to form pairs (source and target).")
  }

  # Process each pair and add as a required edge.
  for (i in seq(1, length(args), by = 2)) {
    # Call the Tetrad API method setRequired(source, target)
    knowledge$setRequired(args[i], args[i + 1])
  }
}
knowledge_tetrad <- function(...) {
  knowledge <- .jnew("edu/cmu/tetrad/data/Knowledge")

  # Create a local environment that binds "knowledge" to our new object.
  local_env <- new.env(parent = parent.frame())
  local_env$knowledge <- knowledge
  exprs <- as.list(substitute(list(...)))[-1]
  allowed_fns <- c("tier", "forbidden", "required")
  for (expr in exprs) {
    if (!is.call(expr)) {
      stop("All arguments to knowledge_tetrad() must be calls to tier, forbidden, or required.")
    }
    fn_name <- as.character(expr[[1]])
    if (!(fn_name %in% allowed_fns)) {
      stop("Only calls to tier, forbidden, and required are permitted in knowledge_tetrad().")
    }
  }

  for (expr in exprs) {
    eval(expr, envir = local_env)
  }

  return(knowledge)
}

# todo
# pcalg part
knowledge_pcalg <- function() {
  stop("Not implemented yet.")
}

# todo
# bnlearn parat
knowledge_bnlearn <- function() {
  stop("Not implemented yet.")
}
