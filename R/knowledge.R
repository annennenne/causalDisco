# tetrad part
#' @export
knowledge_tetrad <- function(...) {
  knowledge <- .jnew("edu/cmu/tetrad/data/Knowledge")

  # tetrad helper functions defined locally
  tier <- function(...) {
    args <- list(...)
    if (length(args) %% 2 != 0) {
      stop("The tier function requires an even number of arguments: each tier number must be paired with its variable(s).")
    }
    for (i in seq(1, length(args), by = 2)) {
      tier_val <- as.integer(args[[i]])
      vars <- as.character(args[[i + 1]])
      for (v in vars) {
        knowledge$addToTier(tier_val, v)
      }
    }
  }

  forbidden <- function(...) {
    args <- unlist(list(...))
    if (length(args) %% 2 != 0) {
      stop("The 'forbidden' function requires an even number of strings to form pairs (source and target).")
    }
    for (i in seq(1, length(args), by = 2)) {
      knowledge$setForbidden(args[i], args[i + 1])
    }
  }

  required <- function(...) {
    args <- unlist(list(...))
    if (length(args) %% 2 != 0) {
      stop("The 'required' function requires an even number of strings to form pairs (source and target).")
    }
    for (i in seq(1, length(args), by = 2)) {
      knowledge$setRequired(args[i], args[i + 1])
    }
  }

  # Create a local environment that binds "knowledge" and helper functions.
  local_env <- new.env(parent = parent.frame())
  local_env$knowledge <- knowledge
  local_env$tier <- tier
  local_env$forbidden <- forbidden
  local_env$required <- required

  # Process the expressions passed to knowledge_tetrad.
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
# bnlearn part
knowledge_bnlearn <- function() {
  stop("Not implemented yet.")
}
