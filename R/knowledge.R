KnowledgeObj <- R6Class("KnowledgeObj",
  public = list(
    tiers = NULL,
    forbidden = NULL,
    required = NULL,
    initialize = function() {
      self$tiers <- list()
      self$forbidden <- list()
      self$required <- list()
    },
    add_to_tier = function(tier, vars) {
      tier <- as.character(tier) # Use the tier number as a character key.
      vars <- as.character(vars)
      if (is.null(self$tiers[[tier]])) {
        self$tiers[[tier]] <- vars
      } else {
        self$tiers[[tier]] <- unique(c(self$tiers[[tier]], vars))
      }
      invisible(self)
    },
    add_forbidden = function(source, target) {
      forbidden_edge <- c(as.character(source), as.character(target))
      self$forbidden[[length(self$forbidden) + 1]] <- forbidden_edge
      invisible(self)
    },
    add_required = function(source, target) {
      required_edge <- c(as.character(source), as.character(target))
      self$required[[length(self$required) + 1]] <- required_edge
      invisible(self)
    },
    get_tetrad_knowledge = function() {
      tetrad_knowledge <- .jnew("edu/cmu/tetrad/data/Knowledge")

      if (length(self$tiers) > 0) {
        for (tier_key in names(self$tiers)) {
          tier_val <- as.integer(tier_key)
          vars <- self$tiers[[tier_key]]
          for (v in vars) {
            tetrad_knowledge$addToTier(tier_val, v)
          }
        }
      }

      if (length(self$forbidden) > 0) {
        for (edge in self$forbidden) {
          tetrad_knowledge$setForbidden(edge[1], edge[2])
        }
      }

      if (length(self$required) > 0) {
        for (edge in self$required) {
          tetrad_knowledge$setRequired(edge[1], edge[2])
        }
      }
      return(tetrad_knowledge)
    },
    print = function(...) {
      cat("Background Knowledge Object:\n")
      cat("Tiers:\n")
      print(self$tiers)
      cat("Forbidden Edges:\n")
      print(self$forbidden)
      cat("Required Edges:\n")
      print(self$required)
      invisible(self)
    }
  )
)


#' Creates a Knowledge Object by calling the function below.
#' Example:
#' my_knowledge <- knowledge(
#'   tier(
#'     1, c("V1", "V2", "V3"),
#'     2, c("V4", "V5", "V6")
#'   ),
#'   forbidden("V1", "V6"), # forbidden and required can be called like this
#'   forbidden(c("V2", "V6")), # or like this
#'   required(c("V1", "V2"),
#'            c("V2", "V3")) # or like this
#' )
#'
#' @export
knowledge <- function(...) {
  knowledge_obj <- KnowledgeObj$new()

  # Helper functions are defined locally. They are not callable outside.
  tier <- function(...) {
    args <- list(...)
    if (length(args) %% 2 != 0) {
      stop("The tier function requires an even number of arguments: each tier number must be paired with its variable(s).")
    }
    for (i in seq(1, length(args), by = 2)) {
      tier_val <- as.integer(args[[i]])
      vars <- as.character(args[[i + 1]])
      for (v in vars) {
        knowledge_obj$add_to_tier(tier_val, v)
      }
    }
  }

  forbidden <- function(...) {
    args <- unlist(list(...))
    if (length(args) %% 2 != 0) {
      stop("The 'forbidden' function requires an even number of strings to form pairs (source and target).")
    }
    for (i in seq(1, length(args), by = 2)) {
      knowledge_obj$add_forbidden(args[i], args[i + 1])
    }
  }

  required <- function(...) {
    args <- unlist(list(...))
    if (length(args) %% 2 != 0) {
      stop("The 'required' function requires an even number of strings to form pairs (source and target).")
    }
    for (i in seq(1, length(args), by = 2)) {
      knowledge_obj$add_required(args[i], args[i + 1])
    }
  }

  # Create a local environment that binds "knowledge" and helper functions.
  local_env <- new.env(parent = parent.frame())
  local_env$knowledge_obj <- knowledge_obj
  local_env$tier <- tier
  local_env$forbidden <- forbidden
  local_env$required <- required

  # Process the expressions passed to knowledge_tetrad.
  exprs <- as.list(substitute(list(...)))[-1]
  allowed_fns <- c("tier", "forbidden", "required")
  for (expr in exprs) {
    if (!is.call(expr)) {
      stop("All arguments to knowledge() must be calls to tier, forbidden, or required.")
    }
    fn_name <- as.character(expr[[1]])
    if (!(fn_name %in% allowed_fns)) {
      stop("Only calls to tier(), forbidden(), and required() are permitted in knowledge().")
    }
  }

  for (expr in exprs) {
    eval(expr, envir = local_env)
  }
  return(knowledge_obj)
}

check_knowledge_obj <- function(x) {
  if (!inherits(x, "KnowledgeObj")) {
    stop("Input must be a KnowledgeObj instance.")
  }
  TRUE
}
