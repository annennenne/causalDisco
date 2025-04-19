#' @title KnowledgeObj R6 Class – Container for Background Knowledge
#'
#' @description
#' A lightweight R6 wrapper that stores user‑supplied background knowledge
#' (tiers of variables, *forbidden* edges and *required* edges) for use by
#' causal‑discovery algorithms.
#'
#' @details
#' A `KnowledgeObj` starts empty and is filled incrementally with helper
#' methods.  All mutators return the object invisibly, so calls may be
#' chained.  The class is designed to be used indirectly through the
#' higher‑level \code{\link{knowledge}()} helper, but it can be manipulated
#' directly if finer control is needed.
#'
#' @docType class
#' @format An \link[R6]{R6Class} generator object.
#' @name KnowledgeObj
#'
#' @examples
#' kn_obj <- KnowledgeObj$new()
#' kn_obj$add_to_tier(1, c("V1", "V2"))$
#'   add_forbidden("V1", "V3")$
#'   add_required("V2", "V3")
#' kn_obj
#'
#' @importFrom R6 R6Class
KnowledgeObj <- R6Class("KnowledgeObj",
  public = list(

    #' @field tiers
    #' A named \code{list}.  Each name is a tier integer (stored as a character
    #' string) and the value is a character vector of variable names assigned
    #' to that tier.
    tiers = NULL,

    #' @field forbidden
    #' A \code{list}.  Each element is a character vector of length 2
    #' \code{c(source, target)} representing a **forbidden** directed edge.
    forbidden = NULL,

    #' @field required
    #' A \code{list}.  Each element is a character vector of length 2
    #' \code{c(source, target)} representing a **required** directed edge
    #' (source → target).
    required = NULL,

    #' @description
    #' Create a new, empty \code{KnowledgeObj}.
    #' @return A new \code{KnowledgeObj} instance.
    initialize = function() {
      self$tiers <- list()
      self$forbidden <- list()
      self$required <- list()
    },

    #' @description
    #' Add one or more variables to a tier. If the tier does not yet exist it
    #' is created; duplicates are removed.
    #'
    #' @param tier `integer` | `numeric` | `character`
    #'   Tier number (will be coerced to character).
    #' @param vars `character`
    #'   Vector of variable names to add.
    #' @return The modified object (invisibly), so the method can be chained.
    add_to_tier = function(tier, vars) {
      tier <- as.character(tier) # Use the tier number as a character key
      vars <- as.character(vars)
      if (is.null(self$tiers[[tier]])) {
        self$tiers[[tier]] <- vars
      } else {
        self$tiers[[tier]] <- unique(c(self$tiers[[tier]], vars))
      }
      invisible(self)
    },

    #' @description
    #' Register a **forbidden** edge \code{source → target}.
    #'
    #' @param source `character`
    #'   Source variable name.
    #' @param target `character`
    #'   Target variable name.
    #' @return The modified object (invisibly).
    add_forbidden = function(source, target) {
      forbidden_edge <- c(as.character(source), as.character(target))
      self$forbidden[[length(self$forbidden) + 1]] <- forbidden_edge
      invisible(self)
    },

    #' @description
    #' Register a **required** edge \code{source → target}.
    #'
    #' @param source `character`
    #'   Source variable name.
    #' @param target `character`
    #'   Target variable name.
    #' @return The modified object (invisibly).
    add_required = function(source, target) {
      required_edge <- c(as.character(source), as.character(target))
      self$required[[length(self$required) + 1]] <- required_edge
      invisible(self)
    },

    #' @description
    #' Convert the internal R representation to a Java
    #' \code{edu.cmu.tetrad.data.Knowledge} object for use with Tetrad.
    #'
    #' @return A Java \code{Knowledge} object.
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
      tetrad_knowledge
    },

    #' @description
    #' Pretty‑print the contents of the object.
    #'
    #' @param ... Ignored; included for compatibility.
    #' @return The object (invisibly).
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

#' @title Construct a \code{KnowledgeObj} with a Mini‑DSL
#'
#' @description
#' \code{knowledge()} is a user‑friendly wrapper that builds a
#' \link{KnowledgeObj} by evaluating a sequence of
#' calls to the helper functions \code{tier()}, \code{forbidden()}, and
#' \code{required()}.
#' Each helper adds information to the underlying R6 object, which is then
#' returned for further use in causal‑discovery algorithms.
#'
#' @param ... One or more calls to \code{tier()}, \code{forbidden()}, or
#'   \code{required()}.  Any other call or a non‑call argument triggers an
#'   error.
#'
#' @details
#' \subsection{Helper functions}{
#'   \itemize{
#'     \item \strong{tier(tier, vars)} — Pairs a tier number with a character
#'       vector of variable names.  May be repeated.
#'     \item \strong{forbidden(source, target)} — Registers a forbidden edge
#'       \code{source → target}.  Accepts multiple pairs, either as separate
#'       arguments or as a single character vector of length 2 × n.
#'     \item \strong{required(source, target)} — Registers a required edge
#'       \code{source → target}.  Same calling conventions as
#'       \code{forbidden()}.
#'   }
#' }
#'
#' @return A populated \link{KnowledgeObj}.
#'
#' @examples
#' # number of samples
#' n <- 10**4

#' # continuous data example
#' V1 <- rnorm(n, 0, 1)
#' V2 <- 0.5 * V1 + rnorm(n, 0, 0.5)
#' V3 <- V2 + rnorm(n, 0, 0.1)
#' V4 <- V3 + rnorm(n, 0, 1)
#' V5 <- rnorm(n, 0, 1)
#' V6 <- rnorm(n, 0, 1) + 0.7 * V5
#'
#' df <- data.frame(V1, V2, V3, V4, V5, V6)
#'
#' # set knowledge
#' my_knowledge <- knowledge(
#'   tier(
#'     1, c("V1", "V2", "V3"),
#'     2, c("V4", "V5", "V6")
#'   ),
#'   forbidden("V1", "V6"), # single pair
#'   forbidden(c("V2", "V6")), # can be given as vector as well
#'   required(
#'     c("V1", "V2"), # two pairs can be given like this as well
#'     c("V2", "V3")
#'   )
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

#' @title Verify That an Object Is a \code{KnowledgeObj}
#'
#' @description
#' A small utility that throws an error if the supplied object is
#' not an instance of class \code{KnowledgeObj}.  Intended for internal use.
#'
#' @param x Any R object.
#'
#' @return \code{TRUE} (invisibly) if \code{x} inherits from
#'   \code{"KnowledgeObj"}; otherwise an error is raised.
#'
#' @examples
#' kg <- KnowledgeObj$new()
#' check_knowledge_obj(kg) # returns TRUE
#' \dontrun{
#' check_knowledge_obj(list()) # errors
#' }
#' @keywords internal
check_knowledge_obj <- function(x) {
  if (!inherits(x, "KnowledgeObj")) {
    stop("Input must be a KnowledgeObj instance.")
  }
  TRUE
}
