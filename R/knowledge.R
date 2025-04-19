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
