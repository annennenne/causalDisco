#' @imports R6
#' @include set.R
#' @export
Theory <- R6::R6Class("Theory",
                  public = list(
                    initialize = function(assumptions = NULL, axioms = NULL) {
                      private$assumptions <- private$input_to_set(assumptions, "Assumption",
                                                          "\'assumptions\'")
                      private$axioms <- private$input_to_set(axioms, "Axiom", "\'axioms\'")
                    },
                    get_assumptions = function() {
                      private$assumptions$get()
                    },
                    add_assumption = function(assumption) {
                      private$assumptions$add(assumption)
                      invisible(self)
                    },
                    remove_assumption = function(assumption) {
                      private$assumptions$remove(assumption)
                      invisible(self)
                    },
                    get_axioms = function() {
                      private$axioms$get()
                    },
                    add_axiom = function(axiom) {
                      private$axioms$add(axiom)
                      invisible(self)
                    },
                    remove_axiom = function(assumption) {
                      private$axioms$remove(axiom)
                      }
                  ),
                  private = list(
                    assumptions = NULL,
                    axioms = NULL,
                    input_to_set = function(x, type, purpose) {
                      if (!is.null(x)) {
                        wrongClass <- TRUE
                        thisClass <- class(x)
                        if (type %in% thisClass) {
                          output <- Set$new(type, x)
                          wrongClass <- FALSE
                        } else if ("Set" %in% thisClass) {
                          if (x$class() == type) {
                            output <- x
                            wrongClass <- FALSE
                          }
                        }
                        if (wrongClass) {
                          stop(paste(purpose, "must be (possibly a Set) of class", type))
                        }
                      } else {
                        output <- Set$new(type)
                      }
                      output
                    }
                  )
)


ab <- Theory$new(assumptions = Assumption$new("no unobs"), Axiom$new(y ~ x))
ab$get_assumptions()

ab
Axiom$new(y ~ x + z)
