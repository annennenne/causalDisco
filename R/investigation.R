#' @imports R6
#' @include log.R theory.R observations.R scm.R
#' @export
Investigation <- R6::R6Class("Investigation",
                         public = list(
                           initialize = function(theory = NULL, observations = NULL, model = NULL) {
                             private$log$add_entry(paste("Initialized investigation with theory: ",
                                                         deparse(substitute(theory)),
                                                         ", observations: ",
                                                         deparse(substitute(observations)),
                                                         ", model: ",
                                                         deparse(substitute(model))))
                             if (is.null(theory)) theory <- Theory$new()
                             if (is.null(observations)) observations <- Observations$new()
                             if (is.null(model)) model <- SCM$new()

                             private$theory_internal <- theory
                             private$observations_internal <- observations
                             private$model_internal <- model
                           },
                           get_log = function(object = FALSE) {
                             if (object) return(private$log)
                             else return(private$log$get())
                           },
                           log_add_comment = function(string) {
                             private$log$add_entry(string)
                             invisible(self)
                           },
                           get_theory = function() {
                             private$theory_internal
                           },
                           get_observations = function() {
                             private$observations_internal
                           },
                           get_model = function() {
                             private$model_internal()
                           }
                         ),
                         active = list(
                           theory = function(value) {
                              #called to access
                              if (missing(value)) {
                                return(private$theory_internal)
                              } else { #called as replacement
                                private$log$add_entry(paste("Theory changed to:",
                                                         "????"))
                                  #How to refer to rhs of replacement?
                                private$theory_internal <- value
                                return(invisible(self))
                              }
                           }
                         ),
                         private = list(
                           log = Log$new(),
                           theory_internal = NULL,
                           observations_internal = NULL,
                           model_internal = NULL
                         )
)

mm <- Investigation$new(theory = Theory$new(),
                        observations = Observations$new(variables = c("y", "x", "z")),
                        model = SCM$new())

View(mm$get_log(FALSE))
mm$theory <- Theory$new(assumptions = Assumption$new("no unobs"))
mm$theory$get_assumptions()
