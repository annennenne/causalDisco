#' @imports R6
#' @export
Assumption <- R6::R6Class("Assumption",
                      public = list(
                        initialize = function(name) {
                          if (name %in% supportedAssumptions$name) {
                            place <- which(name == supportedAssumptions$name)
                            private$name <- name
                            private$description <- supportedAssumptions$description[place]
                            private$id <- supportedAssumptions$id[place]
                          } else {
                            stop(paste(name, "is not a supported assumption."))
                          }
                        },
                        get_name = function() {
                          private$name
                        },
                        get_description = function() {
                          private$description
                        },
                        get_id = function() {
                          private$id
                        }
                      ),
                      private = list(
                        name = NULL,
                        description = NULL,
                        id = NULL
                      )
)


###############################################################################################
###Not exported below #########################################################################
###############################################################################################

supportedAssumptions <- data.frame(name = "no unobs",
                                   description = "No unobserved variables (causal sufficiency)",
                                   id = 1)
