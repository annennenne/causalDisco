#' @imports R6
#' @export
DataProperty <- R6::R6Class("DataProperty",
                      public = list(
                        initialize = function(name) {
                          if (name %in% supportedDataProperties$name) {
                            place <- which(name == supportedDataProperties$name)
                            private$name <- name
                            private$description <- supportedDataProperties$description[place]
                            private$id <- supportedDataProperties$id[place]
                          } else {
                            stop(paste(name, "is not a supported data property."))
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

#DataProperty <- R6Class("DataProperty",
#                        inherit = Restriction$new(type = "DataProperty")
#)

supportedDataProperties <- data.frame(name = "num data",
                                    description = "Numerical data (numeric/integer/Date variables)",
                                    id = 1)
