#' @imports R6
Restriction <- R6::R6Class("Restriction",
                        public = list(
                          initialize = function(name, type) {
                            private$type <- type
                            supported <- get(paste("supported", type, sep = ""))
                            if (name %in% supported$name) {
                              place <- which(name == supported$name)
                              private$name <- name
                              private$description <- supported$description[place]
                              private$id <- supported$id[place]
                            } else {
                              stop(paste(name, "is not a supported", name))
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
                          id = NULL,
                          type = NULL
                        )
)

