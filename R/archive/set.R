
#' List that requires all elements to be of the same class and unique
#' @import R6
Set <- R6::R6Class("Set",
                public = list(
                   initialize = function(type, ...) {
                      private$type <- type
                      if (length(list(...)) > 0) {
                        input <- unique(private$checked_input(...))
                        private$content <- input
                      }
                   },
                   add = function(...) {
                     input <- private$checked_input(...)
                     private$content <- unique(c(private$content, input))
                     invisible(self)
                   },
                   remove = function(...) {
                     input <- list(...)
                     private$content <- setdiff(private$content, input)
                     invisible(self)
                   },
                   get = function() {
                     private$content
                   },
                   class = function() {
                     private$type
                   }
                ),
                private = list(
                  content = list(),
                  type = NULL,
                  checked_input = function(...) {
                    input <- list(...)
                    if (!all(private$type %in% sapply(input, class))) {
                        stop(paste("Not all elements are of class", private$type))
                    }
                    input
                  }
                )
)


a <- Set$new("numeric", 1, 2, 2)
b <- Set$new("numeric")
a$get()
b$get()
