#' @imports R6
CharStack <- R6::R6Class("CharStack",
                   public = list(
                     add = function(x) {
                       x <- tryCatch(as.character(x), error = function(e) NULL)
                       if (is.null(x)) {
                         stop("cannot coerce x to \'character\'")
                       } else private$contents <- c(x, private$contents)
                       invisible(self)
                     },
                     get = function(x) {
                       private$contents
                     }
                   ),
                   private = list(
                     contents = character()
                   )
)


c1 <- CharStack$new()
c1$add("a")
c2 <- CharStack$new()

c1$get()
c2$get()
