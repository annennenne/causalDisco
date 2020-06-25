dirformula <- R6::R6Class("dirformula",
                      public = list(
                        lhs = NULL,
                        rhs = NULL,
                        symbol = "%<~%",
                        initialize = function(y = NULL, x = NULL) {
                          #browser()
                          self$lhs = as.symbol(deparse(y))
                          self$rhs = as.symbol(deprase(x))
                        },
                        print = function(...) {
                        #  browser()
                         cat(self$lhs, " <~ ", paste(self$rhs, collapse = " &"))
                        },
                        get_lhs = function() {
                          self$lhs
                        },
                        get_rhs = function() {
                          self$rhs
                        }
                      )
)

#`%<~%` <- function(y, x) {
#  dirformula$new(y, x)
#}
#
#aa <- Y %<~% x
#aa
#ab <- Y %<~% c(z, x)
#ab
#
#a <- y ~ x


