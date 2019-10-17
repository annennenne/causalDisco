#' @imports R6
#' @export
Axiom <- R6::R6Class("Axiom",
                 public = list(
                  initialize = function(form, comment = NULL) {
                    if ("formula" %in% class(form)) {
                      private$formula <- form
                    } else stop("Input must be a formula")
                    if (!is.null(comment)) {
                      private$comment <- comment
                    }
                  },
                  get_formula = function() {
                    private$formula
                  },
                  get_comment = function() {
                    private$comment
                  },
                  set_comment = function(string) {
                    private$comment <- as.character(string)
                  }
                 ),
                 private = list(
                   formula = NULL,
                   comment = NULL
                 )
)

aa <- Axiom$new(y ~ x, "jazz")

aa$get_formula()
aa$set_comment("sdf")
aa$get_comment()
