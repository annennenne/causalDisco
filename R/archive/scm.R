#' @importFrom R6 RCClass
#' @importFrom lava lvm regression regression<-
#' @export
SCM <- R6::R6Class("SCM", public = list(
                          initialize = function(connected = TRUE, observations = NULL, amatrix = NULL,
                                                formulas = NULL) {
                            if (!is.null(amatrix)) {
                             # browser()
                              private$model <- private$amatrix_to_model(amatrix)
                            } else if (!is.null(formulas)) {
                              private$model <- private$formulas_to_model(formulas)
                            } else if (!is.null(observations)) {
                              vars <- names(observations$get_data())
                              if (connected) {
                                private$model <- fully_connected_scm(vars)
                              } else {
                                private$model <- fully_unconnected_scm(vars)
                              }
                            } else {
                              private$model <- lava::lvm()
                            }
                          },
                          get_amatrix = function() {
                            private$model$M
                          },
                          get_equations = function() {
                            private$amatrix_to_formulas(private$model$M)
                          },
                          add_equation = function(equation) {
                            lava::regression(private$model) <- equation
                            invisible(self)
                          },
                          remove_equation = function(equation) {
                            cancel(private$model) <- equation
                            invisible(self)
                          },
                          get_model = function() private$model
                        ),
                        private = list(
                          model = NULL,
                          formulize = function(lhs, rhs) {
                            if (length(lhs) > 1) {
                              lhs <- paste("list(", paste(lhs, collapse = ","), ")")
                            }
                            rhs <- paste(rhs, collapse = "+")
                            as.formula(paste(lhs, "~", rhs))
                          },
                          fully_connected_scm = function(vars) {
                            m <- lava::lvm()
                            nVars <- length(vars)
                            for (i in 1:nVars) {
                              lvm::regression(m, quick = TRUE) <- private$formulize(vars[i], vars[-i])
                            }
                            m
                          },
                          fully_unconnected_scm = function(vars) {
                            m <- lava::lvm()
                            nVars <- length(vars)
                            for (i in 1:nVars) {
                              lvm::regression(m, quick = TRUE) <- private$formulize(vars[i], 1)
                            }
                            m
                          },
                          amatrix_to_formulas = function(M) {
                            out <- list()
                            n <- ncol(M)
                            vars <- colnames(M)

                            for (i in 1:n) {
                              thisCol <- M[,i]
                              if (sum(thisCol) > 0) {
                                out <- c(out, private$formulize(lhs = vars[i], rhs = vars[as.logical(thisCol)]))
                              } else out <- c(out, private$formulize(lhs = vars[i], rhs = 1))
                            }
                            out
                          },
                          amatrix_to_model = function(M) {
                            fs <- private$amatrix_to_formulas(M)
                            private$formulas_to_model(fs)
                          },
                          formulas_to_model = function(fs) {
                            n <- length(fs)
                            m <- lava::lvm()

                            for (i in 1:n) {
                              lava::regression(m, quick = TRUE) <- fs[[i]]
                            }
                            m
                          }
                        )
)




SCM$new()$get_amatrix()
bb <- SCM$new()$add_equation(y ~ x)
bb$get_amatrix()
bb$get_equations()






