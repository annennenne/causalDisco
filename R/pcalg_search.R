#' Class for pcalg search algorithms
#'
#' This class implements the search algorithms from the pcalg package.
#' It allows to set the data, sufficient statistics, test, score, and algorithm.
#'
#' @importFrom pcalg pc fci ges gaussCItest binCItest disCItest
#' @export pcalgSearch
pcalgSearch <- R6Class(
  "pcalgSearch",
  public = list(
    #' @field data A `data.frame` holding the data set currently attached to the
    #' search object. Can be set with `set_data()`.
    data = NULL,

    #' @field score A function that will be used to build the score,
    #'  when data is set. Can be set with \code{$set_score()}. Recognized values
    #'  are:
    #'  \itemize{
    #'     \item \code{sem_bic} - BIC score for Gaussian data. See \code{\link[pcalg:GaussL0penObsScore-class]{GaussL0penObsScore}}.
    #'     \item \code{sem_bic_int} - BIC score for Gaussian data with integer values. See \code{\link[pcalg:GaussL0penIntScore-class]{GaussL0penIntScore}}.
    #'     }
    score = NULL,

    #' @field test A function that will be used to test independence.
    #'  Can be set with \code{$set_test()}. Recognized values are:
    #'  \itemize{
    #'    \item \code{fisher_z} - Fisher Z test for Gaussian data. See \code{\link[pcalg:gaussCItest]{gaussCItest}}.
    #'    \item \code{g_square} - G square test for discrete data. See \code{\link[pcalg:binCItest]{binCItest}} and \code{\link[pcalg:disCItest]{disCItest}}.
    #'    }
    test = NULL,

    #' @field alg A function that will be used to run the search algorithm.
    #' Can be set with \code{$set_alg()}. Recognized values are:
    #' \itemize{
    #'   \item \code{pc} - PC algorithm. See \code{\link[pcalg:pc]{pc}}.
    #'   \item \code{fci} - FCI algorithm. See \code{\link[pcalg:fci]{fci}}.
    #'   \item \code{ges} - GES algorithm. See \code{\link[pcalg:ges]{ges}}.
    #'   }
    alg = NULL,

    #' @field params A list of parameters for the test and algorithm.
    #' Can be set with \code{$set_params()}.
    #' The parameters are passed to the test and algorithm functions.
    params = NULL,

    #' @field suff_stat Sufficient statistic. The format and contents of the
    #' sufficient statistic depends on which test is being used.
    suff_stat = NULL,

    #' @field continuous Logical; whether the sufficient statistic is for a
    #' continuous test.
    continuous = NULL,

    #' @field knowledge A list of fixed constraints for the search algorithm.
    knowledge = NULL,

    #' @description
    #' Constructor for the `pcalgSearch` class.
    initialize = function() {
      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$knowledge <- NULL
      self$params <- NULL
    },

    #' @description
    #' Sets the parameters for the test and algorithm.
    #'
    #' @param params A list of parameters to set.
    set_params = function(params) {
      self$params <- params
    },

    #' @description
    #' Sets the data for the search algorithm.
    #'
    #' @param data A `data.frame` or a `matrix` containing the data.
    #' @param set_suff_stat Logical; whether to set the sufficient statistic
    #' for the data.
    set_data = function(data, set_suff_stat = TRUE) {
      self$data <- data
      if (set_suff_stat) {
        self$set_suff_stat()
      }

      # Reset uniques, used to determine binary (or not) G square test
      private$uniques <- c()

      # todo: Should it be changed or not?
      # Reset knowledge function
      # private$knowledge_function <- NULL
    },

    #' @description
    #' Sets the sufficient statistic for the data.
    set_suff_stat = function() {
      if (is.null(self$data)) {
        stop("Data must be set before sufficient statistic.",
          call. = FALSE
        )
      }
      if (is.null(self$test)) {
        stop("Test must be set before sufficient statistic.",
          call. = FALSE
        )
      }
      if (is.null(self$continuous)) {
        stop("The pcalgSearch class does not have knowledge on whether the
             sufficient statistic is for a continuous or discrete test.
             Please set test using set_test() or set continuous directly
             by self$continuous <- TRUE/FALSE.",
          call. = FALSE
        )
      }
      if (self$continuous) {
        if (is.matrix(self$data) || is.data.frame(self$data)) {
          self$suff_stat <- list(C = cor(self$data), n = nrow(self$data))
        } else {
          stop("Data must be a matrix or data frame if numeric.",
            call. = FALSE
          )
        }
      } else if (is.data.frame(self$data)) {
        self$suff_stat <- list(dm = data.matrix(self$data), adaptDF = FALSE)
      } else {
        stop("Unrecognized data format.
             The data should be either continouos or discrete,
             and the data should be in a data.frame.",
          call. = FALSE
        )
      }
    },

    #' @description
    #' Sets the test for the search algorithm.
    #'
    #' @param method A string specifying the type of test to use.
    #' @param alpha Significance level for the test.
    set_test = function(method,
                        alpha = NULL) {
      if (!is.null(alpha)) {
        self$params$alpha <- alpha
      }

      method <- tolower(method)
      switch(method,
        "fisher_z" = {
          if (is.null(self$params$alpha)) {
            stop("Alpha must be set before test.",
              call. = FALSE
            )
          }
          self$test <- pcalg::gaussCItest
          self$continuous <- TRUE
        },
        "g_square" = {
          if (is.null(self$params$alpha)) {
            stop("Alpha must be set before test.",
              call. = FALSE
            )
          }
          self$test <- private$use_g_square()
          self$continuous <- FALSE
        },
        stop("Unknown test type using pcalg engine: ", method,
          call. = FALSE
        )
      )
    },

    #' @description
    #' Sets the score for the search algorithm.
    #'
    #' @param method A string specifying the type of score to use.
    #' @param params A list of parameters to pass to the score function.
    set_score = function(method, params = list()) {
      method <- tolower(method)
      # Function that will be used to build the score, when data is set
      return_pcalg_score <- function() {
        if (is.null(self$data)) {
          stop("Data must be set before score.",
            call. = FALSE
          )
        }
        switch(method,
          "sem_bic" = {
            score <- rlang::exec(
              "new",
              Class = "GaussL0penObsScore",
              data  = self$data,
              !!!params # disappears cleanly when empty
            )
          },
          # todo: find the equivalent in tetrad and make them have same name
          # should this even be here?
          "sem_bic_int" = {
            score <- rlang::exec(
              "new",
              Class = "GaussL0penIntScore",
              data  = self$data,
              !!!params # disappears cleanly when empty
            )
          },
          stop("Unknown score type using pcalg engine: ", method,
            call. = FALSE
          )
        )
        return(score)
      }
      private$score_function <- return_pcalg_score
    },

    #' @description
    #' Sets the algorithm for the search.
    #'
    #' @param method A string specifying the type of algorithm to use.
    set_alg = function(method) {
      method <- tolower(method)
      switch(method,
        "pc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.",
              call. = FALSE
            )
          }
          self$alg <- purrr::partial(
            pcalg::pc,
            indepTest = self$test,
            !!!self$params
          )
        },
        "fci" = {
          self$alg <- purrr::partial(
            pcalg::fci,
            indepTest = self$test,
            !!!self$params
          )
        },
        "ges" = {
          # Score will be set when we use run_search()
          self$alg <- purrr::partial(
            pcalg::ges,
            !!!self$params
          )
        },
        stop("Unknown method type using pcalg engine: ", method,
          call. = FALSE
        )
      )
    },

    #' @description
    #' Sets the knowledge for the search algorithm. Due to the nature of pcalg,
    #' we cannot set knowledge before we run it on data. So we set the function
    #' that will be used to build the fixed constraints, but it can first be
    #' done when data is provided.
    #'
    #' @param knowledge_obj A knowledge object that contains the fixed constraints.
    #' @param directed_as_undirected Logical; whether to treat directed edges as undirected.
    set_knowledge = function(knowledge_obj, directed_as_undirected = FALSE) {
      check_knowledge_obj(knowledge_obj)

      private$knowledge_function <- function() {
        if (is.null(self$data)) {
          stop("Data must be set before knowledge.", call. = FALSE)
        }
        labels <- colnames(self$data)
        as_pcalg_constraints(knowledge_obj,
          labels,
          directed_as_undirected = directed_as_undirected
        )
      }
    },

    #' @description
    #' Runs the search algorithm on the data.
    #'
    #' @param data A `data.frame` or a `matrix` containing the data.
    #' @param set_suff_stat Logical; whether to set the sufficient statistic
    run_search = function(data = NULL, set_suff_stat = TRUE) {
      if (!is.null(data)) {
        self$set_data(data, set_suff_stat)
      }
      if (is.null(self$data)) {
        stop("No data is set. Use set_data() first or input data directly into run_search().",
          call. = FALSE
        )
      }
      if (is.null(self$suff_stat) && set_suff_stat) {
        stop("No sufficient statistic is set. Use set_data() first.",
          call. = FALSE
        )
      }
      if (is.null(self$alg)) {
        stop("No algorithm is set. Use set_alg() first.",
          call. = FALSE
        )
      }

      # If score_function is NULL, then we are not using a score-based algorithm
      if (is.null(private$score_function)) {
        if (!is.null(private$knowledge_function)) {
          # If knowledge is set, we now need to call the function
          # to get the fixed constraints.
          self$knowledge <- private$knowledge_function()
          result <- self$alg(
            suffStat = self$suff_stat,
            labels = colnames(self$data),
            fixedGaps = self$knowledge$fixedGaps,
            fixedEdges = self$knowledge$fixedEdges
          )
        } else {
          result <- self$alg(
            suffStat = self$suff_stat,
            labels = colnames(self$data)
          )
        }
        # score_function is not null, so we are using a score-based algorithm
      } else {
        if (!is.null(private$knowledge_function)) {
          # If knowledge is set, we now need to call the function
          # to get the fixed constraints.
          self$knowledge <- private$knowledge_function()
          self$score <- private$score_function()
          if (!is.null(self$knowledge$fixedEdges)) {
            warning(
              "pcalg::ges() does not take required edges as arguments.",
              "\n  They will not be used here.",
              call. = FALSE
            )
          }
          result <- self$alg(
            self$score,
            fixedGaps = self$knowledge$fixedGaps
          )
        } else {
          self$score <- private$score_function()
          result <- self$alg(
            self$score
          )
        }
      }
      return(result |> discography())
    }
  ),
  private = list(

    # Function that will be used to determine which G square test to use.
    # It checks the number of unique values in the sufficient statistic.
    use_g_square = function() {
      return(
        function(x, y, S, suffStat) {
          # Check if the number of unqiue values has been found yet
          # to do: this can probably be done in a faster way
          if (length(private$uniques) == 0) {
            private$uniques <- self$suff_stat$dm |>
              c() |> # turn to vector such that unique works on value level
              unique() |>
              sort()
          }
          if (length(private$uniques) < 2) {
            stop("The data contains less than 2 unique values.",
              " If this is the case, there is nothing to discover.",
              call. = FALSE
            )
          }
          if (length(private$uniques) == 2) {
            return(pcalg::binCItest(x, y, S, suffStat))
          } else {
            return(pcalg::disCItest(x, y, S, suffStat))
          }
        }
      ) # end return
    },
    uniques = c(),
    knowledge_function = NULL,
    score_function = NULL
  )
)
