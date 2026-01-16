# See https://github.com/r-lib/roxygen2/issues/1158 for why this is needed
#' @title R6 Interface to pcalg Search Algorithms
#'
#' @name PcalgSearch
#'
#' @example inst/roxygen-examples/pcalg_search_example.R
NULL

#' @title R6 Interface to pcalg Search Algorithms
#'
#' @description
#' This class implements the search algorithms from the pcalg package.
#' It allows to set the data, sufficient statistics, test, score, and algorithm.
#'
#' @rdname PcalgSearch
#'
#' @importFrom R6 R6Class
#' @export PcalgSearch
PcalgSearch <- R6::R6Class(
  "PcalgSearch",
  public = list(
    #' @template data-field
    data = NULL,

    #' @field score A function that will be used to build the score,
    #'  when data is set. Can be set with \code{$set_score()}. Recognized values
    #'  are:
    #'  \itemize{
    #'     \item \code{"sem_bic"} - BIC score for Gaussian observed data.
    #'     See \code{\link[pcalg]{GaussL0penObsScore-class}}.
    #'     \item \code{"sem_bic_int"} - BIC score for Gaussian data from jointly
    #'     interventional and observational Gaussian data.
    #'     See \code{\link[pcalg]{GaussL0penIntScore-class}}.
    #'     }
    score = NULL,

    #' @field test A function that will be used to test independence.
    #'  Can be set with \code{$set_test()}. Recognized values are:
    #'  \itemize{
    #'    \item \code{"fisher_z"} - Fisher Z test for Gaussian data.
    #'    See [pcalg::gaussCItest()].
    #'    \item \code{"g_square"} - G square test for discrete data.
    #'    See [pcalg::binCItest()] and [pcalg::disCItest()].
    #'  }
    test = NULL,

    #' @field alg A function that will be used to run the search algorithm.
    #' Can be set with \code{$set_alg()}. Recognized values are:
    #' \itemize{
    #'   \item \code{"pc"} - PC algorithm. See [pcalg::pc()].
    #'   \item \code{"fci"} - FCI algorithm. See [pcalg::fci()].
    #'   \item \code{"ges"} - GES algorithm. See [pcalg::ges()].
    #' }
    alg = NULL,

    #' @field params A list of parameters for the test and algorithm.
    #' Can be set with \code{$set_params()}.
    #' The parameters are passed to the test and algorithm functions.
    params = NULL,

    #' @field suff_stat Sufficient statistic. The format and contents of the
    #' sufficient statistic depends on which test is being used.
    suff_stat = NULL,

    #' @field continuous Logical; whether the sufficient statistic is for a
    #' continuous test. If both continuous and discrete are `TRUE`, the
    #' sufficient statistic is build for a mixed test.
    continuous = NULL,

    #' @field discrete Logical; whether the sufficient statistic is for a
    #' discrete test. If both continuous and discrete are `TRUE`, the sufficient
    #' statistic is build for a mixed test.
    discrete = NULL,

    #' @field knowledge A list of fixed constraints for the search algorithm. Note, that
    #' pcalg only works with symmetric knowledge. Thus, the only allowed types of knowledge
    #' is forbidden edges in both directions.
    knowledge = NULL,

    #' @field adapt_df Logical; whether to adapt the degrees of freedom
    #' for discrete tests.
    adapt_df = TRUE,

    #' @description
    #' Constructor for the `PcalgSearch` class.
    initialize = function() {
      .check_if_pkgs_are_installed(
        pkgs = c(
          "pcalg",
          "purrr",
          "rlang"
        ),
        class_name = "PcalgSearch"
      )

      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$knowledge <- NULL
      self$params <- list()
      self$adapt_df <- TRUE
      self$suff_stat <- NULL
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
    #' @param set_suff_stat Logical; whether to set the sufficient statistic.
    #' for the data.
    set_data = function(data, set_suff_stat = TRUE) {
      self$data <- data
      if (set_suff_stat) {
        self$set_suff_stat()
      }
    },

    #' @description
    #' Sets the sufficient statistic for the data.
    set_suff_stat = function() {
      if (is.null(self$data)) {
        stop("Data must be set before sufficient statistic.", call. = FALSE)
      }
      if (is.null(private$test_key)) {
        stop("Test must be set before sufficient statistic.", call. = FALSE)
      }
      if (!is.logical(self$adapt_df)) {
        stop("adapt_df must be a logical.", call. = FALSE)
      }
      out <- .get_pcalg_test_from_string(
        method = private$test_key,
        X = self$data,
        suff_stat = TRUE,
        adaptDF = self$adapt_df,
        nlev = NULL
      )
      self$test <- out$method
      self$suff_stat <- out$suff_stat
    },

    #' @description
    #' Sets the test for the search algorithm.
    #'
    #' @param method A string specifying the type of test to use.
    #' @param alpha Significance level for the test.
    set_test = function(method, alpha = 0.05) {
      if (!is.null(alpha)) {
        self$params$alpha <- alpha
      } else {
        stop("alpha must be set before using tests", call. = FALSE)
      }
      if (!is.character(method)) {
        stop("Currently, only method as string is supported.", call. = FALSE)
      }
      private$test_key <- tolower(method)

      if (!is.null(self$data)) {
        self$set_suff_stat()
      } else {
        out <- .get_pcalg_test_from_string(
          method = private$test_key,
          suff_stat = FALSE
        )
        self$test <- out$method
      }
    },

    #' @description
    #' Sets the score for the search algorithm.
    #'
    #' @param method A string specifying the type of score to use.
    #' @param params A list of parameters to pass to the score function.
    set_score = function(method, params = list()) {
      method <- tolower(method)
      allowed <- c("sem_bic", "sem_bic_int")
      if (!(method %in% allowed)) {
        stop("Unknown score type using pcalg engine: ", method, call. = FALSE)
      }
      # Function that will be used to build the score, when data is set
      return_pcalg_score <- function() {
        if (is.null(self$data)) {
          stop("Data must be set before score.", call. = FALSE)
        }
        switch(
          method,
          "sem_bic" = {
            score <- rlang::exec(
              "new",
              Class = "GaussL0penObsScore",
              data = self$data,
              !!!params
            )
          },
          "sem_bic_int" = {
            score <- rlang::exec(
              "new",
              Class = "GaussL0penIntScore",
              data = self$data,
              !!!params
            )
          }
        )
        score
      }
      private$score_function <- return_pcalg_score
    },

    #' @description
    #' Sets the algorithm for the search.
    #'
    #' @param method A string specifying the type of algorithm to use.
    set_alg = function(method) {
      method <- tolower(method)
      switch(
        method,
        "pc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          self$alg <- purrr::partial(
            pcalg::pc,
            indepTest = self$test,
            !!!self$params
          )
        },
        "fci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
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
        stop("Unknown method type using pcalg engine: ", method, call. = FALSE)
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
      is_knowledge(knowledge_obj)

      private$knowledge_function <- function() {
        if (is.null(self$data)) {
          stop("Data must be set before knowledge.", call. = FALSE)
        }
        labels <- colnames(self$data)
        constraints <- as_pcalg_constraints(
          knowledge_obj,
          labels,
          directed_as_undirected = directed_as_undirected
        )
        if (any(constraints$fixed_edges)) {
          warning(
            "Engine pcalg does not use required edges; ignoring them.",
            call. = FALSE
          )
        }
        constraints
      }
    },

    #' @description
    #' Runs the search algorithm on the data.
    #'
    #' @param data A `data.frame` or a `matrix` containing the data.
    #' @param set_suff_stat Logical; whether to set the sufficient statistic
    run_search = function(data = NULL, set_suff_stat = TRUE) {
      if (!is.null(data)) {
        if (is.null(private$score_function)) {
          self$set_data(data, set_suff_stat = set_suff_stat)
        } else {
          self$set_data(data, set_suff_stat = FALSE)
        }
      }
      if (is.null(self$data)) {
        stop(
          "No data is set. ",
          "Use set_data() first or input data directly into run_search().",
          call. = FALSE
        )
      }
      if (is.null(self$alg)) {
        stop("No algorithm is set. Use set_alg() first.", call. = FALSE)
      }

      # If score_function is NULL, then we are not using a score-based algorithm
      if (is.null(private$score_function)) {
        if (is.null(self$suff_stat) && set_suff_stat) {
          stop(
            "No sufficient statistic is set. Use set_data() first.",
            call. = FALSE
          )
        }
        if (!is.null(private$knowledge_function)) {
          # If knowledge is set, we now need to call the function
          # to get the fixed constraints.
          self$knowledge <- private$knowledge_function()
          result <- self$alg(
            suffStat = self$suff_stat,
            labels = colnames(self$data),
            fixedGaps = self$knowledge$fixed_gaps,
            fixedEdges = self$knowledge$fixed_edges
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
      out <- if (is.list(result) && !is.null(result$essgraph)) {
        # if ges output
        result$essgraph
      } else {
        result
      }
      out |> knowledgeable_caugi()
    }
  ),
  private = list(
    knowledge_function = NULL,
    score_function = NULL,
    test_key = NULL
  )
)
