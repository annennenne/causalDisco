# See https://github.com/r-lib/roxygen2/issues/1158 for why this is needed
#' @title R6 Interface to causalDisco Search Algorithms
#'
#' @name CausalDiscoSearch
#'
#' @example inst/roxygen-examples/causalDisco_search_example.R
NULL

#' @title R6 Interface to causalDisco Search Algorithms
#'
#' @description This class implements the search algorithms from the causalDisco
#' package, which wraps and adds temporal order to pcalg algorithms.
#' It allows to set the data, sufficient statistics, test, score, and algorithm.
#'
#' @importFrom R6 R6Class
#'
#' @rdname CausalDiscoSearch
#'
#' @export CausalDiscoSearch
CausalDiscoSearch <- R6::R6Class(
  "CausalDiscoSearch",
  public = list(
    #' @template data-field
    data = NULL,

    #' @field score A function that will be used to build the score,
    #'  when data is set. Can be set with \code{$set_score()}. Recognized values
    #'  are:
    #'  \itemize{
    #'     \item \code{"tbic"} - Temporal BIC score for Gaussian data.
    #'     See \code{\link{TemporalBIC}}
    #'     \item \code{"tbdeu"} - Temporal BDeu score for discrete data.
    #'     See \code{\link{TemporalBDeu}}.
    #'     }
    score = NULL,

    #' @field test A function that will be used to test independence.
    #'  Can be set with \code{$set_test()}. Recognized values are:
    #'  \itemize{
    #'    \item \code{"fisher_z"} - Fisher Z test for Gaussian data.
    #'    See [cor_test()].
    #'    \item \code{"reg"} - Regression test for discrete or binary data.
    #'    See [reg_test()].
    #'  }
    test = NULL,

    #' @field alg A function that will be used to run the search algorithm.
    #' Can be set with \code{$set_alg()}. Recognized values are:
    #' \itemize{
    #'   \item \code{"tpc"}  - TPC algorithm.
    #'   See [tpc()].
    #'   \item \code{"tfci"} - TFCI algorithm.
    #'   See [tfci()].
    #'   \item \code{"tges"} - TGES algorithm.
    #'   See [tges()].
    #' }
    alg = NULL,

    #' @field params A list of parameters for the test and algorithm.
    #' Can be set with \code{$set_params()}.
    #' TODO: not secure yet in terms of distributing arguments.
    #' Use with caution.
    params = NULL,

    #' @field suff_stat Sufficient statistic. The format and contents of the
    #' sufficient statistic depends on which test is being used.
    suff_stat = NULL,

    #' @field knowledge A `knowledge` object holding background knowledge.
    knowledge = NULL,

    #' @description
    #' Constructor for the `CausalDiscoSearch` class.
    initialize = function() {
      .check_if_pkgs_are_installed(
        pkgs = c(
          "pcalg",
          "purrr",
          "R6",
          "rlang",
          "stats",
          "utils"
        ),
        class_name = "CausalDiscoSearch"
      )

      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$knowledge <- NULL
      self$params <- list(
        na_method = "none"
      )
    },

    #' @description
    #' Sets the parameters for the test and algorithm.
    #'
    #' @param params A list of parameters to set.
    set_params = function(params) {
      if (is.null(params)) {
        return(invisible(self))
      }
      reserved <- c(
        "data",
        "suff_stat",
        "knowledge",
        "score",
        "test",
        "labels"
      )
      bad <- intersect(names(params), reserved)
      if (length(bad)) {
        stop(
          "These parameters are reserved and cannot be set via set_params(): ",
          paste(bad, collapse = ", "),
          call. = FALSE
        )
      }
      # allow overriding default output/na_method/verbose/etc.
      self$params <- utils::modifyList(self$params, params)
      invisible(self)
    },

    #' @description
    #' Sets the data for the search algorithm.
    #'
    #' @param data A `data.frame` or a `matrix` containing the data.
    #' @param set_suff_stat Logical; whether to set the sufficient statistic.
    set_data = function(data, set_suff_stat = TRUE) {
      self$data <- data
      if (set_suff_stat) {
        self$set_suff_stat()
      }

      invisible(self)
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

      out <- .get_pcalg_test_from_string(
        method = private$test_key,
        X = self$data,
        suff_stat = TRUE
      )
      self$test <- out$method
      self$suff_stat <- out$suff_stat
      invisible(self)
    },

    #' @description
    #' Sets the test for the search algorithm.
    #'
    #' @param method A string specifying the type of test to use.
    #' @param alpha Significance level for the test.
    set_test = function(method, alpha = 0.05) {
      method <- tolower(method)
      if (!is.null(alpha)) {
        self$params$alpha <- alpha
      }
      private$test_key <- method

      if (!is.null(self$data)) {
        self$set_suff_stat()
      } else {
        out <- .get_pcalg_test_from_string(
          method = private$test_key,
          suff_stat = FALSE
        )
        self$test <- out$method
      }
      invisible(self)
    },

    #' @description
    #' Sets the score for the search algorithm.
    #'
    #' @param method A string specifying the type of score to use.
    #' @param params A list of parameters to pass to the score function.
    set_score = function(method, params = list()) {
      method <- tolower(method)
      allowed <- c("tbic", "tbdeu")
      if (!(method %in% allowed)) {
        stop(
          "Unknown score type using causalDisco engine: ",
          method,
          call. = FALSE
        )
      }

      private$score_method <- method
      private$score_params <- if (is.null(params)) list() else params

      private$score_function <- function() {
        if (is.null(self$data)) {
          stop("Data must be set before score.", call. = FALSE)
        }

        if (identical(private$score_method, "tbic")) {
          # Gaussian temporal score
          return(rlang::exec(
            "new",
            Class = "TemporalBIC",
            data = self$data,
            nodes = colnames(self$data),
            knowledge = self$knowledge,
            !!!private$score_params
          ))
        }

        if (identical(private$score_method, "tbdeu")) {
          # Categorical temporal score
          return(rlang::exec(
            "new",
            Class = "TemporalBDeu",
            data = self$data,
            nodes = colnames(self$data),
            knowledge = self$knowledge,
            !!!private$score_params
          ))
        }

        stop("Internal: unsupported score method.", call. = FALSE)
      }

      invisible(self)
    },

    #' @description
    #' Sets the algorithm for the search.
    #'
    #' @param method A string specifying the type of algorithm to use.
    set_alg = function(method) {
      method <- tolower(method)
      private$alg_method <- method

      switch(
        method,
        "tpc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          self$alg <- purrr::partial(
            tpc_run,
            test = self$test,
            !!!self$params
          )
        },
        "tfci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          self$alg <- purrr::partial(
            tfci_run,
            test = self$test,
            !!!self$params
          )
        },
        "tges" = {
          self$alg <- purrr::partial(
            tges_run,
            verbose = isTRUE(self$params$verbose)
          )
        },
        stop(
          "Unknown method type using causalDisco engine: ",
          method,
          call. = FALSE
        )
      )

      invisible(self)
    },

    #' @description
    #'
    #' Sets the background knowledge for the search with a `knowledge` object.
    #'
    #' @param kn A `knowledge` object.
    #' @param directed_as_undirected Logical; whether to treat directed edges in
    #' the knowledge as undirected. Default is `FALSE`. This is due to the
    #' nature of how \pkg{pcalg} handles background knowledge when using
    #' [pcalg::skeleton()] under the hood in
    #' [tpc()] and
    #' [tfci()].
    #' @seealso [knowledge()].
    set_knowledge = function(kn, directed_as_undirected = FALSE) {
      is_knowledge(kn)
      self$knowledge <- kn
      private$directed_as_undirected <- directed_as_undirected
      invisible(self)
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
          "No data is set. Use set_data() first or pass data to ",
          "run_search().",
          call. = FALSE
        )
      }
      if (is.null(self$alg)) {
        stop("No algorithm is set. Use set_alg() first.", call. = FALSE)
      }

      # constraint-based path
      if (!identical(private$alg_method, "tges")) {
        if (is.null(self$suff_stat) && set_suff_stat) {
          stop(
            "No sufficient statistic is set. Use set_data() first.",
            call. = FALSE
          )
        }
        res <- self$alg(
          data = self$data,
          knowledge = self$knowledge,
          suff_stat = self$suff_stat
        )
        res
      } else {
        # score-based path (tges)
        if (is.null(private$score_function)) {
          stop("No score is set. Use set_score() first.", call. = FALSE)
        }
        self$score <- private$score_function()

        if (!is.null(self$knowledge)) {
          res <- self$alg(
            score = self$score
          )
        } else {
          res <- self$alg(
            score = self$score
          )
        }
        res
      }
    }
  ),
  private = list(
    alg_method = NULL, # "tpc", "tfci", or "tges"
    test_key = NULL,
    directed_as_undirected = FALSE,
    score_method = NULL,
    score_params = NULL,
    score_function = NULL
  )
)
