library(pcalg)

#' @export pcalgSearch
pcalgSearch <- R6Class(
  "pcalgSearch",
  public = list(
    data = NULL,
    rdata = NULL,
    score = NULL,
    test = NULL,
    alg = NULL,
    knowledge = NULL,
    params = NULL,
    suff_stat = NULL,
    initialize = function() {
      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$knowledge <- NULL
      self$params <- NULL
    },
    set_params = function(params) {
      self$params <- params
    },
    set_data = function(data) {
      self$data <- data
      self$set_suff_stat()
    },
    set_suff_stat = function() {
      if (is.null(self$data)) {
        stop("Data must be set before sufficient statistic.")
      }
      if (is.null(self$test)) {
        stop("Test must be set before sufficient statistic.")
      }
      # to do: check if data is continuous or discrete
      if (TRUE) {
        if (is.matrix(self$data) || is.data.frame(self$data)) {
          self$suff_stat <- list(C = cor(self$data), n = nrow(self$data))
        } else {
          stop("Data must be a matrix or data frame if numeric.")
        }
      } else if (is.data.frame(self$data)) {
        self$suff_stat <- list(dm = data.matrix(self$data), adaptDF = FALSE)
      } else {
        stop("Unrecognized data format.
             The data should be either continouos or discrete,
             and the data should be in a data.frame.")
      }
    },
    set_test = function(method,
                        alpha = NULL,
                        ...) {
      if (!is.null(alpha)) {
        self$params$alpha <- alpha
      }

      switch(method,
        "fisher_z" = {
          if (is.null(self$params$alpha)) {
            stop("Alpha must be set before test.")
          }
          self$test <- pcalg::gaussCItest
        },
        "g_square" = {
          if (is.null(self$alpha)) {
            stop("Alpha must be set before test.")
          }
          # test if data is binary
          if (!all(as.logical(self$suff_stat$dm) == self$suff_stat$dm)) {
            self$test <- pcalg::disCItest
          } else {
            self$test <- pcalg::binCItest
          }
        }
      )
    },
    set_score = function(...) {
      message("set_score() is not yet implemented.")
    },
    set_alg = function(method, ...) {
      method <- tolower(method)
      switch(method,
        "pc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          self$alg <- purrr::partial(
            pcalg::pc,
            indepTest = self$test,
            !!!self$params
          )
        },
        "fci" = {
          self$alg <- "fci" # to do
        },
        "ges" = {
          self$alg <- "ges" # to do
        },
        stop("Unknown method type using pcalg engine: ", method)
      )
      self$params <- list(...) # store extra parameters for the chosen alg
    },
    run_search = function(data) {
      if (!is.null(data)) {
        self$set_data(data)
      }
      if (is.null(self$suff_stat)) {
        stop("No sufficient statistic is set. Use set_data() first.")
      }
      if (is.null(self$data)) {
        stop("No data is set. Use set_data() first or input data directly into run_search().")
      }
      if (is.null(self$alg)) {
        stop("No algorithm is set. Use set_alg() first.")
      }
      result <- self$alg(suffStat = self$suff_stat, labels = colnames(self$data))
    }
  )
)
