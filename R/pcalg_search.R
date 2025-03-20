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
    set_data = function(data) {
      self$data <- data
      self$set_suff_stat(data)
    },
    set_suff_stat = function() {
      if (is.null(self$data)) {
        stop("Data must be set before sufficient statistic.")
      }
      if (is.numeric(self$data)) {
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
      method <- match.arg(method)
      if (!is.null(alpha)) {
        self$alpha <- alpha
      }
      
      switch(method,
        "fisher_z" = {
          if (is.null(self$suff_stat)) {
            stop("Sufficient statistic must be set before test.")
          }
          if (is.null(self$alpha)) {
            stop("Alpha must be set before test.")
          }
          self$test <- pcalg::gaussCItest
        },
        "g_square" = {
          if (is.null(self$suff_stat)) {
            stop("Sufficient statistic must be set before test.")
          } else {
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
          self$alg <- "pc" # todo: partial application?
        },
        "fci" = {
          self$alg <- "fci"
        },
        "ges" = {
          self$alg <- "ges"
        },
        stop("Unknown method type using pcalg engine: ", method)
      )
      self$params <- list(...) # store extra parameters for the chosen alg
    },
    run_search = function(data) {
      if (!is.null(data)) {
        self$set_data(data)
      }
      if (is.null(self$data)) {
        stop("No data is set. Use set_data() first or input data directly into run_search().")
      }
      if (is.null(self$alg)) {
        stop("No algorithm is set. Use set_alg() first.")
      }

        if (self$alg == "pc") {
          cat("Running PC algorithm with alpha =", self$alpha, "\n")
          result <- pc(
            suffStat = self$suff_stat,
            indepTest = self$test,
            alpha = self$alpha,
          )
          return(result)
        } else if (self$alg == "fci") {
          cat("Running FCI algorithm with alpha =", self$alpha, "\n")
          result <- fci(
            suffStat = self$suff_stat,
            indepTest = self$test,
            alpha = self$alpha,
            labels = labels,
            skel.method = self$params$skel.method %||% "stable",
            # etc.
          )
          return(result)
        }
      }

      if (self$alg == "ges") {
        # Score-based approach
        cat("Running GES (score-based) ... \n")
        if (is.null(self$score)) {
          stop("For GES, please set_score() first to define a pcalg score object.")
        }
        result <- ges(
          score = self$score,
          # Possibly pass self$params for extra arguments:
          fixedGaps = self$params$fixedGaps %||% NULL,
          fixedEdges = self$params$fixedEdges %||% NULL
          # ...
        )
        return(result)
      }

      stop("Unknown algorithm or not yet implemented.")
    }
  )
)
