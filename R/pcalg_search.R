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
    params = NULL,
    suff_stat = NULL,
    continuous = NULL,
    knowledge = NULL,
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
      # Reset uniques, used to determine binary (or not) G square test
      private$uniques <- c()

      # todo: Should it be changed or not?
      # Reset knowledge function
      # private$knowledge_function <- NULL
    },
    set_suff_stat = function() {
      if (is.null(self$data)) {
        stop("Data must be set before sufficient statistic.")
      }
      if (is.null(self$test)) {
        stop("Test must be set before sufficient statistic.")
      }
      if (is.null(self$continuous)) {
        stop("The pcalgSearch class does not have knowledge on whether the
             sufficient statistic is for a continuous or discrete test.
             Please set test using set_test() or set continuous directly
             by self$continuous <- TRUE/FALSE.")
      }
      if (self$continuous) {
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

      method <- tolower(method)

      switch(method,
        "fisher_z" = {
          if (is.null(self$params$alpha)) {
            stop("Alpha must be set before test.")
          }
          self$test <- pcalg::gaussCItest
          self$continuous <- TRUE
        },
        "g_square" = {
          if (is.null(self$params$alpha)) {
            stop("Alpha must be set before test.")
          }
          self$test <- private$use_g_square()
          self$continuous <- FALSE
        },
        stop("Unknown test type using pcalg engine: ", method)
      )
    },
    set_score = function(...) {
      message("set_score() is not yet implemented.")
    },
    set_alg = function(method, ...) {
      method <- tolower(method)
      # to do: add them, don't replace them (or remove??)
      # self$params <- list(...) # store extra parameters for the chosen alg
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
    },
    set_knowledge = function(knowledge_obj) {
      check_knowledge_obj(knowledge_obj)

      if (!is.null(knowledge_obj$tiers)) {
        warning("Tiered background knowledge cannot be utilized by the pcalg engine.")
      }

      # Function that will be used to build the fixed constraints
      # but it can first be done, when data is provided.
      return_pcalg_background_knowledge <- function(labels) {
        p <- length(labels)

        fixedGaps <- matrix(FALSE, nrow = p, ncol = p, dimnames = list(labels, labels))
        fixedEdges <- matrix(FALSE, nrow = p, ncol = p, dimnames = list(labels, labels))

        # Create a named vector to map variable names to indices.
        label_to_index <- setNames(seq_along(labels), labels)

        if (length(knowledge_obj$forbidden) > 0) {
          for (edge in knowledge_obj$forbidden) {
            if (length(edge) < 2) {
              stop("Forbidden edge must have at least two elements.")
            }
            if (!(edge[1] %in% labels && edge[2] %in% labels)) {
              stop("Forbidden edge not found in labels: ", paste(edge, collapse = " - "))
            }
            i <- label_to_index[[edge[1]]]
            j <- label_to_index[[edge[2]]]
            fixedGaps[i, j] <- TRUE
            fixedGaps[j, i] <- TRUE
          }
        }

        # Process required edges, which become fixed edges.
        if (length(knowledge_obj$required) > 0) {
          for (edge in knowledge_obj$required) {
            if (length(edge) < 2) {
              stop("Required edge must have at least two elements.")
            }
            if (!(edge[1] %in% labels && edge[2] %in% labels)) {
              stop("Required edge not found in labels: ", paste(edge, collapse = " - "))
            }
            i <- label_to_index[[edge[1]]]
            j <- label_to_index[[edge[2]]]
            fixedEdges[i, j] <- TRUE
            fixedEdges[j, i] <- TRUE
          }
        }
        return(list(fixedGaps = fixedGaps, fixedEdges = fixedEdges))
      }

      # Due to the nature of pcalg, we cannot set knowledge before
      # we run it on data. So we set the function that will be
      # used to build the fixed constraints, but it can first be
      # done when data is provided.
      private$knowledge_function <- return_pcalg_background_knowledge
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
      if (!is.null(private$knowledge_function)) {
        # If knowledge is set, we now need to call the function
        # to get the fixed constraints.
        self$knowledge <- private$knowledge_function(colnames(self$data))
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
      return(result)
    }
  ),
  private = list(
    use_g_square = function() {
      # Finds out whether to use binary or non-binary test (based on suff_stat).
      # Then, sets the test to the found test, and evaluates in this test.
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
            stop("The data contains less than 2 unique values. If this is the case, there is nothing to discover.")
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
    knowledge_function = NULL
  )
)
