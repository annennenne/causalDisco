#' @title TetradSearch R6 Class
#'
#' @description
#' The `TetradSearch` R6 class provides a high-level interface to the Tetrad
#' Java library. It allows users to set up different independence tests and
#' scoring functions, specify causal search algorithms, and run them on data.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{data}}{(Java object) The Java object representing the loaded data.}
#'   \item{\code{rdata}}{(data.frame) The original R data passed to the class.}
#'   \item{\code{score}}{(Java object) The scoring function object.}
#'   \item{\code{test}}{(Java object) The independence test object.}
#'   \item{\code{alg}}{(Java object) The causal discovery algorithm object.}
#'   \item{\code{mc_test}}{(Java object) The independence test object used for Markov checker.}
#'   \item{\code{java}}{(Java object) The resulting graph or model object after running a search.}
#'   \item{\code{knowledge}}{(Java object) The Tetrad \code{Knowledge} object storing background knowledge.}
#'   \item{\code{params}}{(Java object) The Tetrad \code{Parameters} object with various configuration settings.}
#'   \item{\code{bootstrap_graphs}}{(Java List) A list of bootstrapped graphs produced by Tetrad.}
#'   \item{\code{mc_ind_results}}{(Java List) A list of results for independence tests from Markov checking.}
#'   \item{\code{bhat}}{(Java object) BHat adjacency matrix from the Tetrad search, if requested.}
#'   \item{\code{unstable_bhats}}{(Java object) A collection of BHat matrices from the Tetrad search, if requested.}
#'   \item{\code{stable_bhats}}{(Java object) A collection of BHat matrices from the Tetrad search, if requested.}
#' }
#'
#' @name TetradSearch
#' @docType class
#' @rdname TetradSearch
#' @importFrom rJava .jnew .jcall .jfield .jniInitialized .jinit .jcast
#' @importFrom R6 R6Class
#' @export TetradSearch
TetradSearch <- R6Class(
  "TetradSearch",
  public = list(
    data = NULL,
    rdata = NULL,
    score = NULL,
    test = NULL,
    alg = NULL,
    mc_test = NULL,
    java = NULL,
    result = NULL,
    knowledge = NULL,
    params = NULL,
    bootstrap_graphs = NULL,
    mc_ind_results = NULL,
    bhat = NULL,
    unstable_bhats = NULL,
    stable_bhats = NULL,

    #' @description Initializes the \code{TetradSearch} object, creating new Java objects for
    #'   \code{knowledge} and \code{params}.
    initialize = function() {
      if (!.jniInitialized) {
        .jinit(
          # todo: how many gb?
          parameters = "-Xmx2g",
          classpath = "inst/java/tetrad-current.jar"
        )
      }
      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$mc_test <- NULL
      self$knowledge <- .jnew("edu/cmu/tetrad/data/Knowledge")
      self$params <- .jnew("edu/cmu/tetrad/util/Parameters")
      self$bootstrap_graphs <- NULL
      self$set_verbose(FALSE) # Set verbose to FALSE per default.
    },

    #' @description Sets the independence test to use in Tetrad.
    #' @param method (character) Name of the test method (e.g., "chi_square", "fisher_z").
    #' @param ... Additional arguments passed to the private test-setting methods.
    #' @param mc (logical) If TRUE, sets this test for the Markov checker \code{mc_test}.
    #' @return Invisibly returns \code{self}, for chaining.
    set_test = function(method, ..., mc = FALSE) {
      stopifnot(
        is.logical(mc),
        length(mc) == 1
      )
      method <- tolower(method)
      switch(method,
        "chi_square" = {
          private$use_chi_square_test(..., use_for_mc = mc)
        },
        "fisher_z" = {
          private$use_fisher_z_test(..., use_for_mc = mc)
        },
        "cci" = {
          private$use_cci_test(..., use_for_mc = mc)
        },
        "basis_function_lrt" = {
          private$use_basis_function_lrt_test(..., use_for_mc = mc)
        },
        "conditional_gaussian" = {
          private$use_conditional_gaussian_test(..., use_for_mc = mc)
        },
        "degenerate_gaussian" = {
          private$use_degenerate_gaussian_test(..., use_for_mc = mc)
        },
        "g_square" = {
          private$use_g_square_test(..., use_for_mc = mc)
        },
        "kci" = {
          private$use_kci_test(..., use_for_mc = mc)
        },
        "probabilistic" = {
          private$use_probabilistic_test(..., use_for_mc = mc)
        },
        {
          stop("Unknown test type using tetrad engine: ", method, call. = FALSE)
        }
      )
      invisible(self)
    },

    #' @description Sets the scoring function to use in Tetrad.
    #' @param method (character) Name of the score (e.g., "sem_bic", "ebic", "bdeu").
    #' @param ... Additional arguments passed to the private score-setting methods.
    #' @return Invisibly returns \code{self}.
    set_score = function(method, ...) {
      method <- tolower(method)
      switch(method,
        "sem_bic" = {
          private$use_sem_bic_score(...)
        },
        "ebic" = {
          private$use_ebic_score(...)
        },
        "bdeu" = {
          private$use_bdeu_score(...)
        },
        "basis_function_bic" = {
          private$use_basis_function_bic_score(...)
        },
        "conditional_gaussian" = {
          private$use_conditional_gaussian_score(...)
        },
        "degenerate_gaussian" = {
          private$use_degenerate_gaussian_score(...)
        },
        "discrete_bic" = {
          private$use_discrete_bic_score(...)
        },
        "gic" = {
          private$use_gic_score(...)
        },
        "mag_degenerate_gaussian_bic" = {
          private$use_mag_degenerate_gaussian_bic_score(...)
        },
        "mixed_variable_polynomial" = {
          private$use_mixed_variable_polynomial_score(...)
        },
        "poisson_prior" = {
          private$use_poisson_prior_score(...)
        },
        "zhang_shen_bound" = {
          private$use_zhang_shen_bound_score(...)
        },
        {
          stop("Unknown score type using tetrad engine: ", method,
            call. = FALSE
          )
        }
      )
      invisible(self)
    },

    #' @description Sets the causal discovery algorithm to use in Tetrad.
    #' @param method (character) Name of the algorithm (e.g., "fges", "pc", "fci", etc.).
    #' @param ... Additional parameters passed to the private algorithm-setting methods.
    #' @return Invisibly returns \code{self}.
    set_alg = function(method, ...) {
      method <- tolower(method)
      switch(method,
        "fges" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          private$set_fges_alg(...)
        },
        "fges_mb" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          private$set_fges_mb_alg(...)
        },
        "boss" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          private$set_boss_alg(...)
        },
        "restricted_boss" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              " This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_restricted_boss_alg(...)
        },
        "cstar" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              " This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_cstar_alg(...)
        },
        "sp" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          private$set_sp_alg(...)
        },
        "grasp" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_grasp_alg(...)
        },
        "pc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_pc_alg(...)
        },
        "cpc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_cpc_alg(...)
        },
        "pcmax" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_pcmax_alg(...)
        },
        "fci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_fci_alg(...)
        },
        "rfci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_rfci_alg(...)
        },
        "cfci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_cfci_alg(...)
        },
        "gfci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_gfci_alg(...)
        },
        "bfci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_bfci_alg(...)
        },
        "lv_lite" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_lv_lite_alg(...)
        },
        "grasp_fci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_grasp_fci_alg(...)
        },
        "spfci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_spfci_alg(...)
        },
        "ica_lingam" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_ica_lingam_alg(...)
        },
        "ica_lingd" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_ica_lingd_alg(...)
        },
        "fask" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          private$set_fask_alg(...)
        },
        "fofc" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_fofc_alg(...)
        },
        "ccd" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_ccd_alg(...)
        },
        "svar_fci" = {
          private$set_svar_fci_alg(...)
        },
        "direct_lingam" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_direct_lingam_alg(...)
        },
        "dagma" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_dagma_alg(...)
        },
        "pc_lingam" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_pc_lingam_alg(...)
        },
        "svar_gfci" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_svar_gfci_alg(...)
        },
        {
          stop("Unknown method type using tetrad engine: ", method, call. = FALSE)
        }
      )
      invisible(self)
    },
    #' @description Sets the background knowledge object.
    #' @param knowledge_obj An object containing Tetrad knowledge (must implement \code{get_tetrad_knowledge}).
    set_knowledge = function(knowledge_obj) {
      check_knowledge_obj(knowledge_obj)
      knowledge_tetrad <- as_tetrad_knowledge(knowledge_obj)
      self$knowledge <- knowledge_tetrad

      # Set knowledge to algorithm
      if (!is.null(self$alg)) {
        self$alg$setKnowledge(self$knowledge)
      }
    },
    #' @description Sets parameters for the Tetrad search.
    #' @param ... Named arguments for the parameters to set.
    set_params = function(...) {
      # Capture the named arguments as a list.
      arg_list <- list(...)
      for (param_name in names(arg_list)) {
        value <- arg_list[[param_name]]
        # Get the key (static field) from Params using the field name.
        key <- .jfield("edu/cmu/tetrad/util/Params", "S", param_name)

        # Wrap the value based on its type.
        wrapped <- if (is.numeric(value)) {
          .jcast(.jnew("java/lang/Double", as.double(value)), "java/lang/Object")
        } else if (is.logical(value)) {
          .jcast(.jnew("java/lang/Boolean", as.logical(value)), "java/lang/Object")
        } else if (is.character(value)) {
          .jcast(value, "java/lang/Object")
        } else {
          .jcast(value, "java/lang/Object")
        }

        # Set the parameter using the key and wrapped value.
        self$params$set(key, wrapped)
      }
      invisible(NULL)
    },
    #' @description Retrieves the argument names of a matching private function.
    #' @param fn_pattern (character) A pattern that should match a private method name.
    #' @param score (logical) If TRUE, retrieves parameters for a scoring function.
    #' @param test (logical) If TRUE, retrieves parameters for a test function.
    #' @param alg (logical) If TRUE, retrieves parameters for an algorithm.
    #' @return (character) The names of the parameters.
    get_parameters_for_function = function(fn_pattern,
                                           score = FALSE,
                                           test = FALSE,
                                           alg = FALSE) {
      stopifnot(
        is.character(fn_pattern),
        is.logical(c(score, test, alg)),
        length(score) == 1,
        length(test) == 1,
        length(alg) == 1
      )
      # Check if exclusively one of score, etst, or alg is TRUE
      if (sum(c(score, test, alg)) != 1) {
        stop(
          "Score is: ", score, ", test is: ", test, ", and alg is: ", alg,
          ". (Exclusively) one of them should be TRUE.",
          call. = FALSE
        )
      }
      if (score) {
        function_names <- sprintf("^(set_|use_)%s(_score)?$", fn_pattern)
      } else if (test) {
        function_names <- sprintf("^(set_|use_)%s(_test)?$", fn_pattern)
      } else if (alg) {
        function_names <- sprintf("^(set_|use_)%s(_alg)?$", fn_pattern)
      } else {
        stop("Either score or test should be TRUE.", call. = FALSE)
      }
      is_private_method <- function(name) {
        is.function(get(name, envir = as.environment(private))) &&
          grepl(function_names, name)
      }
      private_names <- ls(envir = as.environment(private))
      matched_function <- base::Filter(is_private_method, private_names)
      if (length(matched_function) != 1) {
        if (length(matched_function > 1)) {
          error_message_suffix <- paste0("\n  Matches: ", paste(matched_function, collapse = ", "))
        } else {
          error_message_suffix <- ""
        }
        stop(
          paste0(
            "There is ", length(matched_function), " matches to the function pattern: ",
            fn_pattern, "\n  This is probably a misspecification of either a algorithm, test, or score.",
            "\n  There should be (only) a single match.", error_message_suffix
          ),
          call. = FALSE
        )
      }
      return(names(formals(matched_function, envir = as.environment(private))))
    },
    #' @description Runs the chosen Tetrad algorithm on the data.
    #' @param data (optional) If provided, overrides the previously set data.
    #' @param bootstrap (logical) If TRUE, bootstrapped graphs will be generated.
    #' @param bhat (logical) If TRUE, retrieve the BHat adjacency matrix.
    #' @param unstable_bhat (logical) If TRUE, retrieve unstable BHats.
    #' @param stable_bhat (logical) If TRUE, retrieve stable BHats.
    #' @return Nothing, but populates \code{self$java} with the resulting graph.
    run_search = function(data = NULL, bootstrap = FALSE, bhat = FALSE,
                          unstable_bhat = FALSE, stable_bhat = FALSE) {
      stopifnot(
        is.logical(
          c(
            bootstrap,
            bhat,
            unstable_bhat,
            stable_bhat
          )
        ),
        length(bootstrap) == 1,
        length(bhat) == 1,
        length(unstable_bhat) == 1,
        length(stable_bhat) == 1
      )
      if (!is.null(data)) {
        self$set_data(data)
      }
      if (is.null(self$data)) {
        stop("No data is set. Use set_data() first or input data directly into run_search().", call. = FALSE)
      }
      if (is.null(self$alg)) {
        stop("No algorithm is set. Use set_alg() first.", call. = FALSE)
      }
      # run the search
      self$java <- self$alg$search(self$data, self$params)

      # convert to tetrad_graph object (essentially a wrapper around amat.pag)
      self$result <- tetrad_graph(self$get_amat())

      if (bootstrap) {
        self$bootstrap_graphs <- self$alg$getBootstrapGraphs()
      }
      if (bhat) {
        self$bhat <- self$alg$getBhat()
      }
      if (unstable_bhat) {
        self$unstable_bhats <- self$alg$getUnstableBhats()
      }
      if (stable_bhat) {
        self$stable_bhats <- self$alg$getStableBhats()
      }
      return(self$result |> discography())
    },
    # NOT TESTED
    #' @description Configures bootstrapping parameters for the Tetrad search.
    #' @param number_resampling (integer) Number of bootstrap samples.
    #' @param percent_resample_size (numeric) Percentage of sample size for each bootstrap.
    #' @param add_original (logical) If TRUE, add the original dataset to the bootstrap set.
    #' @param with_replacement (logical) If TRUE, sampling is done with replacement.
    #' @param resampling_ensemble (integer) How the resamples are used or aggregated.
    #' @param seed (integer) Random seed, or -1 for none.
    set_bootstrapping = function(number_resampling = 0,
                                 percent_resample_size = 100,
                                 add_original = TRUE,
                                 with_replacement = TRUE,
                                 resampling_ensemble = 1,
                                 seed = -1) {
      stopifnot(
        is.numeric(number_resampling),
        floor(number_resampling) == number_resampling,
        number_resampling >= 0,
        is.numeric(percent_resample_size),
        percent_resample_size >= 0,
        percent_resample_size <= 100,
        is.logical(c(add_original, with_replacement)),
        length(add_original) == 1,
        length(with_replacement) == 1,
        is.numeric(resampling_ensemble),
        floor(resampling_ensemble) == resampling_ensemble,
        is.numeric(seed)
      )
      self$set_params(
        NUMBER_RESAMPLING = number_resampling,
        PERCENT_RESAMPLE_SIZE = percent_resample_size,
        ADD_ORIGINAL_DATASET = add_original,
        RESAMPLING_WITH_REPLACEMENT = with_replacement,
        RESAMPLING_ENSEMBLE = resampling_ensemble,
        SEED = seed
      )
    },
    #' @description Sets or overrides the data used by Tetrad.
    #' @param data (data.frame) The new data to load.
    set_data = function(data) {
      stopifnot(
        is.data.frame(data)
      )
      if (is.null(self$data) ||
        is.null(self$rdata) ||
        !isTRUE(all.equal(self$rdata, data))) {
        self$rdata <- data
        self$data <- rdata_to_tetrad(data)
      }
    },
    #' @description Toggles the verbosity in Tetrad.
    #' @param verbose (logical) TRUE to enable verbose logging, FALSE otherwise.
    set_verbose = function(verbose) {
      stopifnot(
        is.logical(verbose),
        length(verbose) == 1
      )
      self$set_params(
        VERBOSE = verbose
      )
    },
    #' @description Sets an integer time lag for time-series algorithms.
    #' @param time_lag (integer) The time lag to set.
    set_time_lag = function(time_lag = 0) {
      stopifnot(
        is.numeric(time_lag),
        floor(time_lag) == time_lag,
      )
      self$set_params(
        TIME_LAG = time_lag
      )
    },
    #' @description Retrieves the current Java data object.
    #' @return (Java object) Tetrad dataset.
    get_data = function() {
      return(self$data)
    },
    #' @description Returns the background knowledge object.
    #' @return (Java object) Tetrad Knowledge.
    get_knowledge = function() {
      return(self$knowledge)
    },
    #' @description Gets the main Java result object (usually a graph) from the last search.
    #' @return (Java object) The Tetrad result graph or model.
    get_java = function() {
      return(self$java)
    },
    #' @description Returns the string representation of a given Java object or \code{self$java}.
    #' @param java_obj (Java object, optional) If NULL, uses \code{self$java}.
    #' @return (character) The \code{toString()} of that Java object.
    get_string = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        return(.jcall(self$java, "S", "toString"))
      } else {
        return(.jcall(java_obj, "S", "toString"))
      }
    },
    #' @description Produces a DOT (Graphviz) representation of the graph.
    #' @param java_obj (Java object, optional) If NULL, uses \code{self$java}.
    #' @return (character) The DOT-format string.
    get_dot = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        self$java <- cast_obj(self$java)
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToDot",
          self$java
        ))
      } else {
        java_obj <- cast_obj(java_obj)
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToDot",
          java_obj
        ))
      }
    },
    #' @description Produces an amat representation of the graph.
    #' @param java_obj (Java object, optional) If NULL, uses \code{self$java}.
    #' @return (character) The DOT-format string.
    get_amat = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        self$java <- cast_obj(self$java)
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToPcalg",
          self$java
        ))
      } else {
        java_obj <- cast_obj(java_obj)
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToPcalg",
          java_obj
        ))
      }
    }
  ),

  # Scores and tests are private
  # and should be called through
  # set_score and set_test.
  private = list(
    # Scores
    use_basis_function_bic_score = function(truncation_limit = 3,
                                            penalty_discount = 2,
                                            singularity_lambda = 0.0,
                                            do_one_equation_only = FALSE) {
      stopifnot(
        is.numeric(c(truncation_limit, penalty_discount, singularity_lambda)),
        truncation_limit >= 0,
        floor(truncation_limit) == truncation_limit,
        penalty_discount >= 0,
        singularity_lambda >= 0,
        is.logical(do_one_equation_only),
        length(do_one_equation_only) == 1
      )
      self$set_params(
        TRUNCATION_LIMIT = truncation_limit,
        PENALTY_DISCOUNT = penalty_discount,
        SINGULARITY_LAMBDA = singularity_lambda,
        DO_ONE_EQUATION_ONLY = do_one_equation_only
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/BasisFunctionBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_bdeu_score = function(sample_prior = 10, structure_prior = 0) {
      stopifnot(
        is.numeric(c(sample_prior, structure_prior))
      )
      self$set_params(
        PRIOR_EQUIVALENT_SAMPLE_SIZE = sample_prior,
        STRUCTURE_PRIOR = structure_prior
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/BdeuScore")
      self$score <- cast_obj(self$score)
    },
    use_conditional_gaussian_score = function(penalty_discount = 1,
                                              discretize = TRUE,
                                              num_categories_to_discretize = 3,
                                              structure_prior = 0) {
      stopifnot(
        is.numeric(c(penalty_discount, num_categories_to_discretize)),
        penalty_discount >= 0,
        num_categories_to_discretize >= 0,
        floor(num_categories_to_discretize) == num_categories_to_discretize,
        is.logical(discretize),
        length(discretize) == 1
      )
      self$set_params(
        PENALTY_DISCOUNT = penalty_discount,
        STRUCTURE_PRIOR = structure_prior,
        DISCRETIZE = discretize,
        NUM_CATEGORIES_TO_DISCRETIZE = num_categories_to_discretize
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/ConditionalGaussianBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_degenerate_gaussian_score = function(penalty_discount = 1,
                                             structure_prior = 0,
                                             singularity_lambda = 0.0,
                                             precompute_covariances = TRUE) {
      stopifnot(
        is.numeric(c(penalty_discount, structure_prior, singularity_lambda)),
        penalty_discount >= 0,
        structure_prior >= 0,
        singularity_lambda >= 0,
        is.logical(precompute_covariances),
        length(precompute_covariances) == 1
      )
      self$set_params(
        PENALTY_DISCOUNT = penalty_discount,
        STRUCTURE_PRIOR = structure_prior,
        SINGULARITY_LAMBDA = singularity_lambda,
        PRECOMPUTE_COVARIANCES = precompute_covariances
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/DegenerateGaussianBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_discrete_bic_score = function(penalty_discount = 2,
                                      structure_prior = 0) {
      stopifnot(
        is.numeric(c(penalty_discount, structure_prior)),
        penalty_discount >= 0,
        structure_prior >= 0
      )
      self$set_params(
        PENALTY_DISCOUNT = penalty_discount,
        STRUCTURE_PRIOR = structure_prior
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/DiscreteBicScore")
      self$score <- cast_obj(self$score)
    },
    use_ebic_score = function(gamma = 0.8,
                              precompute_covariances = TRUE,
                              singularity_lambda = 0.0) {
      stopifnot(
        is.numeric(c(gamma, singularity_lambda)),
        gamma >= 0,
        singularity_lambda >= 0,
        is.logical(precompute_covariances),
        length(precompute_covariances) == 1
      )
      self$set_params(
        EBIC_GAMMA = gamma,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        SINGULARITY_LAMBDA = singularity_lambda
      )

      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/EbicScore")
      self$score <- cast_obj(self$score)
    },
    use_gic_score = function(penalty_discount = 1,
                             sem_gic_rule = "bic",
                             precompute_covariances = TRUE,
                             singularity_lambda = 0.0) {
      stopifnot(
        is.numeric(c(penalty_discount, singularity_lambda)),
        penalty_discount >= 0,
        singularity_lambda >= 0,
        is.character(sem_gic_rule),
      )
      sem_gic_rule_int <- switch(sem_gic_rule,
        "bic"  = 1L,
        "gic2" = 2L,
        "ric"  = 3L,
        "ricc" = 4L,
        "gic5" = 5L,
        "gic6" = 6L,
        stop("Unsupported gic rule:", sem_gic_rule)
      )
      self$set_params(
        SEM_GIC_RULE = sem_gic_rule_int,
        PENALTY_DISCOUNT_ZS = penalty_discount,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/GicScores")
      self$score <- cast_obj(self$score)
    },
    use_mag_degenerate_gaussian_bic_score = function(penalty_discount = 1,
                                                     structure_prior = 0,
                                                     precompute_covariances = TRUE) {
      stopifnot(
        is.numeric(c(penalty_discount, structure_prior)),
        penalty_discount >= 0,
        structure_prior >= 0,
        is.logical(precompute_covariances),
        length(precompute_covariances) == 1
      )
      self$set_params(
        PENALTY_DISCOUNT = penalty_discount,
        STRUCTURE_PRIOR = structure_prior,
        PRECOMPUTE_COVARIANCES = precompute_covariances
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/MagDgBicScore")
      self$score <- cast_obj(self$score)
    },
    use_mixed_variable_polynomial_score = function(structure_prior = 0,
                                                   f_degree = 0,
                                                   discretize = FALSE) {
      stopifnot(
        is.numeric(c(structure_prior, f_degree)),
        floor(f_degree) == f_degree,
        is.logical(discretize),
        length(discretize) == 1
      )
      self$set_params(
        STRUCTURE_PRIOR = structure_prior,
        DISCRETIZE = discretize
      )
      # f_degree is not a static field in Params so we set it manually.
      self$params$set(
        "fDegree",
        .jcast(
          .jnew("java/lang/Double", as.double(f_degree)),
          "java/lang/Object"
        )
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/MVPBicScore")
      self$score <- cast_obj(self$score)
    },
    use_poisson_prior_score = function(poission_lambda = 2,
                                       precompute_covariances = TRUE,
                                       singularity_lambda = 0.0) {
      stopifnot(
        is.numeric(c(poission_lambda, singularity_lambda)),
        poission_lambda >= 0,
        singularity_lambda >= 0,
        is.logical(precompute_covariances),
        length(precompute_covariances) == 1
      )
      self$set_params(
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        POISSON_LAMBDA = poission_lambda,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/PoissonPriorScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_sem_bic_score = function(penalty_discount = 2,
                                 structure_prior = 0,
                                 sem_bic_rule = 1,
                                 precompute_covariances = TRUE,
                                 singularity_lambda = 0.0) {
      stopifnot(
        is.numeric(singularity_lambda),
        singularity_lambda >= 0,
        is.numeric(c(penalty_discount, structure_prior)),
        floor(penalty_discount) == penalty_discount,
        floor(structure_prior) == structure_prior,
        sem_bic_rule %in% c(1, 2),
        is.logical(precompute_covariances),
        length(precompute_covariances) == 1
      )
      self$set_params(
        PENALTY_DISCOUNT = penalty_discount,
        SEM_BIC_STRUCTURE_PRIOR = structure_prior,
        SEM_BIC_RULE = sem_bic_rule,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/SemBicScore")
      self$score <- cast_obj(self$score)
    },
    use_zhang_shen_bound_score = function(risk_bound = 0.2,
                                          precompute_covariances = TRUE,
                                          singularity_lambda = 0.0) {
      stopifnot(
        is.numeric(c(risk_bound, singularity_lambda)),
        risk_bound >= 0,
        singularity_lambda >= 0,
        is.logical(precompute_covariances),
        length(precompute_covariances) == 1
      )
      self$set_params(self$params,
        ZS_RISK_BOUND = risk_bound,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/ZhangShenBoundScore"
      )
      self$score <- cast_obj(self$score)
    },
    # Tests
    use_basis_function_lrt_test = function(truncation_limit = 3,
                                           alpha = 0.01,
                                           singularity_lambda = 0.0,
                                           do_one_equation_only = FALSE,
                                           use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(truncation_limit, alpha, singularity_lambda)),
        truncation_limit >= 0,
        floor(truncation_limit) == truncation_limit,
        alpha >= 0,
        singularity_lambda >= 0,
        is.logical(do_one_equation_only),
        length(do_one_equation_only) == 1
      )
      self$set_params(
        ALPHA = alpha,
        TRUNCATION_LIMIT = truncation_limit,
        SINGULARITY_LAMBDA = singularity_lambda,
        DO_ONE_EQUATION_ONLY = do_one_equation_only
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/BasisFunctionLrt"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/BasisFunctionLrt"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_fisher_z_test = function(alpha = 0.01,
                                 singularity_lambda = 0.0,
                                 use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(alpha, singularity_lambda)),
        alpha >= 0,
        singularity_lambda >= 0
      )
      self$set_params(
        ALPHA = alpha,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/FisherZ"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew("edu/cmu/tetrad/algcomparison/independence/FisherZ")
        self$test <- cast_obj(self$test)
      }
    },
    use_chi_square_test = function(min_count = 1,
                                   alpha = 0.01,
                                   cell_table_type = 1,
                                   use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(min_count, alpha, cell_table_type)),
        min_count >= 0,
        alpha >= 0,
        floor(min_count) == min_count,
        floor(cell_table_type) == cell_table_type
      )
      self$set_params(
        ALPHA = alpha,
        MIN_COUNT_PER_CELL = min_count,
        CELL_TABLE_TYPE = cell_table_type
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ChiSquare"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ChiSquare"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_g_square_test = function(min_count = 1,
                                 alpha = 0.01,
                                 cell_table_type = 1,
                                 use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(min_count, alpha, cell_table_type)),
        min_count >= 0,
        alpha >= 0,
        floor(min_count) == min_count,
        floor(cell_table_type) == cell_table_type
      )
      self$set_params(
        ALPHA = alpha,
        MIN_COUNT_PER_CELL = min_count,
        CELL_TABLE_TYPE = cell_table_type
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/GSquare"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew("edu/cmu/tetrad/algcomparison/independence/GSquare")
        self$test <- cast_obj(self$test)
      }
    },
    use_conditional_gaussian_test = function(alpha = 0.01,
                                             discretize = TRUE,
                                             num_categories_to_discretize = 3,
                                             use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(alpha, num_categories_to_discretize)),
        alpha >= 0,
        num_categories_to_discretize >= 0,
        floor(num_categories_to_discretize) == num_categories_to_discretize,
        is.logical(discretize),
        length(discretize) == 1
      )
      self$set_p
      arams(
        ALPHA = alpha,
        DISCRETIZE = discretize,
        NUM_CATEGORIES_TO_DISCRETIZE = num_categories_to_discretize
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLrt"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLrt"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_degenerate_gaussian_test = function(alpha = 0.01,
                                            singularity_lambda = 0.0,
                                            use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(alpha, singularity_lambda)),
        alpha >= 0,
        singularity_lambda >= 0
      )
      self$set_params(
        ALPHA = alpha,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/DegenerateGaussianLrt"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/DegenerateGaussianLrt"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_probabilistic_test = function(threshold = FALSE,
                                      cutoff = 0.5,
                                      prior_ess = 10,
                                      use_for_mc = FALSE) {
      stopifnot(
        is.logical(c(threshold, use_for_mc)),
        length(threshold) == 1,
        length(use_for_mc) == 1,
        is.numeric(c(cutoff, prior_ess)),
        cutoff >= 0,
        prior_ess >= 0
      )
      self$set_params(
        NO_RANDOMLY_DETERMINED_INDEPENDENCE = threshold,
        CUTOFF_IND_TEST = cutoff,
        PRIOR_EQUIVALENT_SAMPLE_SIZE = prior_ess
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ProbabilisticTest"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ProbabilisticTest"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_kci_test = function(alpha = 0.01,
                            approximate = TRUE,
                            scaling_factor = 1,
                            num_bootstraps = 5000,
                            threshold = 1e-3,
                            kernel_type = "gaussian",
                            polyd = 5,
                            polyc = 1,
                            use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(alpha, scaling_factor, num_bootstraps, threshold, epsilon)),
        alpha >= 0,
        scaling_factor >= 0,
        num_bootstraps >= 0,
        floor(num_bootstraps) == num_bootstraps,
        threshold >= 0,
        epsilon >= 0,
        is.logical(c(approximate, use_for_mc)),
        length(approximate) == 1,
        length(use_for_mc) == 1,
        kernel_type %in% c("gaussian", "linear", "polynomial"),
        floor(polyd) == polyd,
        polyd >= 1,
      )

      switch(kernel_type,
        gaussian = self$set_params(KERNEL_TYPE = 1),
        linear = self$set_params(KERNEL_TYPE = 2),
        polynomial = self$set_params(KERNEL_TYPE = 3)
      )

      self$set_params(
        KCI_USE_APPROXIMATION = approximate,
        ALPHA = alpha,
        SCALING_FACTOR = scaling_factor,
        KCI_NUM_BOOTSTRAPS = num_bootstraps,
        THRESHOLD_FOR_NUM_EIGENVALUES = threshold,
        POLYNOMIAL_DEGREE = polyd,
        POLYNOMIAL_CONSTANT = polyc
      )
      if (use_for_mc) {
        self$mc_test <- .jnew("edu/cmu/tetrad/algcomparison/independence/Kci")
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- .jnew("edu/cmu/tetrad/algcomparison/independence/Kci")
        self$test <- cast_obj(self$test)
      }
    },
    use_cci_test = function(alpha = 0.01,
                            scaling_factor = 2,
                            basis_type = 4,
                            basis_scale = 0.0,
                            truncation_limit = 3,
                            use_for_mc = FALSE) {
      stopifnot(
        is.numeric(c(alpha, scaling_factor, truncation_limit)),
        alpha >= 0,
        scaling_factor >= 0,
        truncation_limit >= 0,
        floor(truncation_limit) == truncation_limit,
        is.logical(use_for_mc),
        length(use_for_mc) == 1,
        floor(basis_type) == basis_type,
        basis_scale >= 0
      )
      self$set_params(
        ALPHA = alpha,
        SCALING_FACTOR = scaling_factor,
        BASIS_TYPE = basis_type,
        BASIS_SCALE = basis_scale,
        TRUNCATION_LIMIT = truncation_limit
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/CciTest"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew("edu/cmu/tetrad/algcomparison/independence/CciTest")
        self$test <- cast_obj(self$test)
      }
    },
    # Algorithms
    set_fges_alg = function(symmetric_first_step = FALSE,
                            max_degree = -1,
                            parallelized = FALSE,
                            faithfulness_assumed = FALSE) {
      stopifnot(
        is.logical(symmetric_first_step),
        length(symmetric_first_step) == 1,
        is.numeric(max_degree),
        is.logical(parallelized),
        length(parallelized) == 1,
        is.logical(faithfulness_assumed),
        length(faithfulness_assumed) == 1
      )
      self$set_params(
        SYMMETRIC_FIRST_STEP = symmetric_first_step,
        MAX_DEGREE = max_degree,
        PARALLELIZED = parallelized,
        FAITHFULNESS_ASSUMED = faithfulness_assumed
      )
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Fges",
        self$score
      )

      # Set the knowledge object
      self$alg$setKnowledge(self$knowledge)
    },
    set_fges_mb_alg = function(targets = "",
                               max_degree = -1,
                               trimming_style = 3,
                               number_of_expansions = 2,
                               faithfulness_assumed = FALSE) {
      stopifnot(
        is.character(targets),
        length(targets) == 1,
        is.numeric(max_degree),
        max_degree >= -1,
        is.numeric(trimming_style),
        floor(trimming_style) == trimming_style,
        is.numeric(number_of_expansions),
        floor(number_of_expansions) == number_of_expansions,
        number_of_expansions >= 0,
        is.logical(faithfulness_assumed),
        length(faithfulness_assumed) == 1
      )
      self$set_params(
        TARGETS = targets,
        FAITHFULNESS_ASSUMED = faithfulness_assumed,
        MAX_DEGREE = max_degree,
        TRIMMING_STYLE = trimming_style,
        NUMBER_OF_EXPANSIONS = number_of_expansions
      )
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/FgesMb",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_boss_alg = function(num_starts = 1,
                            use_bes = FALSE,
                            use_data_order = TRUE,
                            output_cpdag = TRUE) {
      stopifnot(
        is.numeric(c(num_starts, time_lag)),
        floor(num_starts) == num_starts,
        num_starts >= 1,
        is.logical(c(use_bes, use_data_order, output_cpdag)),
        length(use_bes) == 1,
        length(use_data_order) == 1,
        length(output_cpdag) == 1
      )
      self$set_params(
        USE_BES = use_bes,
        NUM_STARTS = num_starts,
        USE_DATA_ORDER = use_data_order,
        OUTPUT_CPDAG = output_cpdag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Boss",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_restricted_boss_alg = function(targets = "",
                                       use_bes = FALSE,
                                       num_starts = 1,
                                       allow_internal_randomness = TRUE) {
      stopifnot(
        is.character(targets),
        length(targets) == 1,
        is.numeric(num_starts),
        floor(num_starts) == num_starts,
        num_starts >= 1,
        is.logical(c(use_bes, allow_internal_randomness)),
        length(use_bes) == 1,
        length(allow_internal_randomness) == 1
      )
      self$set_params(
        TARGETS = targets,
        USE_BES = use_bes,
        NUM_STARTS = num_starts,
        ALLOW_INTERNAL_RANDOMNESS = allow_internal_randomness
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/RestrictedBoss",
        self$score
      )
    },
    set_cstar_alg = function(targets = "",
                             file_out_path = "cstar-out",
                             selection_min_effect = 0.0,
                             num_subsamples = 50,
                             top_bracket = 10,
                             parallelized = FALSE,
                             cpdag_algorithm = 4,
                             remove_effect_nodes = TRUE,
                             sample_style = 1) {
      stopifnot(
        is.character(targets),
        length(targets) == 1,
        is.numeric(c(selection_min_effect, num_subsamples, top_bracket)),
        selection_min_effect >= 0,
        num_subsamples >= 1,
        floor(num_subsamples) == num_subsamples,
        top_bracket >= 1,
        floor(top_bracket) == top_bracket,
        is.logical(c(parallelized, remove_effect_nodes)),
        length(parallelized) == 1,
        length(remove_effect_nodes) == 1,
        floor(cpdag_algorithm) == cpdag_algorithm,
        floor(sample_style) == sample_style
      )
      self$set_params(
        SELECTION_MIN_EFFECT = selection_min_effect,
        NUM_SUBSAMPLES = num_subsamples,
        TARGETS = targets,
        TOP_BRACKET = top_bracket,
        PARALLELIZED = parallelized,
        CSTAR_CPDAG_ALGORITHM = cpdag_algorithm,
        FILE_OUT_PATH = file_out_path,
        REMOVE_EFFECT_NODES = remove_effect_nodes,
        SAMPLE_STYLE = sample_style
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Cstar",
        self$test,
        self$score
      )
    },
    set_sp_alg = function() {
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Sp",
        self$score
      )
    },
    set_grasp_alg = function(covered_depth = 4,
                             singular_depth = 1,
                             nonsingular_depth = 1,
                             ordered_alg = FALSE,
                             raskutti_uhler = FALSE,
                             use_data_order = TRUE,
                             num_starts = 1) {
      stopifnot(
        is.numeric(c(covered_depth, singular_depth, nonsingular_depth)),
        floor(covered_depth) == covered_depth,
        floor(singular_depth) == singular_depth,
        floor(nonsingular_depth) == nonsingular_depth,
        is.logical(c(ordered_alg, raskutti_uhler, use_data_order)),
        length(ordered_alg) == 1,
        length(raskutti_uhler) == 1,
        length(use_data_order) == 1,
        is.numeric(num_starts),
        floor(num_starts) == num_starts,
        num_starts >= 1
      )
      self$set_params(
        GRASP_DEPTH = covered_depth,
        GRASP_SINGULAR_DEPTH = singular_depth,
        GRASP_NONSINGULAR_DEPTH = nonsingular_depth,
        GRASP_ORDERED_ALG = ordered_alg,
        GRASP_USE_RASKUTTI_UHLER = raskutti_uhler,
        USE_DATA_ORDER = use_data_order,
        NUM_STARTS = num_starts
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Grasp",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_pc_alg = function(conflict_rule = 1,
                          depth = -1,
                          stable_fas = TRUE,
                          guarantee_cpdag = FALSE) {
      stopifnot(
        is.numeric(conflict_rule),
        length(conflict_rule) == 1,
        is.numeric(depth),
        length(depth) == 1,
        is.logical(stable_fas),
        length(stable_fas) == 1,
        is.logical(guarantee_cpdag),
        length(guarantee_cpdag) == 1
      )
      self$set_params(
        CONFLICT_RULE = conflict_rule,
        DEPTH = depth,
        STABLE_FAS = stable_fas,
        GUARANTEE_CPDAG = guarantee_cpdag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Pc",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_cpc_alg = function(conflict_rule = 1,
                           depth = -1,
                           stable_fas = TRUE,
                           guarantee_cpdag = TRUE) {
      stopifnot(
        is.numeric(conflict_rule),
        length(conflict_rule) == 1,
        is.numeric(depth),
        length(depth) == 1,
        is.logical(stable_fas),
        length(stable_fas) == 1,
        is.logical(guarantee_cpdag),
        length(guarantee_cpdag) == 1
      )
      self$set_params(
        CONFLICT_RULE = conflict_rule,
        DEPTH = depth,
        STABLE_FAS = stable_fas,
        GUARANTEE_CPDAG = guarantee_cpdag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Cpc",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_pcmax_alg = function(conflict_rule = 1,
                             depth = -1,
                             use_heuristic = TRUE,
                             max_disc_path_length = -1,
                             stable_fas = TRUE) {
      stopifnot(
        is.numeric(conflict_rule),
        length(conflict_rule) == 1,
        is.numeric(depth),
        length(depth) == 1,
        is.logical(use_heuristic),
        length(use_heuristic) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        is.logical(stable_fas),
        length(stable_fas) == 1
      )
      self$set_params(
        CONFLICT_RULE = conflict_rule,
        DEPTH = depth,
        USE_MAX_P_ORIENTATION_HEURISTIC = use_heuristic,
        MAX_P_ORIENTATION_MAX_PATH_LENGTH = max_disc_path_length,
        STABLE_FAS = stable_fas
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/PcMax",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_fci_alg = function(depth = -1,
                           stable_fas = TRUE,
                           max_disc_path_length = -1,
                           complete_rule_set_used = TRUE,
                           guarantee_pag = FALSE) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        is.logical(stable_fas),
        length(stable_fas) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1,
        is.logical(guarantee_pag),
        length(guarantee_pag) == 1
      )
      self$set_params(
        DEPTH = depth,
        STABLE_FAS = stable_fas,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used,
        GUARANTEE_PAG = guarantee_pag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Fci",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_rfci_alg = function(depth = -1,
                            stable_fas = TRUE,
                            max_disc_path_length = -1,
                            complete_rule_set_used = TRUE) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        is.logical(stable_fas),
        length(stable_fas) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1
      )
      self$set_params(
        DEPTH = depth,
        STABLE_FAS = stable_fas,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Rfci",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_cfci_alg = function(depth = -1,
                            max_disc_path_length = -1,
                            complete_rule_set_used = TRUE) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1
      )
      self$set_params(
        DEPTH = depth,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Cfci",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_gfci_alg = function(depth = -1,
                            max_degree = -1,
                            max_disc_path_length = -1,
                            complete_rule_set_used = TRUE,
                            guarantee_pag = FALSE) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        is.numeric(max_degree),
        length(max_degree) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1,
        is.logical(guarantee_pag),
        length(guarantee_pag) == 1
      )
      self$set_params(
        DEPTH = depth,
        MAX_DEGREE = max_degree,
        COMPLETE_RULE_SET_USED = complete_rule_set_used,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        GUARANTEE_PAG = guarantee_pag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Gfci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_bfci_alg = function(depth = -1,
                            max_disc_path_length = -1,
                            complete_rule_set_used = TRUE,
                            guarantee_pag = FALSE) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1,
        is.logical(guarantee_pag),
        length(guarantee_pag) == 1
      )
      self$set_params(
        DEPTH = depth,
        COMPLETE_RULE_SET_USED = complete_rule_set_used,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        GUARANTEE_PAG = guarantee_pag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Bfci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_lv_lite_alg = function(num_starts = 1,
                               max_blocking_path_length = 5,
                               depth = 5,
                               max_disc_path_length = 5,
                               guarantee_pag = TRUE) {
      stopifnot(
        is.numeric(c(num_starts, max_blocking_path_length, depth, max_disc_path_length)),
        floor(num_starts) == num_starts,
        num_starts >= 1,
        max_blocking_path_length >= 0,
        depth >= 0,
        max_disc_path_length >= 0,
        is.logical(guarantee_pag),
        length(guarantee_pag) == 1
      )
      self$set_params(
        NUM_STARTS = num_starts,
        MAX_BLOCKING_PATH_LENGTH = max_blocking_path_length,
        DEPTH = depth,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        GUARANTEE_PAG = guarantee_pag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/LvLite",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_grasp_fci_alg = function(depth = -1,
                                 stable_fas = TRUE,
                                 max_disc_path_length = -1,
                                 complete_rule_set_used = TRUE,
                                 covered_depth = 4,
                                 singular_depth = 1,
                                 nonsingular_depth = 1,
                                 ordered_alg = FALSE,
                                 raskutti_uhler = FALSE,
                                 use_data_order = TRUE,
                                 num_starts = 1,
                                 guarantee_pag = FALSE) {
      stopifnot(
        is.numeric(c(depth, max_disc_path_length, covered_depth, singular_depth, nonsingular_depth)),
        floor(depth) == depth,
        floor(max_disc_path_length) == max_disc_path_length,
        floor(covered_depth) == covered_depth,
        floor(singular_depth) == singular_depth,
        floor(nonsingular_depth) == nonsingular_depth,
        is.logical(c(stable_fas, ordered_alg, raskutti_uhler, use_data_order, guarantee_pag)),
        length(stable_fas) == 1,
        length(ordered_alg) == 1,
        length(raskutti_uhler) == 1,
        length(use_data_order) == 1,
        length(guarantee_pag) == 1,
        is.numeric(num_starts),
        floor(num_starts) == num_starts,
        num_starts >= 1
      )
      self$set_params(
        GRASP_DEPTH = covered_depth,
        GRASP_SINGULAR_DEPTH = singular_depth,
        GRASP_NONSINGULAR_DEPTH = nonsingular_depth,
        GRASP_ORDERED_ALG = ordered_alg,
        GRASP_USE_RASKUTTI_UHLER = raskutti_uhler,
        USE_DATA_ORDER = use_data_order,
        NUM_STARTS = num_starts,
        GUARANTEE_PAG = guarantee_pag,
        DEPTH = depth,
        STABLE_FAS = stable_fas,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/GraspFci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_spfci_alg = function(max_disc_path_length = -1,
                             complete_rule_set_used = TRUE,
                             depth = -1,
                             guarantee_pag = FALSE) {
      stopifnot(
        is.numeric(c(max_disc_path_length, depth)),
        max_disc_path_length >= 0,
        depth >= 0,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1,
        is.logical(guarantee_pag),
        length(guarantee_pag) == 1
      )
      self$set_params(
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used,
        DEPTH = depth,
        GUARANTEE_PAG = guarantee_pag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/SpFci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_ica_lingam_alg = function(ica_a = 1.1,
                                  ica_max_iter = 5000,
                                  ica_tolerance = 1e-8,
                                  threshold_b = 0.1) {
      stopifnot(
        is.numeric(c(ica_a, ica_max_iter, ica_tolerance, threshold_b)),
        ica_a >= 0,
        ica_max_iter >= 0,
        floor(ica_max_iter) == ica_max_iter,
        ica_tolerance >= 0,
        threshold_b >= 0
      )
      self$set_params(
        FAST_ICA_A = ica_a,
        FAST_ICA_MAX_ITER = ica_max_iter,
        FAST_ICA_TOLERANCE = ica_tolerance,
        THRESHOLD_B = threshold_b
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/IcaLingam",
        self$score
      )
    },
    set_ica_lingd_alg = function(ica_a = 1.1,
                                 ica_max_iter = 5000,
                                 ica_tolerance = 1e-8,
                                 threshold_b = 0.1,
                                 threshold_w = 0.1) {
      stopifnot(
        is.numeric(c(ica_a, ica_max_iter, ica_tolerance, threshold_b, threshold_w)),
        ica_a >= 0,
        ica_max_iter >= 0,
        floor(ica_max_iter) == ica_max_iter,
        ica_tolerance >= 0,
        threshold_b >= 0,
        threshold_w >= 0
      )
      self$set_params(
        FAST_ICA_A = ica_a,
        FAST_ICA_MAX_ITER = ica_max_iter,
        FAST_ICA_TOLERANCE = ica_tolerance,
        THRESHOLD_B = threshold_b,
        THRESHOLD_W = threshold_w
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/IcaLingD",
        self$score
      )
    },
    set_fask_alg = function(alpha = 0.05,
                            depth = -1,
                            fask_delta = -0.3,
                            left_right_rule = 1,
                            skew_edge_threshold = 0.3) {
      stopifnot(
        is.numeric(c(alpha, depth, fask_delta, skew_edge_threshold)),
        alpha >= 0,
        depth >= -1,
        fask_delta >= -1,
        skew_edge_threshold >= 0,
        floor(depth) == depth,
        floor(left_right_rule) == left_right_rule
      )
      self$set_params(
        ALPHA = alpha,
        DEPTH = depth,
        FASK_DELTA = fask_delta,
        FASK_LEFT_RIGHT_RULE = left_right_rule,
        SKEW_EDGE_THRESHOLD = skew_edge_threshold
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/Fask",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_fofc_alg = function(alpha = 0.001,
                            penalty_discount = 2.0,
                            tetrad_test = 1,
                            include_structure_model = TRUE,
                            precompute_covariances = TRUE) {
      stopifnot(
        is.numeric(c(alpha, penalty_discount)),
        alpha >= 0,
        penalty_discount >= 0,
        floor(tetrad_test) == tetrad_test,
        is.logical(c(include_structure_model, precompute_covariances)),
        length(include_structure_model) == 1,
        length(precompute_covariances) == 1
      )
      self$set_params(
        ALPHA = alpha,
        PENALTY_DISCOUNT = penalty_discount,
        TETRAD_TEST_FOFC = tetrad_test,
        INCLUDE_STRUCTURE_MODEL = include_structure_model,
        PRECOMPUTE_COVARIANCES = precompute_covariances
      )

      self$alg <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/cluster/Fofc")
    },
    set_ccd_alg = function(depth = -1, apply_r1 = TRUE) {
      stopifnot(
        is.numeric(c(depth)),
        depth >= -1,
        is.logical(apply_r1),
        length(apply_r1) == 1
      )
      stopifnot(
        is.numerical(depth),
        depth >= -1,
        floor(depth) == depth,
        is.logical(apply_r1),
        length(apply_r1) == 1
      )
      self$set_params(self$params,
        DEPTH = depth,
        APPLY_R1 = apply_r1
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Ccd",
        self$test
      )
    },
    set_svar_fci_alg = function(penalty_discount = 2) {
      stopifnot(
        is.numeric(penalty_discount),
        penalty_discount >= 0
      )
      num_lags <- 2
      # Create lagged data using Java method.
      lagged_data <- .jcall(
        "edu/cmu/tetrad/search/TimeSeriesUtils",
        "Ljava/lang/Object;",
        "createLagData",
        self$data,
        num_lags
      )
      ts_test <- .jnew(
        "edu/cmu/tetrad/search/utils/IndTestFisherZ",
        lagged_data,
        0.01
      )
      ts_score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/SemBicScore",
        lagged_data
      )

      ts_score$setPenaltyDiscount(.jcast(
        .jnew("java/lang/Double", as.double(penalty_discount)),
        "java/lang/Object"
      ))
      svar_fci <- .jnew("edu/cmu/tetrad/search/utils/SvarFci", ts_test)
      svar_fci$setKnowledge(lagged_data$getKnowledge())
      svar_fci$setVerbose(TRUE)
      self$alg <- svar_fci
    },
    set_direct_lingam_alg = function() {
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/DirectLingam",
        self$score
      )
    },
    set_dagma_alg = function(lambda1 = 0.05, w_threshold = 0.1, cpdag = TRUE) {
      stopifnot(
        is.numeric(c(lambda1, w_threshold)),
        lambda1 >= 0,
        w_threshold >= 0,
        is.logical(cpdag),
        length(cpdag) == 1
      )
      self$set_params(
        LAMBDA1 = lambda1,
        W_THRESHOLD = w_threshold,
        CPDAG = cpdag
      )
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/Dagma"
      )
    },
    set_pc_lingam_alg = function() {
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/PcLingam"
      )
    },
    set_svar_gfci_alg = function(penalty_discount = 2) {
      stopifnot(
        is.numeric(penalty_discount),
        penalty_discount >= 0
      )
      num_lags <- 2
      lagged_data <- .jcall(
        "edu/cmu/tetrad/search/utils/TsUtils",
        "Ljava/lang/Object;",
        "createLagData",
        self$data,
        num_lags
      )
      ts_test <- .jnew(
        "edu/cmu/tetrad/search/test/IndTestFisherZ",
        lagged_data,
        0.01
      )
      ts_score <- .jnew(
        "edu/cmu/tetrad/search/score/SemBicScore",
        lagged_data,
        TRUE
      )
      ts_score$setPenaltyDiscount(.jcast(
        .jnew("java/lang/Double", as.double(penalty_discount)),
        "java/lang/Object"
      ))
      svar_gfci <- .jnew(
        "edu/cmu/tetrad/search/utils/SvarGfci",
        ts_test,
        ts_score
      )
      svar_gfci$setKnowledge(lagged_data$getKnowledge())
      self$alg <- svar_gfci
    }
  )
)
