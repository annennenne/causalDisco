# See https://github.com/r-lib/roxygen2/issues/1158 for why this is needed
#' @title R6 Interface to Tetrad Search Algorithms
#'
#' @name TetradSearch
#'
#' @example inst/roxygen-examples/TetradSearch_example.R
NULL

#' @title R6 Interface to Tetrad Search Algorithms
#'
#' @description
#' High-level wrapper around the Java-based **Tetrad** causal-discovery
#' library. The class lets you choose independence tests, scores, and search
#' algorithms from Tetrad, run them on an R data set, and retrieve the
#' resulting graph or statistics.
#'
#' @docType class
#' @name TetradSearch
#' @rdname TetradSearch
#' @importFrom R6 R6Class
#' @export
TetradSearch <- R6Class(
  # nolint: object_name_linter.
  "TetradSearch",
  public = list(
    #' @field data Java object that stores the (possibly converted) data set
    #'  used by Tetrad.
    data = NULL,

    #' @field rdata Original **R** `data.frame` supplied by the user.
    rdata = NULL,

    #' @field score Java object holding the scoring function selected with
    #'   \code{set_score()}. Supply one of the method strings for
    #'   \code{set_score()}. Recognised values are:
    #'
    #'   \itemize{
    #'      \item \code{"sem_bic"} - SEM BIC score.
    #'      \item \code{"ebic"} - Extended BIC score.
    #'      \item \code{"bdeu"} - Bayes Dirichlet Equivalent score with uniform priors.
    #'      \item \code{"basis_function_bic"} - BIC score for basis-function models.
    #'        This is a generalization of the Degenerate Gaussian score.
    #'      \item \code{"conditional_gaussian"} - Mixed discrete/continuous BIC score.
    #'      \item \code{"degenerate_gaussian"} - Degenerate Gaussian BIC score.
    #'      \item \code{"discrete_bic"} - BIC score for discrete data.
    #'      \item \code{"gic"} - Generalized Information Criterion (GIC) score.
    #'      \item \code{"mag_degenerate_gaussian_bic"} - MAG Degenerate Gaussian BIC Score.
    #'      \item \code{"mixed_variable_polynomial"} - Mixed variable polynomial BIC score.
    #'      \item \code{"poisson_prior"} - Poisson prior score.
    #'      \item \code{"zhang_shen_bound"} - Gaussian Extended BIC score.
    #'   }
    score = NULL,

    #' @field test Java object holding the independence test selected with
    #'   \code{set_test()}. Supply one of the method strings for
    #'   \code{set_test()}. Recognised values are:
    #'
    #'   \itemize{
    #'     \item \code{"chi_square"} - chi-squared test
    #'     \item \code{"g_square"}   - likelihood-ratio \eqn{G^2} test
    #'     \item \code{"basis_function_lrt"} - basis-function likelihood-ratio
    #'     \item \code{"probabilistic"} - Uses BCInference by Cooper and Bui to calculate
    #'        probabilistic conditional independence judgments.
    #'     \item \code{"fisher_z"} - Fisher \eqn{Z} (partial correlation) test
    #'     \item \code{"degenerate_gaussian"} - Degenerate Gaussian test as a likelihood ratio test
    #'     \item \code{"cci"} - Conditional independence of variable in a continuous data set using Daudin's method.
    #'     \item \code{"conditional_gaussian"} - Mixed discrete/continuous test
    #'     \item \code{"kci"} - Kernel Conditional Independence Test (KCI) by Kun Zhang
    #'   }
    test = NULL,

    #' @field alg Java object representing the search algorithm.
    #'  Supply one of the method strings for \code{set_alg()}.
    #'  Recognised values are:
    #'
    #' \itemize{
    #'   \item \code{"boss"} - BOSS algorithm.
    #'   \item \code{"boss_fci"} - BOSS-FCI algorithm.
    #'   \item \code{"boss_pod"} - BOSS-POD (BOSS-PAG of DAG) algorithm.
    #'   \item \code{"ccd"} - Cyclic Causal Discovery.
    #'   \item \code{"cfci"} - Adjusts FCI to use conservative orientation as in CPC.
    #'   \item \code{"cpc"} - Conservative PC algorithm.
    #'   \item \code{"cstar"} - CStaR algorithm (Causal Stability Ranking).
    #'   \item \code{"dagma"} - DAGMA algorithm.
    #'   \item \code{"direct_lingam"} - DirectLiNGAM algorithm.
    #'   \item \code{"fask"} - FASK algorithm.
    #'   \item \code{"fci"} - FCI algorithm.
    #'   \item \code{"fges"} - Fast Greedy Equivalence Search (FGES) algorithm.
    #'   \item \code{"fges_mb"} - Fast Greedy Equivalence Search with Markov Blanket (FGES-MB) algorithm.
    #'   \item \code{"fofc"} - Find One Factor Clusters (FOFC)/MIMBUILD algorithm.
    #'   \item \code{"gfci"} - GFCI algorithm. Combines FGES and FCI.
    #'   \item \code{"grasp"} - GRaSP (Greedy Relations of Sparsest Permutation) algorithm.
    #'   \item \code{"grasp_fci"} - GRaSP-FCI algorithm. Combines GRaSP and FCI.
    #'   \item \code{"ica_lingam"} - ICA LiNGAM algorithm.
    #'   \item \code{"ica_lingd"} - ICA-LiNG-D algorithm
    #'   \item \code{"fcit"} - FCI Targeted Testing (FCIT) algorithm
    #'   \item \code{"pc"} - Peter-Clark (PC) algorithm
    #'   \item \code{"pc_max"} - PCMax algorithm
    #'   \item \code{"restricted_boss"} - Restricted BOSS algorithm
    #'   \item \code{"rfci"} - Restricted FCI algorithm
    #'   \item \code{"sp"} - Sparsest Permutation algorithm
    #'   \item \code{"sp_fci"} - Sparsest Permutation using FCI
    #'   \item \code{"svar_fci"} - SvarFCI algorithm (for timeseries data)
    #'   \item \code{"svar_gfci"} - SvarGFCI algorithm. Similar to SvarFCI,
    #'      but uses a BIC score to search for a skeleton.
    #' }
    alg = NULL,

    #' @field mc_test Java independence-test object used by the Markov checker.
    mc_test = NULL,

    #' @field java Java object returned by the search (typically a graph).
    java = NULL,

    #' @field result Convenience alias for \code{java}; may store additional
    #'  metadata depending on the search type.
    result = NULL,

    #' @field knowledge Java \code{Knowledge} object carrying background
    #'  constraints (required/forbidden edges).
    knowledge = NULL,

    #' @field params Java \code{Parameters} object holding algorithm settings.
    params = NULL,

    #' @field bootstrap_graphs Java \code{List} of graphs produced by bootstrap
    #'  resampling, if that feature was requested.
    bootstrap_graphs = NULL,

    #' @field mc_ind_results Java \code{List} with Markov-checker test results.
    mc_ind_results = NULL,

    #' @description Initializes the \code{TetradSearch} object, creating new Java objects for
    #'   \code{knowledge} and \code{params}.
    initialize = function() {
      .check_if_pkgs_are_installed(
        pkgs = c(
          "rJava"
        ),
        function_name = "TetradSearch"
      )

      if (!rJava::.jniInitialized) {
        init_java() # nocov
      }
      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$mc_test <- NULL
      self$knowledge <- rJava::.jnew("edu/cmu/tetrad/data/Knowledge")
      self$params <- rJava::.jnew("edu/cmu/tetrad/util/Parameters")
      self$bootstrap_graphs <- NULL
      self$set_verbose(FALSE) # Set verbose to FALSE per default.
    },

    #' @description Sets the independence test to use in Tetrad.
    #' @param method (character) Name of the test method (e.g., "chi_square", "fisher_z").
    #'   \itemize{
    #'     \item \code{"chi_square"} - chi-squared test
    #'     \item \code{"g_square"}   - likelihood-ratio \(G^2\) test
    #'     \item \code{"basis_function_lrt"} - basis-function likelihood-ratio
    #'     \item \code{"probabilistic"} - Uses BCInference by Cooper and Bui to calculate
    #'        probabilistic conditional independence judgments.
    #'     \item \code{"fisher_z"} - Fisher \(Z\) (partial correlation) test
    #'     \item \code{"degenerate_gaussian"} - Degenerate Gaussian test as a likelihood ratio test
    #'     \item \code{"cci"} - Conditional independence of variable in a continuous data set using Daudin's method.
    #'     \item \code{"conditional_gaussian"} - Mixed discrete/continuous test
    #'     \item \code{"kci"} - Kernel Conditional Independence Test (KCI) by Kun Zhang
    #'   }
    #' @param ... Additional arguments passed to the private test-setting methods.
    #'   For the following tests, the following parameters are available:
    #'     \itemize{
    #'       \item \code{"chi_square"} - chi-squared test
    #'       \itemize{
    #'          \item \code{min_count = 1} - Minimum count for the chi-squared
    #'          test per cell. Increasing this can improve accuracy of the test
    #'          estimates,
    #'          \item \code{alpha = 0.01} - Significance level for the
    #'          independence test,
    #'          \item \code{cell_table_type = "ad"} - The type of cell table to
    #'          use for optimization. Available types are:
    #'          \code{"ad"} - AD tree, \code{"count"} - Count sample.
    #'       }
    #'       \item \code{"g_square"}   - likelihood-ratio \(G^2\) test
    #'       \itemize{
    #'          \item \code{min_count = 1} - Minimum count for the independence
    #'          test. Increasing this can improve accuracy of chi square
    #'          estimates,
    #'          \item \code{alpha = 0.01} - Significance level for the
    #'          chi-squared test,
    #'          \item \code{cell_table_type = "ad"} - The type of cell table to
    #'          use for optimization. Available types are:
    #'          \code{"ad"} - AD tree, \code{"count"} - Count sample.
    #'       }
    #'       \item \code{"basis_function_lrt"} - basis-function likelihood-ratio
    #'       \itemize{
    #'          \item \code{truncation_limit = 3} - Basis functions 1 through
    #'          this number will be used. The Degenerate Gaussian category
    #'          indicator variables for mixed data are also used,
    #'          \item \code{alpha = 0.01} - Significance level for the
    #'          likelihood-ratio test,
    #'          \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'          lambda to the diagonal, < 0 Pseudoinverse,
    #'          \item \code{do_one_equation_only = FALSE} - If TRUE, only one
    #'          equation should be used when expanding the basis.
    #'       }
    #'       \item \code{"probabilistic"} - Uses BCInference by Cooper and Bui
    #'        to calculate probabilistic conditional independence judgments.
    #'       \itemize{
    #'          \item \code{threshold = FALSE} - Set to TRUE if using the cutoff
    #'          threshold for the independence test,
    #'          \item \code{cutoff = 0.5} - Cutoff for the independence test,
    #'          \item \code{prior_ess = 10} - Prior equivalent sample size
    #'          for the independence test. This number is added to the sample
    #'          size for each conditional probability table in the model and is
    #'          divided equally among the cells in the table.
    #'       }
    #'       \item \code{"fisher_z"} - Fisher \(Z\) (partial correlation) test
    #'       \itemize{
    #'          \item \code{alpha = 0.01} - Significance level
    #'          for the independence test,
    #'          \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'          lambda to the diagonal, < 0 Pseudoinverse.
    #'       }
    #'       \item \code{"degenerate_gaussian"} - Degenerate Gaussian
    #'       likelihood ratio test
    #'       \itemize{
    #'          \item \code{alpha = 0.01} - Significance level for the
    #'          independence test,
    #'          \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'          lambda to the diagonal, < 0 Pseudoinverse.
    #'       }
    #'       \item \code{"cci"} - Conditional independence of variable in a
    #'       continuous data set using Daudin's method.
    #'       \itemize{
    #'          \item \code{alpha = 0.01} - Significance level for the
    #'          independence test,
    #'          \item \code{scaling_factor = 2} - For Gaussian kernel: The
    #'          scaling factor * Silverman bandwidth.
    #'          \item \code{basis_type = "legendre"} -
    #'          The type of basis function to use. Available types are
    #'          \code{"polynomial"}, \code{"hermite1"}, \code{"hermite2"},
    #'          \code{"legendre"}, or \code{"chebyshev"}.
    #'          \item \code{basis_scale = 0.0} - The scale of the basis function.
    #'          Variables are scaled to `[-b, b]` for this b (0 = standardized).
    #'          \item \code{truncation_limit = 3} - Basis functions 1 through
    #'          this number will be used. The Degenerate Gaussian category
    #'          indicator variables for mixed data are also used.
    #'       }
    #'       \item \code{"conditional_gaussian"} - Mixed discrete/continuous test
    #'       \itemize{
    #'          \item \code{alpha = 0.01} - Significance level for the
    #'           independence test,
    #'          \item \code{discretize = TRUE} - If TRUE for the conditional
    #'           Gaussian likelihood, when scoring X --> D where X is continuous
    #'           and D discrete, one should to simply discretize X for just
    #'           those cases.
    #'           If FALSE, the integration will be exact,
    #'          \item \code{num_categories_to_discretize = 3} - In case the exact
    #'           algorithm is not used for discrete children and continuous
    #'           parents is not used, this parameter gives the number of
    #'           categories to use for this second (discretized) backup copy of
    #'           the continuous variables,
    #'          \item \code{min_sample_size_per_cell = 4} - Minimum sample size
    #'          per cell for the independence test.
    #'       }
    #'       \item \code{"kci"} - Kernel Conditional Independence Test (KCI) by Kun Zhang
    #'       \itemize{
    #'          \item \code{alpha = 0.01} - Significance level for the
    #'          independence test,
    #'          \item \code{approximate = TRUE} - If TRUE, use the approximate
    #'          Gamma approximation algorithm. If FALSE, use the exact,
    #'          \item \code{scaling_factor = 1} - For Gaussian kernel: The
    #'          scaling factor * Silverman bandwidth.
    #'          \item \code{num_bootstraps = 5000} - Number of bootstrap
    #'          samples to use for the KCI test.
    #'          \item \code{threshold = 1e-3} - Threshold for the KCI test.
    #'          Threshold to determine how many eigenvalues to use --
    #'          the lower the more (0 to 1).
    #'          \item \code{kernel_type = "gaussian"} - The type of kernel to
    #'          use. Available types are \code{"gaussian"}, \code{"linear"}, or
    #'          \code{"polynomial"}.
    #'          \item \code{polyd = 5} - The degree of the polynomial kernel,
    #'          if used.
    #'          \item \code{polyc = 1} - The constant of the polynomial kernel,
    #'          if used.
    #'       }
    #'     }
    #' @param mc (logical) If TRUE, sets this test for the Markov checker \code{mc_test}.
    #' @return Invisibly returns \code{self}, for chaining.
    set_test = function(method, ..., mc = FALSE) {
      stopifnot(
        is.logical(mc),
        length(mc) == 1
      )
      method <- tolower(method)
      switch(
        method,
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
    #'   \itemize{
    #'      \item \code{"sem_bic"} - SEM BIC score.
    #'      \item \code{"ebic"} - Extended BIC score.
    #'      \item \code{"bdeu"} - Bayes Dirichlet Equivalent score with uniform priors.
    #'      \item \code{"basis_function_bic"} - BIC score for basis-function models.
    #'          This is a generalization of the Degenerate Gaussian score.
    #'      \item \code{"conditional_gaussian"} - Mixed discrete/continuous BIC score.
    #'      \item \code{"degenerate_gaussian"} - Degenerate Gaussian BIC score.
    #'      \item \code{"discrete_bic"} - BIC score for discrete data.
    #'      \item \code{"gic"} - Generalized Information Criterion (GIC) score.
    #'      \item \code{"mag_degenerate_gaussian_bic"} - MAG Degenerate Gaussian BIC Score.
    #'      \item \code{"mixed_variable_polynomial"} - Mixed variable polynomial BIC score.
    #'      \item \code{"poisson_prior"} - Poisson prior score.
    #'      \item \code{"zhang_shen_bound"} - Gaussian Extended BIC score.
    #'   }
    #' @param ... Additional arguments passed to the private score-setting methods.
    #'    For the following scores, the following parameters are available:
    #'    \itemize{
    #'    \item \code{sem_bic} - SEM BIC score.
    #'      \itemize{
    #'        \item \code{penalty_discount = 2} - Penalty discount factor used in
    #'        BIC = 2L - ck log N, where c is the penalty. Higher c yield sparser
    #'        graphs,
    #'        \item \code{structure_prior = 0} - The default number of parents
    #'        for any conditional probability table. Higher weight is accorded
    #'        to tables with about that number of parents. The prior structure
    #'        weights are distributed according to a binomial distribution,
    #'        \item \code{sem_bic_rule = 1} - The Chickering Rule uses the
    #'        difference of BIC scores to add or remove edges. The Nandy et al.
    #'        rule uses a single calculation of a partial correlation in place
    #'        of the likelihood difference,
    #'        \item \code{precompute_covariances = TRUE} - For more than 5000
    #'        variables or so, set this to FALSE in order to calculate
    #'        covariances on the fly from data,
    #'        \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'        lambda to the diagonal, < 0 Pseudoinverse
    #'      }
    #'    \item \code{ebic} - Extended BIC score.
    #'    \itemize{
    #'      \item \code{gamma} - The gamma parameter in the EBIC score.
    #'      \item \code{precompute_covariances = TRUE} - For more than 5000
    #'      variables or so, set this to FALSE in order to calculate
    #'      covariances on the fly from data,
    #'      \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'      lambda to the diagonal, < 0 Pseudoinverse.
    #'    }
    #'    \item \code{bdeu} - Bayes Dirichlet Equivalent score with uniform priors.
    #'    \itemize{
    #'      \item \code{sample_prior = 10} - This sets the prior equivalent
    #'      sample size. This number is added to the sample size for each
    #'      conditional probability table in the model and is divided equally
    #'      among the cells in the table,
    #'      \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'        lambda to the diagonal, < 0 Pseudoinverse.
    #'    }
    #'    \item \code{basis_function_bic} - BIC score for basis-function models.
    #'      This is a generalization of the Degenerate Gaussian score.
    #'    \itemize{
    #'      \item \code{truncation_limit = 3} - Basis functions 1 though this
    #'      number will be used. The Degenerate Gaussian category indicator
    #'      variables for mixed data are also used,
    #'      \item \code{penalty_discount = 2} - Penalty discount. Higher penalty
    #'      yields sparser graphs,
    #'      \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'      lambda to the diagonal, < 0 Pseudoinverse,
    #'      \item \code{do_one_equation_only = FALSE} - If TRUE, only one
    #'      equation should be used when expanding the basis.
    #'    }
    #'    \item \code{conditional_gaussian} - Mixed discrete/continuous BIC score.
    #'    \itemize{
    #'      \item \code{penalty_discount = 1} - Penalty discount. Higher penalty
    #'      yields sparser graphs,
    #'      \item \code{discretize = TRUE} - If TRUE for the conditional
    #'       Gaussian likelihood, when scoring X --> D where X is continuous and
    #'       D discrete, one should to simply discretize X for just those cases.
    #'       If FALSE, the integration will be exact,
    #'      \item \code{num_categories_to_discretize = 3} -  In case the exact
    #'       algorithm is not used for discrete children and continuous parents
    #'       is not used, this parameter gives the number of categories to use
    #'       for this second (discretized) backup copy of the continuous
    #'       variables,
    #'      \item \code{structure_prior = 0} - The default number of parents
    #'        for any conditional probability table. Higher weight is accorded
    #'        to tables with about that number of parents. The prior structure
    #'        weights are distributed according to a binomial distribution.
    #'    }
    #'    \item \code{"degenerate_gaussian"} - Degenerate Gaussian BIC score.
    #'    \itemize{
    #'      \item \code{penalty_discount = 1} - Penalty discount. Higher penalty
    #'      yields sparser graphs,
    #'      \item \code{structure_prior = 0} - The default number of parents
    #'      for any conditional probability table. Higher weight is accorded
    #'      to tables with about that number of parents. The prior structure
    #'      weights are distributed according to a binomial distribution,
    #'      \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'      lambda to the diagonal, < 0 Pseudoinverse.
    #'      \item \code{precompute_covariances = TRUE} - For more than 5000
    #'      variables or so, set this to FALSE in order to calculate
    #'      covariances on the fly from data.
    #'    }
    #'    \item \code{"discrete_bic"} - BIC score for discrete data.
    #'    \itemize{
    #'      'item \code{penalty_discount = 2} - Penalty discount. Higher penalty
    #'      yields sparser graphs,
    #'      \item \code{structure_prior = 0} - The default number of parents
    #'      for any conditional probability table. Higher weight is accorded
    #'      to tables with about that number of parents. The prior structure
    #'      weights are distributed according to a binomial distribution.
    #'    }
    #'    \item \code{"gic"} - Generalized Information Criterion (GIC) score.
    #'    \itemize{
    #'      \item \code{penalty_discount = 1} - Penalty discount. Higher penalty
    #'      yields sparser graphs,
    #'      \item \code{sem_gic_rule = "bic"} - The following rules are available:
    #'      \code{"bic"} — \eqn{\ln n},
    #'      \code{"gic2"} — \eqn{p n^{1/3}},
    #'      \code{"ric"} — \eqn{2 \ln(p n)},
    #'      \code{"ricc"} — \eqn{2(\ln(p n) + \ln\ln(p n))},
    #'      \code{"gic6"} — \eqn{\ln n \ln(p n)}.
    #'      \item \code{precompute_covariances = TRUE} - For more than 5000
    #'      variables or so, set this to FALSE in order to calculate
    #'      covariances on the fly from data,
    #'      \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'      lambda to the diagonal, < 0 Pseudoinverse.
    #'    }
    #'    \item \code{"mag_degenerate_gaussian_bic"} - MAG Degenerate Gaussian BIC Score.
    #'    \itemize{
    #'      \item \code{penalty_discount = 1} - Penalty discount. Higher penalty
    #'      yields sparser graphs,
    #'      \item \code{structure_prior = 0} - The default number of parents
    #'      for any conditional probability table. Higher weight is accorded
    #'      to tables with about that number of parents. The prior structure
    #'      weights are distributed according to a binomial distribution,
    #'      \item \code{precompute_covariances = TRUE} - For more than 5000
    #'      variables or so, set this to FALSE in order to calculate
    #'      covariances on the fly from data.
    #'    }
    #'    \item \code{"mixed_variable_polynomial"} - Mixed variable polynomial BIC score.
    #'    \itemize{
    #'      \item \code{structure_prior = 0} - The default number of parents
    #'      for any conditional probability table. Higher weight is accorded
    #'      to tables with about that number of parents. The prior structure
    #'      weights are distributed according to a binomial distribution,
    #'      \item \code{f_degree = 0} - The f degree.
    #'      \item \code{discretize = FALSE} - If TRUE for the conditional
    #'      Gaussian likelihood, when scoring X --> D where X is continuous and
    #'      D discrete, one should to simply discretize X for just those cases.
    #'      If FALSE, the integration will be exact.
    #'    }
    #'    \item \code{"poisson_prior"} - Poisson prior score.
    #'    \itemize{
    #'      \item \code{poission_lambda = 2} - Lambda parameter for the Poisson
    #'      distribution (> 0),
    #'      \item \code{precompute_covariances = TRUE} - For more than 5000
    #'      variables or so, set this to FALSE in order to calculate
    #'      covariances on the fly from data,
    #'      \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'      lambda to the diagonal, < 0 Pseudoinverse.
    #'    }
    #'    \item \code{"zhang_shen_bound"} - Gaussian Extended BIC score.
    #'    \itemize{
    #'      \item \code{risk_bound = 0.2} - This is the probability of getting
    #'      the true model if a correct model is discovered. Could underfit.
    #'      \item \code{precompute_covariances = TRUE} - For more than 5000
    #'      variables or so, set this to FALSE in order to calculate
    #'      covariances on the fly from data,
    #'      \item \code{singularity_lambda = 0.0} - Small number >= 0: Add
    #'      lambda to the diagonal, < 0 Pseudoinverse.
    #'    }
    #'  }
    #'
    #' @return Invisibly returns \code{self}.
    set_score = function(method, ...) {
      method <- tolower(method)
      switch(
        method,
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
          stop(
            "Unknown score type using tetrad engine: ",
            method,
            call. = FALSE
          )
        }
      )
      invisible(self)
    },

    #' @description Sets the causal discovery algorithm to use in Tetrad.
    #' @param method (character) Name of the algorithm (e.g., "fges", "pc",
    #'  "fci", etc.).
    #' @param ... Additional parameters passed to the private algorithm-setting
    #' methods.
    #' For the following algorithms, the following parameters are available:
    #' \itemize{
    #'   \item \code{"boss"} - BOSS algorithm.
    #'    \itemize{
    #'      \item \code{num_starts = 1} - The number of times the algorithm
    #'      should be started from different initializations. By default, the
    #'       algorithm will be run through at least once using the initialized
    #'       parameters,
    #'      \item \code{use_bes = TRUE} - If TRUE, the algorithm uses the
    #'       backward equivalence search from the GES algorithm as one of its
    #'        steps,
    #'      \item \code{use_data_order = TRUE} - If TRUE, the data variable
    #'       order should be used for the first initial permutation,
    #'      \item \code{output_cpdag = TRUE} - If TRUE, the DAG output of the
    #'      algorithm is converted to a CPDAG.
    #'    }
    #'  \item \code{"boss_fci"} - BOSS-FCI algorithm.
    #'   \itemize{
    #'    \item \code{depth = -1} - Maximum size of conditioning set,
    #'     Set to -1 for unlimited,
    #'    \item \code{max_disc_path_length = -1} - Maximum length for any
    #'     discriminating path,
    #'     Set to -1 for unlimited,
    #'    \item \code{use_bes = TRUE} - If TRUE, the algorithm uses the
    #'     backward equivalence search from the GES algorithm as one of its
    #'     steps,
    #'    \item \code{use_heuristic} - If TRUE, use the max p heuristic
    #'     version,
    #'    \item \code{complete_rule_set_used = TRUE} -  FALSE if the (simpler)
    #'     final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'     completeness, should be used; TRUE if the (fuller) set due to
    #'     J. Zhang, should be used guaranteeing additional tail completeness,
    #'    \item \code{guarantee_pag = FALSE} - Ensure the output is a legal PAG
    #'     (where feasible).
    #'   }
    #'   \item \code{"boss_pod"} - BOSS-POD (BOSS-PAG of DAG) algorithm.
    #'    \itemize{
    #'     \item \code{use_bes = TRUE} - If TRUE, the algorithm uses the
    #'      backward equivalence search from the GES algorithm as one of its
    #'      steps,
    #'     \item \code{use_data_order = TRUE} - If TRUE, the data variable
    #'      order should be used for the first initial permutation,
    #'     \item \code{num_starts = 1} - The number of times the algorithm
    #'      should be started from different initializations. By default, the
    #'      algorithm will be run through at least once using the initialized
    #'      parameters,
    #'     \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'      final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'      completeness, should be used; TRUE if the (fuller) set due to
    #'      J. Zhang, should be used guaranteeing additional tail completeness.
    #'   }
    #'   \item \code{"ccd"} - Cyclic Causal Discovery.
    #'    \itemize{
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{apply_r1 = TRUE} - Set this parameter to FALSE if a
    #'       chain of directed edges pointing in the same direction, when only
    #'       the first few such orientations are justified based on the data.
    #'    }
    #'   \item \code{"cfci"} - Adjusts FCI to use conservative orientation as in
    #'    CPC.
    #'    \itemize{
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{max_disc_path_length = -1} - Maximum length for any
    #'      discriminating path,
    #'      \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'      final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'      completeness, should be used; TRUE if the (fuller) set due to
    #'      J. Zhang, should be used guaranteeing additional tail completeness.
    #'    }
    #'   \item \code{"cpc"} - Conservative PC algorithm.
    #'    \itemize{
    #'      \item \code{conflict_rule = 1} -
    #'      The value of \code{conflict_rule} determines how collider conflicts are handled. \code{1}
    #'      corresponds to the "overwrite" rule as introduced in the \pkg{pcalg} package, see
    #'      \code{\link[pcalg]{pc}}. \code{2} means that all collider conflicts using bidirected edges
    #'      should be prioritized, while \code{3} means that existing colliders should be prioritized,
    #'      ignoring subsequent conflicting information.
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{stable_fas = TRUE} - If TRUE, the "stable" version of
    #'       the PC adjacency search is used, which for k > 0 fixes the graph
    #'       for depth k + 1 to that of the previous depth k.
    #'      \item \code{guarantee_cpdag = FALSE} - If TRUE, ensure the output is
    #'       a legal CPDAG.
    #'    }
    #'   \item \code{"cstar"} - CStaR algorithm (Causal Stability Ranking).
    #'    \itemize{
    #'      \item \code{targets = ""} - Target names (comma or space separated),
    #'      \item \code{file_out_path = "cstar_out"} -  Path to a directory in
    #'       which results can be stored
    #'      \item \code{selection_min_effect = 0.0} - Minimum effect size for
    #'       listing effects in the CStaR table
    #'      \item \code{num_subsamples = 50} -  CStaR works by generating
    #'       subsamples and summarizing across them;
    #'       this specifies the number of subsamples to generate.
    #'       Must be >= 1,
    #'      \item \code{top_bracket = 10} - Top bracket to look for causes in,
    #'      \item \code{parallelized = FALSE} - If TRUE, the algorithm should
    #'       be parallelized,
    #'      \item \code{cpdag_algorithm = "restricted_boss"} - The CPDAG algorithm to use.
    #'      \code{"pc"} corresponds to PC Stable, \code{"fges"} selects the FGES algorithm,
    #'      \code{"boss"} selects the BOSS algorithm, and \code{"restricted_boss"} selects
    #'      the restricted BOSS variant.
    #'      \item \code{remove_effect_nodes = TRUE} - If TRUE, the effect nodes
    #'       should be removed from possible causes,
    #'      \item \code{sample_style = "subsample"} - The sampling style to use. Available options are
    #'      \code{"subsample"} and \code{"bootstrap"}.
    #'    }
    #'   \item \code{"dagma"} - DAGMA algorithm.
    #'    \itemize{
    #'      \item \code{lambda1 = 0.05} - Tuning parameter for DAGMA,
    #'      \item \code{w_threshold = 0.1} - Second tuning parameter for DAGMA,
    #'      \item \code{cpdag = TRUE} - The algorithm returns a DAG;
    #'      if this is set to TRUE, this DAG is converted to a CPDAG.
    #'    }
    #'   \item \code{"direct_lingam"} - DirectLiNGAM algorithm. No parameters.
    #'   \item \code{"fask"} - FASK algorithm.
    #'    \itemize{
    #'      \item \code{alpha = 0.05} - Significance level for the
    #'       independence test,
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{fask_delta = -0.3} - The bias for orienting with
    #'       negative coefficients (\code{0} means no bias) for \code{FASK v1},
    #'      \item \code{left_right_rule = 1} - The FASK left right rule v2 is
    #'       default, but two other (related) left-right rules are given for
    #'       relation to the literature, and the v1 FASK rule is included for
    #'       backward compatibility,
    #'      \item \code{skew_edge_threshold = 0.3} - For FASK, this includes an
    #'       adjacency X --- Y in the model if
    #'       |corr(X, Y | X > 0) - corr(X, Y | Y > 0)|
    #'       exceeds some threshold.
    #'    }
    #'   \item \code{"fci"} - FCI algorithm.
    #'    \itemize{
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{stable_fas = TRUE} - If TRUE, the "stable" version of
    #'       the PC adjacency search is used, which for k > 0 fixes the graph
    #'       for depth k + 1 to that of the previous depth k.
    #'      \item \code{max_disc_path_length = -1} - Maximum length for any
    #'       discriminating path,
    #'      \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'       final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'       completeness, should be used; TRUE if the (fuller) set due to
    #'       J. Zhang, should be used guaranteeing additional tail completeness.
    #'      \item \code{guarantee_pag = FALSE} - Ensure the output is a legal
    #'       PAG (where feasible).
    #'    }
    #'   \item \code{"fcit"} - FCI Targeted Testing (FCIT) algorithm
    #'    \itemize{
    #'      \item \code{use_bes = TRUE} - If TRUE, the algorithm uses the
    #'       backward equivalence search from the GES algorithm as one of its
    #'       steps,
    #'      \item \code{use_data_order = TRUE} - If TRUE, the data variable
    #'       order should be used for the first initial permutation,
    #'      \item \code{num_starts = 1} - The number of times the algorithm
    #'       should be started from different initializations. By default, the
    #'       algorithm will be run through at least once using the initialized
    #'       parameters,
    #'      \item \code{max_disc_path_length = -1} - Maximum length for any
    #'       discriminating path,
    #'      \item \code{start_with = "BOSS"} - What algorithm to run first to get
    #'       the initial CPDAG that the rest of the FCIT procedure refines. Available
    #'       options are: \code{"BOSS"}, \code{"GRaSP"}, and \code{"SP"}.
    #'      \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'       final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'       completeness, should be used; TRUE if the (fuller) set due to
    #'       J. Zhang, should be used guaranteeing additional tail completeness,
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{guarantee_pag = FALSE} - Ensure the output is a legal
    #'       PAG (where feasible).
    #'    }
    #'   \item \code{"fges"} - Fast Greedy Equivalence Search (FGES) algorithm.
    #'    \itemize{
    #'      \item \code{symmetric_first_step = FALSE} - If TRUE, scores for both
    #'       X --> Y and X <-- Y will be calculated and the higher score used.
    #'      \item \code{max_degree = -1} - Maximum degree of any node in the
    #'       graph. Set to -1 for unlimited,
    #'      \item \code{parallelized = FALSE} - If TRUE, the algorithm should
    #'       be parallelized,
    #'      \item \code{faithfulness_assumed = FALSE} - If TRUE, assume that if
    #'       \eqn{X \perp\!\!\!\perp Y} (by an independence test) then
    #'       \eqn{X \perp\!\!\!\perp Y} | Z for nonempty Z.
    #'    }
    #'   \item \code{"fges_mb"} - Fast Greedy Equivalence Search with Markov
    #'   Blanket (FGES-MB) algorithm.
    #'    \itemize{
    #'      \item \code{targets = ""} - Target names (comma or space separated),
    #'      \item \code{max_degree = -1} - Maximum degree of any node in the
    #'       graph. Set to -1 for unlimited,
    #'      \item \code{trimming_style = "mb_dags"} - The trimming style to use:
    #'       \code{"none"} applies no trimming. \code{"adj"} trims to the adjacencies
    #'       of the targets. \code{"mb_dags"} trims to Union(MB(targets)) U targets.
    #'       \code{"semidir_paths"} trims to nodes with semidirected paths to the targets.
    #'       \item \code{number_of_expansions = 2} - Number of expansions of the
    #'          algorithm away from the target,
    #'      \item \code{faithfulness_assumed = FALSE} - If TRUE, assume that if
    #'       \eqn{X \perp\!\!\!\perp Y} (by an independence test) then
    #'       \eqn{X \perp\!\!\!\perp Y} | Z for nonempty Z.
    #'    }
    #'   \item \code{"fofc"} - Find One Factor Clusters (FOFC)/MIMBUILD
    #'   algorithm.
    #'    \itemize{
    #'      \item \code{alpha = 0.001} - Cutoff for p values (alpha),
    #'      \item \code{penalty_discount = 2.0} - Penalty discount factor used
    #'       in BIC = 2L - ck log N, where c is the penalty. Higher c yields
    #'       sparser graphs,
    #'      \item \code{tetrad_test = "cca"} -  The tetrad test used. Available
    #'      options are: \code{"cca"} for CCA, \code{"bt"} for Bollen-Ting,
    #'      \code{"wishart"} for Wishart, and \code{"ark"} for Ark.
    #'      \item \code{include_structure_model = TRUE} - If \code{TRUE}
    #'       FOFC goes beyond the clustering step and calls a MIMBUILD routine
    #'       (PCA or Bollen, chosen via \code{mimbuild_type}) to learn the
    #'       causal relationships **between** the latent factors.
    #'       The returned graph then contains both the measurement model
    #'       (latent --> indicator edges) *and* the latent-level structure.
    #'       When \code{FALSE} FOFC stops after the clustering phase and returns
    #'       only the measurement model.
    #'      \item \code{precompute_covariances = TRUE} - For more than 5000
    #'        variables or so, set this to FALSE in order to calculate
    #'        covariances on the fly from data.
    #'    }
    #'   \item \code{"gfci"} - GFCI algorithm. Combines FGES and FCI.
    #'    \itemize{
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{max_degree = -1} - Maximum degree of any node in the
    #'       graph. Set to -1 for unlimited,
    #'      \item \code{max_disc_path_length = -1} - Maximum length for any
    #'       discriminating path,
    #'      \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'       final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'       completeness, should be used; TRUE if the (fuller) set due to
    #'       J. Zhang, should be used guaranteeing additional tail completeness.
    #'      \item \code{guarantee_pag = FALSE} - Ensure the output is a legal
    #'       PAG (where feasible).
    #'    }
    #'   \item \code{"grasp"} - GRaSP (Greedy Relations of Sparsest Permutation)
    #'   algorithm.
    #'    \itemize{
    #'      \item \code{covered_depth = 4} - The depth of recursion for first
    #'       search,
    #'      \item \code{singular_depth = 1} - Recursion depth for singular
    #'       tucks,
    #'      \item \code{nonsingular_depth = 1} - Recursion depth for nonsingular
    #'       tucks,
    #'      \item \code{ordered_alg = FALSE} - If TRUE, earlier GRaSP stages
    #'       should be performed before later stages,
    #'      \item \code{raskutti_uhler = FALSE} - If TRUE, use Raskutti and
    #'       Uhler's DAG-building method (test); if FALSE, use Grow-Shrink
    #'       (score).
    #'      \item \code{use_data_order = TRUE} - If TRUE, the data variable
    #'       order should be used for the first initial permutation,
    #'      \item \code{num_starts = 1} - The number of times the algorithm
    #'       should be started from different initializations. By default, the
    #'       algorithm will be run through at least once using the initialized
    #'       parameters.
    #'    }
    #'   \item \code{"grasp_fci"} - GRaSP-FCI algorithm. Combines GRaSP and FCI.
    #'    \itemize{
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{stable_fas = TRUE} - If TRUE, the "stable" version of
    #'       the PC adjacency search is used, which for k > 0 fixes the graph
    #'       for depth k + 1 to that of the previous depth k.
    #'      \item \code{max_disc_path_length = -1} - Maximum length for any
    #'       discriminating path,
    #'      \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'       final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'       completeness, should be used; TRUE if the (fuller) set due to
    #'       J. Zhang, should be used guaranteeing additional tail completeness,
    #'      \item \code{covered_depth = 4} - The depth of recursion for first
    #'       search,
    #'      \item \code{singular_depth = 1} - Recursion depth for singular
    #'       tucks,
    #'      \item \code{nonsingular_depth = 1} - Recursion depth for nonsingular
    #'       tucks,
    #'      \item \code{ordered_alg = FALSE} - If TRUE, earlier GRaSP stages
    #'       should be performed before later stages,
    #'      \item \code{raskutti_uhler = FALSE} - If TRUE, use Raskutti and
    #'       Uhler's DAG-building method (test); if FALSE, use Grow-Shrink
    #'       (score).
    #'      \item \code{use_data_order = TRUE} - If TRUE, the data variable
    #'       order should be used for the first initial permutation,
    #'      \item \code{num_starts = 1} - The number of times the algorithm
    #'       should be started from different initializations. By default, the
    #'       algorithm will be run through at least once using the initialized
    #'       parameters,
    #'      \item \code{guarantee_pag = FALSE} - If TRUE, ensure the output is a
    #'       legal PAG (where feasible).
    #'    }
    #'   \item \code{"ica_lingam"} - ICA LiNGAM algorithm.
    #'    \itemize{
    #'      \item \code{ica_a = 1.1} - The 'a' parameter of Fast ICA
    #'       (see Hyvarinen, A. (2001)). It ranges between 1 and 2.
    #'      \item \code{ica_max_iter = 5000} -  Maximum number if iterations of
    #'       the optimization procedure of ICA.
    #'      \item \code{ica_tolerance = 1e-8} - Fast ICA tolerance parameter.
    #'      \item \code{threshold_b = 0.1} - The estimated B matrix is
    #'       thresholded by setting small entries less than this threshold to
    #'       zero.
    #'    }
    #'   \item \code{"ica_lingd"} - ICA-LiNG-D algorithm
    #'    \itemize{
    #'      \item \code{ica_a = 1.1} - The 'a' parameter of Fast ICA
    #'       (see Hyvarinen, A. (2001)). It ranges between 1 and 2.
    #'      \item \code{ica_max_iter = 5000} -  Maximum number if iterations of
    #'       the optimization procedure of ICA.
    #'      \item \code{ica_tolerance = 1e-8} - Fast ICA tolerance parameter.
    #'      \item \code{threshold_b = 0.1} - The estimated B matrix is
    #'       thresholded by setting small entries less than this threshold to
    #'       zero.
    #'      \item \code{threshold_w} - The estimated W matrix is thresholded by
    #'       setting small entries less than this threshold to zero.
    #'    }
    #'
    #'   \item \code{"pc"} - Peter-Clark (PC) algorithm
    #'    \itemize{
    #'      \item \code{conflict_rule = 1} -
    #'      The value of \code{conflict_rule} determines how collider conflicts are handled. \code{1}
    #'      corresponds to the "overwrite" rule as introduced in the \pkg{pcalg} package, see
    #'      \code{\link[pcalg]{pc}}. \code{2} means that all collider conflicts using bidirected edges
    #'      should be prioritized, while \code{3} means that existing colliders should be prioritized,
    #'      ignoring subsequent conflicting information.
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{stable_fas = TRUE} - If TRUE, the "stable" version of
    #'       the PC adjacency search is used, which for k > 0 fixes the graph
    #'       for depth k + 1 to that of the previous depth k.
    #'      \item \code{guarantee_cpdag = FALSE} - If TRUE, ensure the output is
    #'       a legal CPDAG.
    #'    }
    #'   \item \code{"pc_max"} - PCMax algorithm
    #'    \itemize{
    #'      \item \code{conflict_rule = 1} -
    #'      The value of \code{conflict_rule} determines how collider conflicts are handled. \code{1}
    #'      corresponds to the "overwrite" rule as introduced in the \pkg{pcalg} package, see
    #'      \code{\link[pcalg]{pc}}. \code{2} means that all collider conflicts using bidirected edges
    #'      should be prioritized, while \code{3} means that existing colliders should be prioritized,
    #'      ignoring subsequent conflicting information.
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{use_heuristic = TRUE} - If TRUE, use the max p heuristic
    #'       version
    #'      \item \code{max_disc_path_length = -1} - The maximum path length to
    #'       use for the max p heuristic version. If -1, no limit is used.
    #'      \item \code{stable_fas = TRUE} - If TRUE, the "stable" version of
    #'       the PC adjacency search is used, which for k > 0 fixes the graph
    #'       for depth k + 1 to that of the previous depth k.
    #'    }
    #'   \item \code{"restricted_boss"} - Restricted BOSS algorithm
    #'    \itemize{
    #'      \item \code{targets = ""} - Target names (comma or space separated),
    #'      \item \code{use_bes = TRUE} - If TRUE, the algorithm uses the
    #'       backward equivalence search from the GES algorithm as one of its
    #'       steps,
    #'      \item \code{num_starts = 1} - The number of times the algorithm
    #'       should be started from different initializations. By default, the
    #'       algorithm will be run through at least once using the initialized
    #'       parameters,
    #'      \item \code{allow_internal_randomness = TRUE} -  If TRUE, the
    #'       algorithm allow the algorithm to use certain heuristic random
    #'       steps. This can improve performance, but may make the algorithm
    #'       non-deterministic.
    #'    }
    #'   \item \code{"rfci"} - Restricted FCI algorithm
    #'    \itemize{
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{stable_fas = TRUE} - If TRUE, the "stable" version of
    #'       the PC adjacency search is used, which for k > 0 fixes the graph
    #'       for depth k + 1 to that of the previous depth k.
    #'      \item \code{max_disc_path_length = -1} - Maximum length for any
    #'       discriminating path,
    #'      \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'       final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'       completeness, should be used; TRUE if the (fuller) set due to
    #'       J. Zhang, should be used guaranteeing additional tail completeness.
    #'      \item \code{guarantee_pag = FALSE} - Ensure the output is a legal
    #'       PAG (where feasible).
    #'    }
    #'   \item \code{"sp"} - Sparsest Permutation algorithm. No parameters.
    #'   \item \code{"sp_fci"} - Sparsest Permutation using FCI
    #'    \itemize{
    #'      \item \code{depth = -1} - Maximum size of conditioning set,
    #'      \item \code{max_disc_path_length = -1} - Maximum length for any
    #'       discriminating path,
    #'      \item \code{complete_rule_set_used = TRUE} - FALSE if the (simpler)
    #'       final orientation rules set due to P. Spirtes, guaranteeing arrow
    #'       completeness, should be used; TRUE if the (fuller) set due to
    #'       J. Zhang, should be used guaranteeing additional tail completeness,
    #'       \item \code{guarantee_pag = FALSE} - Ensure the output is a legal
    #'       PAG (where feasible).
    #'    }
    #'   \item \code{"svar_fci"} - SvarFCI algorithm (for timeseries data)
    #'    \itemize{
    #'      \item \code{penalty_discount = 2} - Penalty discount factor used
    #'       in BIC = 2L - ck log N, where c is the penalty. Higher c yields
    #'       sparser graphs.
    #'    }
    #'   \item \code{"svar_gfci"} - SvarGFCI algorithm. Similar to SvarFCI,
    #'   but uses a BIC score to search for a skeleton.
    #'    \itemize{
    #'      \item \code{penalty_discount = 2} - Penalty discount factor used
    #'       in BIC = 2L - ck log N, where c is the penalty. Higher c yields
    #'       sparser graphs.
    #'    }
    #' }
    #' @return Invisibly returns \code{self}.
    set_alg = function(method, ...) {
      method <- tolower(method)
      switch(
        method,
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
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
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
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
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
        "pc_max" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_pc_max_alg(...)
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
        "boss_fci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_boss_fci_alg(...)
        },
        "boss_pod" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          private$set_boss_pod_alg(...)
        },
        "fcit" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_fcit_alg(...)
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
        "sp_fci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.", call. = FALSE)
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.", call. = FALSE)
          }
          private$set_sp_fci_alg(...)
        },
        "ica_lingam" = {
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_ica_lingam_alg(...)
        },
        "ica_lingd" = {
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
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
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
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
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
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
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_direct_lingam_alg(...)
        },
        "dagma" = {
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_dagma_alg(...)
        },
        "svar_gfci" = {
          if (!rJava::.jcall(self$knowledge, "Z", "isEmpty")) {
            warning(
              "Background knowledge is set.",
              "This algorithm does not use background knowledge.",
              call. = FALSE
            )
          }
          private$set_svar_gfci_alg(...)
        },
        {
          stop(
            "Unknown method type using tetrad engine: ",
            method,
            call. = FALSE
          )
        }
      )
      invisible(self)
    },
    #' @description Sets the background knowledge object.
    #' @param knowledge_obj An object containing Tetrad knowledge (must implement \code{get_tetrad_knowledge}).
    set_knowledge = function(knowledge_obj) {
      is_knowledge(knowledge_obj)
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
        key <- rJava::.jfield("edu/cmu/tetrad/util/Params", "S", param_name)

        # Wrap the value based on its type.
        wrapped <- if (is.integer(value)) {
          rJava::.jcast(
            rJava::.jnew("java/lang/Integer", as.integer(value)),
            "java/lang/Object"
          )
        } else if (is.numeric(value)) {
          rJava::.jcast(
            rJava::.jnew("java/lang/Double", as.double(value)),
            "java/lang/Object"
          )
        } else if (is.logical(value)) {
          rJava::.jcast(
            rJava::.jnew("java/lang/Boolean", as.logical(value)),
            "java/lang/Object"
          )
        } else if (is.character(value)) {
          rJava::.jcast(
            rJava::.jnew("java/lang/String", value),
            "java/lang/Object"
          )
        } else {
          rJava::.jcast(value, "java/lang/Object") # must already be a Java object
        }

        # Set the parameter using the key and wrapped value.
        self$params$set(key, wrapped)
      }
      invisible(NULL)
    },
    #' @description Retrieves the argument names of a matching private function.
    #' @param fn_pattern (character) A pattern that should match a private method name.
    #' @param score If TRUE, retrieves parameters for a scoring function.
    #' @param test If TRUE, retrieves parameters for a test function.
    #' @param alg If TRUE, retrieves parameters for an algorithm.
    #' @return (character) The names of the parameters.
    get_parameters_for_function = function(
      fn_pattern,
      score = FALSE,
      test = FALSE,
      alg = FALSE
    ) {
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
          "Score is: ",
          score,
          ", test is: ",
          test,
          ", and alg is: ",
          alg,
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
      }

      is_private_method <- function(name) {
        is.function(get(name, envir = as.environment(private))) &&
          grepl(function_names, name)
      }
      private_names <- ls(envir = as.environment(private))
      matched_function <- base::Filter(is_private_method, private_names)
      if (length(matched_function) != 1) {
        if (length(matched_function > 1)) {
          # this is an extra precaution, which cannot be triggered currently,
          # but is nice to have for extra security.
          # nocov start
          error_message_suffix <- paste0(
            "\n  Matches: ",
            paste(matched_function, collapse = ", ")
          ) # nocov end
        } else {
          error_message_suffix <- ""
        }
        stop(
          paste0(
            "There is ",
            length(matched_function),
            " matches to the function pattern: ",
            fn_pattern,
            "\n  This is probably a misspecification of either a algorithm, test, or score.",
            "\n  There should be (only) a single match.",
            error_message_suffix
          ),
          call. = FALSE
        )
      }
      names(formals(matched_function, envir = as.environment(private)))
    },
    #' @description Runs the chosen Tetrad algorithm on the data.
    #' @param data (optional) If provided, overrides the previously set data.
    #' @param bootstrap (logical) If TRUE, bootstrapped graphs will be
    #' generated.
    #' @param int_cols_as_cont (logical) If `TRUE`, integer columns are treated
    #' as continuous, since Tetrad does not support ordinal data, but only
    #' either continuous or nominal data. Default is `TRUE.`
    #' @return A `caugi` and a `knowledge` (`knowledgeable_caugi`) object.
    #' Also populates \code{self$java}.
    run_search = function(
      data = NULL,
      bootstrap = FALSE,
      int_cols_as_cont = TRUE
    ) {
      stopifnot(
        is.logical(bootstrap),
        length(bootstrap) == 1
      )
      if (!is.null(data)) {
        self$set_data(data, int_cols_as_cont)
      }
      if (is.null(self$data)) {
        stop(
          "No data is set. Use set_data() first or input data directly into run_search().",
          call. = FALSE
        )
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

      # todo: make a better solution, probably in caugi

      # Always call with class = "PAG"?
      # Old code:
      # out <- tryCatch(self$result |> knowledgeable_caugi(),
      #   error = function(e) {
      #     self$result |> knowledgeable_caugi(class = "PAG")
      #   }
      # )
      # But gives incorrect edges for PC algorithm?
      self$result |> knowledgeable_caugi(class = "PAG")
    },
    #' @description Configures bootstrapping parameters for the Tetrad search.
    #' @param number_resampling (integer) Number of bootstrap samples.
    #' @param percent_resample_size (numeric) Percentage of sample size for each bootstrap.
    #' @param add_original (logical) If TRUE, add the original dataset to the bootstrap set.
    #' @param with_replacement (logical) If TRUE, sampling is done with replacement.
    #' @param resampling_ensemble (integer) How the resamples are used or aggregated.
    #' @param seed (integer) Random seed, or -1 for none.
    set_bootstrapping = function(
      number_resampling = 0,
      percent_resample_size = 100,
      add_original = TRUE,
      with_replacement = TRUE,
      resampling_ensemble = 1,
      seed = -1
    ) {
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
        FRACTION_RESAMPLE_SIZE = percent_resample_size,
        ADD_ORIGINAL_DATASET = add_original,
        RESAMPLING_WITH_REPLACEMENT = with_replacement,
        RESAMPLING_ENSEMBLE = resampling_ensemble,
        SEED = seed
      )
    },
    #' @description Sets or overrides the data used by Tetrad.
    #' @param data (data.frame) The new data to load.
    #' @param int_cols_as_cont (logical) If `TRUE`, integer columns are treated
    #' as continuous, since Tetrad does not support ordinal data, but only
    #' either continuous or nominal data. Default is `TRUE.`
    set_data = function(data, int_cols_as_cont = TRUE) {
      stopifnot(
        is.data.frame(data)
      )
      if (
        is.null(self$data) ||
          is.null(self$rdata) ||
          !isTRUE(all.equal(self$rdata, data))
      ) {
        self$rdata <- data
        self$data <- rdata_to_tetrad(data, int_cols_as_cont)
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
        length(time_lag) == 1,
        is.finite(time_lag),
        floor(time_lag) == time_lag,
        time_lag >= 0
      )
      self$set_params(
        TIME_LAG = time_lag
      )
    },
    #' @description Retrieves the current Java data object.
    #' @return (Java object) Tetrad dataset.
    get_data = function() {
      self$data
    },
    #' @description Returns the background knowledge object.
    #' @return (Java object) Tetrad Knowledge.
    get_knowledge = function() {
      self$knowledge
    },
    #' @description Gets the main Java result object (usually a graph) from the last search.
    #' @return (Java object) The Tetrad result graph or model.
    get_java = function() {
      self$java
    },
    #' @description Returns the string representation of a given Java object or \code{self$java}.
    #' @param java_obj (Java object, optional) If NULL, uses \code{self$java}.
    #' @return (character) The \code{toString()} of that Java object.
    get_string = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        rJava::.jcall(self$java, "S", "toString")
      } else {
        rJava::.jcall(java_obj, "S", "toString")
      }
    },
    #' @description Produces a DOT (Graphviz) representation of the graph.
    #' @param java_obj (Java object, optional) If NULL, uses \code{self$java}.
    #' @return (character) The DOT-format string.
    get_dot = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        self$java <- cast_obj(self$java)
        rJava::.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToDot",
          self$java
        )
      } else {
        java_obj <- cast_obj(java_obj)
        rJava::.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToDot",
          java_obj
        )
      }
    },
    #' @description Produces an amat representation of the graph.
    #' @param java_obj (Java object, optional) If NULL, uses \code{self$java}.
    #' @return (character) The adjacency matrix.
    get_amat = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        self$java <- cast_obj(self$java)
        rJava::.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToPcalg",
          self$java
        )
      } else {
        java_obj <- cast_obj(java_obj)
        rJava::.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToPcalg",
          java_obj
        )
      }
    }
  ),

  # Setting scores and tests is done through private functions,
  # and should be this should be done through set_score() and set_test().
  private = list(
    # Scores
    use_basis_function_bic_score = function(
      truncation_limit = 3,
      penalty_discount = 2,
      singularity_lambda = 0.0,
      do_one_equation_only = FALSE
    ) {
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
      self$score <- rJava::.jnew(
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
      self$score <- rJava::.jnew("edu/cmu/tetrad/algcomparison/score/BdeuScore")
      self$score <- cast_obj(self$score)
    },
    use_conditional_gaussian_score = function(
      penalty_discount = 1,
      discretize = TRUE,
      num_categories_to_discretize = 3,
      structure_prior = 0
    ) {
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
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/ConditionalGaussianBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_degenerate_gaussian_score = function(
      penalty_discount = 1,
      structure_prior = 0,
      singularity_lambda = 0.0,
      precompute_covariances = TRUE
    ) {
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
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/DegenerateGaussianBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_discrete_bic_score = function(
      penalty_discount = 2,
      structure_prior = 0
    ) {
      stopifnot(
        is.numeric(c(penalty_discount, structure_prior)),
        penalty_discount >= 0,
        structure_prior >= 0
      )
      self$set_params(
        PENALTY_DISCOUNT = penalty_discount,
        STRUCTURE_PRIOR = structure_prior
      )
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/DiscreteBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_ebic_score = function(
      gamma = 0.8,
      precompute_covariances = TRUE,
      singularity_lambda = 0.0
    ) {
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

      self$score <- rJava::.jnew("edu/cmu/tetrad/algcomparison/score/EbicScore")
      self$score <- cast_obj(self$score)
    },
    use_gic_score = function(
      penalty_discount = 1,
      sem_gic_rule = "bic",
      precompute_covariances = TRUE,
      singularity_lambda = 0.0
    ) {
      stopifnot(
        is.numeric(c(penalty_discount, singularity_lambda)),
        penalty_discount >= 0,
        singularity_lambda >= 0,
        is.character(sem_gic_rule)
      )
      sem_gic_rule_int <- switch(
        sem_gic_rule,
        "bic" = 1L,
        "gic2" = 2L,
        "ric" = 3L,
        "ricc" = 4L,
        "gic5" = 5L,
        "gic6" = 6L,
        stop(
          "Unsupported `sem_gic_rule` input:",
          sem_gic_rule,
          "\n",
          "Supported values are: 'bic', 'gic2', 'ric', 'ricc', 'gic5', and ",
          "'gic6'.",
          call. = FALSE
        )
      )
      self$set_params(
        SEM_GIC_RULE = sem_gic_rule_int,
        PENALTY_DISCOUNT_ZS = penalty_discount,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      self$score <- rJava::.jnew("edu/cmu/tetrad/algcomparison/score/GicScores")
      self$score <- cast_obj(self$score)
    },
    use_mag_degenerate_gaussian_bic_score = function(
      penalty_discount = 1,
      structure_prior = 0,
      precompute_covariances = TRUE
    ) {
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
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/MagDgBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_mixed_variable_polynomial_score = function(
      structure_prior = 0,
      f_degree = 0,
      discretize = FALSE
    ) {
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
        rJava::.jcast(
          rJava::.jnew("java/lang/Double", as.double(f_degree)),
          "java/lang/Object"
        )
      )
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/MVPBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_poisson_prior_score = function(
      poission_lambda = 2,
      precompute_covariances = TRUE,
      singularity_lambda = 0.0
    ) {
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
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/PoissonPriorScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_sem_bic_score = function(
      penalty_discount = 2,
      structure_prior = 0,
      sem_bic_rule = 1,
      precompute_covariances = TRUE,
      singularity_lambda = 0.0
    ) {
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
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/SemBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_zhang_shen_bound_score = function(
      risk_bound = 0.2,
      precompute_covariances = TRUE,
      singularity_lambda = 0.0
    ) {
      stopifnot(
        is.numeric(c(risk_bound, singularity_lambda)),
        risk_bound >= 0,
        singularity_lambda >= 0,
        is.logical(precompute_covariances),
        length(precompute_covariances) == 1
      )
      self$set_params(
        ZS_RISK_BOUND = risk_bound,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        SINGULARITY_LAMBDA = singularity_lambda
      )
      self$score <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/score/ZhangShenBoundScore"
      )
      self$score <- cast_obj(self$score)
    },
    # Tests
    use_basis_function_lrt_test = function(
      truncation_limit = 3,
      alpha = 0.01,
      singularity_lambda = 0.0,
      do_one_equation_only = FALSE,
      use_for_mc = FALSE
    ) {
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
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/BasisFunctionLrt"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/BasisFunctionLrt"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_fisher_z_test = function(
      alpha = 0.01,
      singularity_lambda = 0.0,
      use_for_mc = FALSE
    ) {
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
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/FisherZ"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/FisherZ"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_chi_square_test = function(
      min_count = 1,
      alpha = 0.01,
      cell_table_type = "ad",
      use_for_mc = FALSE
    ) {
      stopifnot(
        is.numeric(c(min_count, alpha)),
        min_count >= 0,
        alpha >= 0,
        floor(min_count) == min_count,
        is.character(cell_table_type),
        is.logical(use_for_mc)
      )
      cell_table_type_int <- switch(
        tolower(cell_table_type),
        ad = 1L,
        count = 2L,
        stop(
          "Unsupported `cell_table_type` input: ",
          cell_table_type,
          "\n",
          "Supported values are: 'ad' and 'count'.",
          call. = FALSE
        )
      )
      self$set_params(
        ALPHA = alpha,
        MIN_COUNT_PER_CELL = min_count,
        CELL_TABLE_TYPE = cell_table_type_int
      )
      if (use_for_mc) {
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/ChiSquare"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/ChiSquare"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_g_square_test = function(
      min_count = 1,
      alpha = 0.01,
      cell_table_type = "ad",
      use_for_mc = FALSE
    ) {
      stopifnot(
        is.numeric(c(min_count, alpha)),
        min_count >= 0,
        alpha >= 0,
        floor(min_count) == min_count,
        is.character(cell_table_type),
        is.logical(use_for_mc)
      )
      cell_table_type_int <- switch(
        tolower(cell_table_type),
        ad = 1L,
        count = 2L,
        stop(
          "Unsupported `cell_table_type` input: ",
          cell_table_type,
          "\n",
          "Supported values are: 'ad' and 'count'.",
          call. = FALSE
        )
      )
      self$set_params(
        ALPHA = alpha,
        MIN_COUNT_PER_CELL = min_count,
        CELL_TABLE_TYPE = cell_table_type_int
      )
      if (use_for_mc) {
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/GSquare"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/GSquare"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_conditional_gaussian_test = function(
      alpha = 0.01,
      discretize = TRUE,
      num_categories_to_discretize = 3,
      min_sample_size_per_cell = 4,
      use_for_mc = FALSE
    ) {
      stopifnot(
        is.numeric(c(
          alpha,
          num_categories_to_discretize,
          min_sample_size_per_cell
        )),
        alpha >= 0,
        num_categories_to_discretize >= 0,
        floor(num_categories_to_discretize) == num_categories_to_discretize,
        is.logical(discretize),
        length(discretize) == 1,
        length(min_sample_size_per_cell) == 1,
        floor(min_sample_size_per_cell) == min_sample_size_per_cell
      )
      self$set_params(
        ALPHA = alpha,
        DISCRETIZE = discretize,
        NUM_CATEGORIES_TO_DISCRETIZE = num_categories_to_discretize,
        MIN_SAMPLE_SIZE_PER_CELL = min_sample_size_per_cell
      )
      if (use_for_mc) {
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLrt"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLrt"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_degenerate_gaussian_test = function(
      alpha = 0.01,
      singularity_lambda = 0.0,
      use_for_mc = FALSE
    ) {
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
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/DegenerateGaussianLrt"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/DegenerateGaussianLrt"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_probabilistic_test = function(
      threshold = FALSE,
      cutoff = 0.5,
      prior_ess = 10,
      use_for_mc = FALSE
    ) {
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
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/ProbabilisticTest"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/ProbabilisticTest"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_kci_test = function(
      alpha = 0.01,
      approximate = TRUE,
      scaling_factor = 1,
      num_bootstraps = 5000,
      threshold = 1e-3,
      kernel_type = "gaussian",
      polyd = 5,
      polyc = 1,
      use_for_mc = FALSE
    ) {
      stopifnot(
        is.numeric(c(
          alpha,
          scaling_factor,
          num_bootstraps,
          threshold,
          polyd,
          polyc
        )),
        alpha >= 0,
        scaling_factor >= 0,
        num_bootstraps >= 0,
        floor(num_bootstraps) == num_bootstraps,
        threshold >= 0,
        is.logical(c(approximate, use_for_mc)),
        length(approximate) == 1,
        length(use_for_mc) == 1,
        kernel_type %in% c("gaussian", "linear", "polynomial"),
        floor(polyd) == polyd,
        floor(polyc) == polyc,
        polyd >= 1,
        polyc >= 0
      )

      kernel_type_int <- switch(
        kernel_type,
        gaussian = 1L,
        linear = 2L,
        polynomial = 3L,
        stop(
          "Unsupported `kernel_type` input: ",
          kernel_type,
          "\n",
          "Supported values are: 'gaussian', 'linear', and 'polynomial'.",
          call. = FALSE
        )
      )

      self$set_params(
        KCI_USE_APPROXIMATION = approximate,
        ALPHA = alpha,
        KERNEL_TYPE = kernel_type_int,
        SCALING_FACTOR = scaling_factor,
        KCI_NUM_BOOTSTRAPS = num_bootstraps,
        THRESHOLD_FOR_NUM_EIGENVALUES = threshold,
        POLYNOMIAL_DEGREE = polyd,
        POLYNOMIAL_CONSTANT = polyc
      )
      if (use_for_mc) {
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/Kci"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/Kci"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_cci_test = function(
      alpha = 0.01,
      scaling_factor = 2,
      basis_type = "legendre",
      basis_scale = 0.0,
      truncation_limit = 3,
      use_for_mc = FALSE
    ) {
      stopifnot(
        is.numeric(c(alpha, scaling_factor, truncation_limit)),
        alpha >= 0,
        scaling_factor >= 0,
        truncation_limit >= 0,
        floor(truncation_limit) == truncation_limit,
        is.logical(use_for_mc),
        length(use_for_mc) == 1,
        is.character(basis_type),
        basis_scale >= 0
      )
      basis_type_int <- switch(
        tolower(basis_type),
        polynomial = 1L,
        hermite1 = 2L,
        hermite2 = 3L,
        legendre = 4L,
        chebyshev = 5L,
        stop(
          "Unsupported `basis_type` input: ",
          basis_type,
          "\n",
          "Supported values are: 'polynomial', 'hermite1', 'hermite2', ",
          "'legendre', and 'chebyshev'.",
          call. = FALSE
        )
      )
      self$set_params(
        ALPHA = alpha,
        SCALING_FACTOR = scaling_factor,
        BASIS_TYPE = basis_type_int,
        BASIS_SCALE = basis_scale,
        TRUNCATION_LIMIT = truncation_limit
      )
      if (use_for_mc) {
        self$mc_test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/CciTest"
        )
        self$mc_test <- cast_obj(self$mc_test)
      } else {
        self$test <- rJava::.jnew(
          "edu/cmu/tetrad/algcomparison/independence/CciTest"
        )
        self$test <- cast_obj(self$test)
      }
    },
    # Algorithms
    set_fges_alg = function(
      symmetric_first_step = FALSE,
      max_degree = -1,
      parallelized = FALSE,
      faithfulness_assumed = FALSE
    ) {
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
      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Fges",
        self$score
      )

      # Set the knowledge object
      self$alg$setKnowledge(self$knowledge)
    },
    set_fges_mb_alg = function(
      targets = "",
      max_degree = -1,
      trimming_style = "mb_dags",
      number_of_expansions = 2,
      faithfulness_assumed = FALSE
    ) {
      stopifnot(
        is.character(targets),
        length(targets) == 1,
        is.numeric(max_degree),
        max_degree >= -1,
        is.character(trimming_style),
        is.numeric(number_of_expansions),
        floor(number_of_expansions) == number_of_expansions,
        number_of_expansions >= 0,
        is.logical(faithfulness_assumed),
        length(faithfulness_assumed) == 1
      )
      trimming_style_int <- switch(
        tolower(trimming_style),
        none = 1L,
        adj = 2L,
        mb_dags = 3L,
        semdir_paths = 4L,
        stop(
          "Unsupported `trimming_style` input: ",
          trimming_style,
          "\n",
          "Supported values are: 'none', 'adj', 'mb_dags' or 'semdir_paths'.",
          call. = FALSE
        )
      )
      self$set_params(
        TARGETS = targets,
        FAITHFULNESS_ASSUMED = faithfulness_assumed,
        MAX_DEGREE = max_degree,
        TRIMMING_STYLE = trimming_style_int,
        NUMBER_OF_EXPANSIONS = number_of_expansions
      )
      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/FgesMb",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_boss_alg = function(
      num_starts = 1,
      use_bes = TRUE,
      use_data_order = TRUE,
      output_cpdag = TRUE
    ) {
      stopifnot(
        is.numeric(num_starts),
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Boss",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_restricted_boss_alg = function(
      targets = "",
      use_bes = TRUE,
      num_starts = 1,
      allow_internal_randomness = TRUE
    ) {
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/RestrictedBoss",
        self$score
      )
    },
    set_cstar_alg = function(
      targets = "",
      file_out_path = "cstar-out",
      selection_min_effect = 0.0,
      num_subsamples = 50,
      top_bracket = 10,
      parallelized = FALSE,
      cpdag_algorithm = "restricted_boss",
      remove_effect_nodes = TRUE,
      sample_style = "subsample"
    ) {
      stopifnot(
        is.character(targets),
        is.character(sample_style),
        is.character(cpdag_algorithm),
        length(targets) == 1,
        is.numeric(c(selection_min_effect, num_subsamples, top_bracket)),
        selection_min_effect >= 0,
        num_subsamples >= 1,
        floor(num_subsamples) == num_subsamples,
        top_bracket >= 1,
        floor(top_bracket) == top_bracket,
        is.logical(c(parallelized, remove_effect_nodes)),
        length(parallelized) == 1,
        length(remove_effect_nodes) == 1
      )
      sample_style_int <- switch(
        tolower(sample_style),
        subsample = 1L,
        bootstrap = 2L,
        stop(
          "Unsupported `sample_style` input: ",
          sample_style,
          "\n",
          "Supported values are: 'subsample' or 'bootstrap'.",
          call. = FALSE
        )
      )
      cpdag_algorithm_int <- switch(
        tolower(cpdag_algorithm),
        pc = 1L,
        fges = 2L,
        boss = 3L,
        restricted_boss = 4L,
        stop(
          "Unsupported `cpdag_algorithm` input: ",
          cpdag_algorithm,
          "\n",
          "Supported values are: 'pc', 'fges', 'boss', or 'restricted_boss'.",
          call. = FALSE
        )
      )
      self$set_params(
        SELECTION_MIN_EFFECT = selection_min_effect,
        NUM_SUBSAMPLES = num_subsamples,
        TARGETS = targets,
        TOP_BRACKET = top_bracket,
        PARALLELIZED = parallelized,
        CSTAR_CPDAG_ALGORITHM = cpdag_algorithm_int,
        FILE_OUT_PATH = file_out_path,
        REMOVE_EFFECT_NODES = remove_effect_nodes,
        SAMPLE_STYLE = sample_style_int
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Cstar",
        self$test,
        self$score
      )
    },
    set_sp_alg = function() {
      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Sp",
        self$score
      )
    },
    set_grasp_alg = function(
      covered_depth = 4,
      singular_depth = 1,
      nonsingular_depth = 1,
      ordered_alg = FALSE,
      raskutti_uhler = FALSE,
      use_data_order = TRUE,
      num_starts = 1
    ) {
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Grasp",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_pc_alg = function(
      conflict_rule = 1,
      depth = -1,
      stable_fas = TRUE,
      guarantee_cpdag = FALSE
    ) {
      stopifnot(
        is.numeric(conflict_rule),
        length(conflict_rule) == 1,
        is.numeric(depth),
        length(depth) == 1,
        depth >= -1,
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Pc",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_cpc_alg = function(
      conflict_rule = 1,
      depth = -1,
      stable_fas = TRUE,
      guarantee_cpdag = FALSE
    ) {
      stopifnot(
        is.numeric(conflict_rule),
        length(conflict_rule) == 1,
        is.numeric(depth),
        depth >= -1,
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Cpc",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_pc_max_alg = function(
      conflict_rule = 1,
      depth = -1,
      use_heuristic = TRUE,
      max_disc_path_length = -1,
      stable_fas = TRUE
    ) {
      stopifnot(
        is.numeric(conflict_rule),
        length(conflict_rule) == 1,
        is.numeric(depth),
        length(depth) == 1,
        depth >= -1,
        is.logical(use_heuristic),
        length(use_heuristic) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
        is.logical(stable_fas),
        length(stable_fas) == 1
      )
      self$set_params(
        CONFLICT_RULE = conflict_rule,
        DEPTH = depth,
        USE_MAX_P_ORIENTATION_HEURISTIC = use_heuristic,
        MaX_PAX_P_ORIENTATION_HEURISTIC_MAX_LENGTH = max_disc_path_length,
        STABLE_FAS = stable_fas
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/PcMax",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_fci_alg = function(
      depth = -1,
      stable_fas = TRUE,
      max_disc_path_length = -1,
      complete_rule_set_used = TRUE,
      guarantee_pag = FALSE
    ) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        depth >= -1,
        is.logical(stable_fas),
        length(stable_fas) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Fci",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_rfci_alg = function(
      depth = -1,
      stable_fas = TRUE,
      max_disc_path_length = -1,
      complete_rule_set_used = TRUE
    ) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        depth >= -1,
        is.logical(stable_fas),
        length(stable_fas) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1
      )
      self$set_params(
        DEPTH = depth,
        STABLE_FAS = stable_fas,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Rfci",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_cfci_alg = function(
      depth = -1,
      max_disc_path_length = -1,
      complete_rule_set_used = TRUE
    ) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        depth >= -1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
        is.logical(complete_rule_set_used),
        length(complete_rule_set_used) == 1
      )
      self$set_params(
        DEPTH = depth,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Cfci",
        self$test
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_gfci_alg = function(
      depth = -1,
      max_degree = -1,
      max_disc_path_length = -1,
      complete_rule_set_used = TRUE,
      guarantee_pag = FALSE
    ) {
      stopifnot(
        is.numeric(depth),
        length(depth) == 1,
        depth >= -1,
        is.numeric(max_degree),
        length(max_degree) == 1,
        is.numeric(max_disc_path_length),
        length(max_disc_path_length) == 1,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Gfci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_boss_fci_alg = function(
      use_bes = TRUE,
      max_disc_path_length = -1,
      complete_rule_set_used = TRUE,
      depth = -1,
      num_threads = 0,
      guarantee_pag = FALSE,
      use_heuristic = FALSE,
      num_starts = 1
    ) {
      stopifnot(
        is.numeric(c(
          max_disc_path_length,
          depth,
          num_starts
        )),
        is.logical(c(
          use_bes,
          complete_rule_set_used,
          guarantee_pag,
          use_heuristic
        )),
        length(max_disc_path_length) == 1,
        length(depth) == 1,
        length(num_starts) == 1,
        length(guarantee_pag) == 1,
        floor(max_disc_path_length) == max_disc_path_length,
        floor(depth) == depth,
        floor(num_starts) == num_starts,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
        depth >= -1,
        num_starts >= 1
      )

      self$set_params(
        USE_BES = use_bes,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used,
        DEPTH = depth,
        GUARANTEE_PAG = guarantee_pag,
        USE_MAX_P_HEURISTIC = use_heuristic,
        NUM_STARTS = num_starts
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/BossFci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_boss_pod_alg = function(
      use_bes = TRUE,
      use_data_order = TRUE,
      num_starts = 1,
      complete_rule_set_used = TRUE
    ) {
      # Early exit if the Java class does not exist
      class_name <- "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/BossDot" # TODO: v7.6.9 renames this to BossPod

      stopifnot(
        is.numeric(c(num_starts)),
        is.logical(c(
          use_bes,
          complete_rule_set_used,
          use_data_order
        )),
        length(num_starts) == 1,
        floor(num_starts) == num_starts,
        num_starts >= 1
      )

      self$set_params(
        USE_BES = use_bes,
        USE_DATA_ORDER = use_data_order,
        COMPLETE_RULE_SET_USED = complete_rule_set_used,
        NUM_STARTS = num_starts
      )

      self$alg <- rJava::.jnew(class_name, self$score)
      self$alg$setKnowledge(self$knowledge)
    },
    set_fcit_alg = function(
      use_bes = TRUE,
      use_data_order = TRUE,
      num_starts = 1,
      max_disc_path_length = -1,
      start_with = "boss",
      complete_rule_set_used = TRUE,
      depth = -1,
      guarantee_pag = FALSE
    ) {
      stopifnot(
        is.numeric(c(num_starts, depth)),
        is.logical(c(
          use_bes,
          use_data_order,
          complete_rule_set_used,
          guarantee_pag
        )),
        floor(num_starts) == num_starts,
        floor(depth) == depth,
        length(depth) == 1,
        num_starts >= 1,
        depth >= -1,
        length(guarantee_pag) == 1,
        is.character(start_with)
      )
      start_with_int <- switch(
        tolower(start_with),
        boss = 1L,
        grasp = 2L,
        sp = 3L,
        stop(
          "Unsupported `start_with` input: ",
          start_with,
          "\n",
          "Supported values are: 'boss', 'grasp', or 'sp'.",
          call. = FALSE
        )
      )
      self$set_params(
        USE_BES = use_bes,
        USE_DATA_ORDER = use_data_order,
        NUM_STARTS = num_starts,
        FCIT_STARTS_WITH = start_with_int,
        MAX_DISCRIMINATING_PATH_LENGTH = max_disc_path_length,
        COMPLETE_RULE_SET_USED = complete_rule_set_used,
        DEPTH = depth,
        GUARANTEE_PAG = guarantee_pag
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Fcit",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_grasp_fci_alg = function(
      depth = -1,
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
      guarantee_pag = FALSE
    ) {
      stopifnot(
        is.numeric(c(
          depth,
          max_disc_path_length,
          covered_depth,
          singular_depth,
          nonsingular_depth
        )),
        floor(depth) == depth,
        floor(max_disc_path_length) == max_disc_path_length,
        floor(covered_depth) == covered_depth,
        floor(singular_depth) == singular_depth,
        floor(nonsingular_depth) == nonsingular_depth,
        is.logical(c(
          stable_fas,
          ordered_alg,
          raskutti_uhler,
          use_data_order,
          guarantee_pag
        )),
        length(stable_fas) == 1,
        length(ordered_alg) == 1,
        length(raskutti_uhler) == 1,
        length(use_data_order) == 1,
        length(guarantee_pag) == 1,
        length(max_disc_path_length) == 1,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
        is.numeric(num_starts),
        floor(num_starts) == num_starts,
        num_starts >= 1,
        depth >= -1
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/GraspFci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_sp_fci_alg = function(
      depth = -1,
      max_disc_path_length = -1,
      complete_rule_set_used = TRUE,
      guarantee_pag = FALSE
    ) {
      stopifnot(
        is.numeric(c(max_disc_path_length, depth)),
        length(max_disc_path_length) == 1,
        max_disc_path_length >= 0 || max_disc_path_length == -1,
        depth >= -1,
        length(depth) == 1,
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/SpFci",
        self$test,
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_ica_lingam_alg = function(
      ica_a = 1.1,
      ica_max_iter = 5000,
      ica_tolerance = 1e-8,
      threshold_b = 0.1
    ) {
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/IcaLingam"
      )
    },
    set_ica_lingd_alg = function(
      ica_a = 1.1,
      ica_max_iter = 5000,
      ica_tolerance = 1e-8,
      threshold_b = 0.1,
      threshold_w = 0.1
    ) {
      stopifnot(
        is.numeric(c(
          ica_a,
          ica_max_iter,
          ica_tolerance,
          threshold_b,
          threshold_w
        )),
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

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/IcaLingD"
      )
    },
    set_fask_alg = function(
      alpha = 0.05,
      depth = -1,
      fask_delta = -0.3,
      left_right_rule = 1,
      skew_edge_threshold = 0.3
    ) {
      stopifnot(
        is.numeric(c(alpha, depth, fask_delta, skew_edge_threshold)),
        alpha >= 0,
        depth >= -1,
        fask_delta >= -1,
        skew_edge_threshold >= 0,
        floor(depth) == depth,
        length(depth) == 1,
        floor(left_right_rule) == left_right_rule
      )
      self$set_params(
        ALPHA = alpha,
        DEPTH = depth,
        FASK_DELTA = fask_delta,
        FASK_LEFT_RIGHT_RULE = left_right_rule,
        SKEW_EDGE_THRESHOLD = skew_edge_threshold
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/Fask",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_fofc_alg = function(
      alpha = 0.001,
      penalty_discount = 2.0,
      tetrad_test = "cca",
      include_structure_model = TRUE,
      precompute_covariances = TRUE
    ) {
      stopifnot(
        is.numeric(c(alpha, penalty_discount)),
        alpha >= 0,
        penalty_discount >= 0,
        is.character(tetrad_test),
        is.logical(c(include_structure_model, precompute_covariances)),
        length(include_structure_model) == 1,
        length(precompute_covariances) == 1
      )
      tetrad_test_int <- switch(
        tolower(tetrad_test),
        cca = 1L,
        bt = 2L,
        wishart = 3L,
        ark = 4L,
        stop(
          "Unsupported `tetrad_test` input: ",
          tetrad_test,
          "\n",
          "Supported values are: 'cca', 'bt', 'wishart' or 'ark'.",
          call. = FALSE
        )
      )
      self$set_params(
        ALPHA = alpha,
        PENALTY_DISCOUNT = penalty_discount,
        TETRAD_TEST_FOFC = tetrad_test_int,
        INCLUDE_STRUCTURE_MODEL = include_structure_model,
        PRECOMPUTE_COVARIANCES = precompute_covariances
      )
      # TODO: v7.6.9 removes this folder. Figure out where new one is
      # See this commit https://github.com/cmu-phil/tetrad/commit/295dceef6b83ac08ff0032fb194cf3ee5e429337#diff-adf829223cc59eac11682310f8a77c0ec3cf26a5b4310d75ec8edfaa86dd285b
      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/cluster/Fofc"
      )
    },
    set_ccd_alg = function(depth = -1, apply_r1 = TRUE) {
      stopifnot(
        is.numeric(c(depth)),
        depth >= -1,
        is.logical(apply_r1),
        length(apply_r1) == 1
      )
      stopifnot(
        is.numeric(depth),
        depth >= -1,
        floor(depth) == depth,
        is.logical(apply_r1),
        length(apply_r1) == 1
      )
      self$set_params(
        DEPTH = depth,
        APPLY_R1 = apply_r1
      )

      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Ccd",
        self$test
      )
    },
    set_direct_lingam_alg = function() {
      self$alg <- rJava::.jnew(
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
      self$alg <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/Dagma"
      )
    },
    set_svar_fci_alg = function(penalty_discount = 2) {
      stopifnot(
        is.numeric(penalty_discount),
        penalty_discount >= 0,
        length(penalty_discount) == 1
      )
      if (is.null(self$data)) {
        stop("Data must be set before using `set_svar_fci_alg`.", call. = FALSE)
      }
      num_lags <- 2L
      # Create lagged data using Java method.
      lagged_data <- rJava::.jcall(
        "edu/cmu/tetrad/search/utils/TsUtils", # correct class
        "Ledu/cmu/tetrad/data/DataSet;", # return type
        "createLagData", # static method
        self$data, # DataSet
        as.integer(num_lags) # int
      )

      ts_test <- rJava::.jnew(
        "edu/cmu/tetrad/search/test/IndTestFisherZ",
        lagged_data,
        0.01
      )
      ts_test <- rJava::.jcast(
        ts_test,
        "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper"
      )
      svar_fci <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/SvarFci",
        ts_test
      )
      svar_fci$setKnowledge(lagged_data$getKnowledge())
      self$alg <- svar_fci
    },
    set_svar_gfci_alg = function(penalty_discount = 2) {
      stopifnot(
        is.numeric(penalty_discount),
        penalty_discount >= 0,
        length(penalty_discount) == 1
      )

      if (is.null(self$data)) {
        stop(
          "Data must be set before using `set_svar_gfci_alg`.",
          call. = FALSE
        )
      }
      num_lags <- 2L
      lagged_data <- rJava::.jcall(
        "edu/cmu/tetrad/search/utils/TsUtils", # correct class
        "Ledu/cmu/tetrad/data/DataSet;", # return type
        "createLagData", # static method
        self$data, # DataSet
        as.integer(num_lags) # int
      )

      ts_test <- rJava::.jnew(
        "edu/cmu/tetrad/search/test/IndTestFisherZ",
        lagged_data,
        0.01
      )
      ts_test <- rJava::.jcast(
        ts_test,
        "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper"
      )
      ts_score <- rJava::.jnew(
        "edu/cmu/tetrad/search/score/SemBicScore",
        lagged_data,
        TRUE
      )
      ts_score <- rJava::.jcast(
        ts_score,
        "edu/cmu/tetrad/algcomparison/score/ScoreWrapper"
      )
      self$set_params(
        PENALTY_DISCOUNT = penalty_discount
      )
      svar_gfci <- rJava::.jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/SvarGfci",
        ts_test,
        ts_score
      )
      svar_gfci$setKnowledge(lagged_data$getKnowledge())
      self$alg <- svar_gfci
    }
  )
)
