#' @importFrom R6 R6Class
#' @import bnlearn
#' @export bnlearnSearch
bnlearnSearch <- R6Class(
  "bnlearnSearch",
  public = list(
    data = NULL,
    rdata = NULL,
    score = NULL,
    test = NULL,
    alg = NULL,
    params = NULL,
    knowledge = NULL,
    initialize = function() {
      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$knowledge <- NULL
      self$params <- list()
    },
    set_params = function(params) {
      self$params <- params
    },
    set_data = function(data) {
      self$data <- data
    },
    set_test = function(method,
                        alpha = 0.05) {
      stopifnot(
        is.numeric(alpha),
        length(alpha) == 1,
        alpha > 0,
        alpha < 1
      )

      method <- tolower(method)

      allowed_tests <- c(
        # Discrete with categorical variables
        "mi", # mutual information
        "mi-adf", # with adjusted degrees of freedom
        "mc-mi", # Monte Carlo mutual information
        "smc-mi", # sequential Monte Carlo mutual information
        "sp-mi", # semi-parametric mutual information
        "mi-sh", # mutual information shrinkage estimator
        "x2", # chi-squared test
        "x2-adf", # chi-squared test with adjusted degrees of freedom
        "mc-x2", # Monte Carlo chi-squared test
        "smc-x2", # sequential Monte Carlo chi-squared test
        "sp-x2", # semi-parametric chi-squared test

        # Discrete with ordered factors
        "jt", # Jonckheere-Terpstra test
        "mc-jt", # Monte Carlo Jonckheere-Terpstra test
        "smc-jt", # sequential Monte Carlo Jonckheere-Terpstra test

        # Gaussian variables
        "cor", # pearson correlation
        "mc-cor", # Monte Carlo pearson correlation
        "smc-cor", # sequential Monte Carlo pearson correlation
        "zf", # fisher Z test
        "mc-zf", # Monte Carlo fisher Z test
        "smc-zf", # sequential Monte Carlo fisher Z test
        "mi-g", # mutual information for Gaussian variables
        "mc-mi-g", # Monte Carlo mutual information for Gaussian variables
        "smc-mi-g", # sequential Monte Carlo mutual inf. for Gaussian variables
        "mi-g-sh", # mutual information for Gaussian variables with shrinkage

        # Conditional Gaussian
        "mi-cg" # mutual information for conditional Gaussian variables
      )

      if (!(method %in% allowed_tests)) {
        stop("Unknown test type using bnlearn engine: ", method,
          call. = FALSE
        )
      }

      self$params$alpha <- alpha
      self$test <- method
      invisible(self)
    },
    set_score = function(method) {
      method <- tolower(method)

      allowed_scores <- c(
        # Discrete with categorical variables
        "loglik", # log-likelihood
        "aic", # Akaike Information Criterion
        "bic", # Bayesian Information Criterion
        "ebic", # Extended Bayesian Information Criterion
        "pred-loglik", # predictive log-likelihood
        "bde", # Bayesian Dirichlet equivalent (uniform)
        "bds", # Bayesian Dirichlet score
        "mbde", # modified Bayesian Dirichlet equivalent
        "bdla", # locally averaged Bayesian Dirichlet
        "k2", # K2 score
        "fnml", # factorized normalized maximum likelihood score
        "qnml", # quotient normalized maximum likelihood score
        "nal", # node-average (log-)likelihood
        "pnal", # penalized node-average (log-)likelihood

        # Gaussian variables
        "loglik-g", # log-likelihood for Gaussian variables
        "aic-g", # Akaike Information Criterion for Gaussian variables
        "bic-g", # Bayesian Information Criterion for Gaussian vars
        "ebic-g", # Extended Bayesian Information Criterion for Gaussian
        "pred-loglik-g", # predictive log-likelihood for Gaussian variables
        "bge", # Gaussian posterior density
        "nal-g", # node-average (log-)likelihood for Gaussian variables
        "pnal-g", # penalized node-average (log-)likelihood for Gaussian

        # Conditional Gaussian
        "loglik-cg", # log-likelihood for cg variables
        "aic-cg", # Akaike Information Criterion for cg variables
        "bic-cg", # Bayesian Information Criterion for cg variables
        "ebic-cg", # Extended Bayesian Information Criterion cg variables
        "pred-loglik-cg", # predictive log-likelihood for cg variables
        "nal-cg", # node-average (log-)likelihood for cg variables
        "pnal-cg" # penalized node-average (log-)likelihood for cg vars
      )
      if (!(method %in% allowed_scores)) {
        stop("Unknown score type using bnlearn engine: ", method,
          call. = FALSE
        )
      }

      self$score <- method
      invisible(self)
    },
    set_alg = function(method) {
      method <- tolower(method)

      need_test <- c(
        "pc.stable", "gs", "iamb", "fast.iamb", "inter.iamb", "iamb.fdr",
        "mmpc", "si.hiton.pc", "hpc"
      )
      need_score <- c("hc", "tabu")
      need_both <- c("mmhc", "rsmax2", "h2pc")
      need_restrict_maximize <- c("rsmax2")
      # guard clauses
      if (method %in% need_test && is.null(self$test)) {
        stop("No test is set. Use set_test() first.", call. = FALSE)
      }

      if (method %in% need_score && is.null(self$score)) {
        stop("No score is set. Use set_score() first.", call. = FALSE)
      }

      if (method %in% need_both) {
        if (is.null(self$test) || is.null(self$score)) {
          stop("Both test and score must be set for this algorithm.", call. = FALSE)
        }
        if (method %in% need_restrict_maximize ||
          (is.null(self$maximize_alg) || is.null(self$restrict_alg))) {
          stop("Both maximize and restrict algorithms must be set for this algorithm.", call. = FALSE)
        }
      }

      self$alg <- switch(method,

        # constraint-based
        "pc.stable" = purrr::partial(bnlearn::pc.stable,
          test = self$test,
          !!!self$params
        ),
        "gs" = purrr::partial(bnlearn::gs,
          test = self$test,
          !!!self$params
        ),
        "iamb" = purrr::partial(bnlearn::iamb,
          test = self$test,
          !!!self$params
        ),
        "fast.iamb" = purrr::partial(bnlearn::fast.iamb,
          test = self$test,
          !!!self$params
        ),
        "inter.iamb" = purrr::partial(bnlearn::inter.iamb,
          test = self$test,
          !!!self$params
        ),
        "iamb.fdr" = purrr::partial(bnlearn::iamb.fdr,
          test = self$test,
          !!!self$params
        ),

        # local / skeleton discovery
        "mmpc" = purrr::partial(bnlearn::mmpc,
          test = self$test,
          !!!self$params
        ),
        "si.hiton.pc" = purrr::partial(bnlearn::si.hiton.pc,
          test = self$test,
          !!!self$params
        ),
        "hpc" = purrr::partial(bnlearn::hpc,
          test = self$test,
          !!!self$params
        ),

        # score-based
        "hc" = purrr::partial(bnlearn::hc,
          score = self$score,
          !!!self$params
        ),
        "tabu" = purrr::partial(bnlearn::tabu,
          score = self$score,
          !!!self$params
        ),

        # hybrid
        "mmhc" = purrr::partial(
          bnlearn::mmhc,
          !!!self$params
        ),
        "rsmax2" = purrr::partial(bnlearn::rsmax2,
          test = self$test,
          score = self$score,
          !!!self$params
        ),
        "h2pc" = purrr::partial(
          bnlearn::h2pc,
          !!!self$params
        ),

        # pairwise mutual-information learners
        "chow.liu" = purrr::partial(
          bnlearn::chow.liu,
          !!!self$params
        ),
        "aracne" = purrr::partial(
          bnlearn::aracne,
          !!!self$params
        ),
        stop("Unknown method type using bnlearn engine: ", method, call. = FALSE)
      )

      invisible(self)
    },
    set_knowledge = function(knowledge_obj, directed_as_undirected = FALSE) {
      check_knowledge_obj(knowledge_obj)
    },
  ),
  private = list()
)
