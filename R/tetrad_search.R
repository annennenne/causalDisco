library(R6)
library(rJava)

# Source tetrad <-> R data conversion functions.
# source("/home/fabben/BioStat/causalDisco/tetrad/tetrad_rdata.R")

# Source R <-> Java helper functions.
# source("/home/fabben/BioStat/causalDisco/tetrad/java_r_functions.R")

# Initialize the JVM if it isn’t already running.
# todo : how many gb?
if (!.jniInitialized) {
  .jinit(
    parameters = "-Xmx2g",
    classpath = "/tetrad/tetrad-current.jar"
  )
}

#' Define the TetradSearch R6 class.
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
    knowledge = NULL,
    params = NULL,
    bootstrap_graphs = NULL,
    mc_ind_results = NULL,
    bhat = NULL,
    unstable_bhats = NULL,
    stable_bhats = NULL,
    initialize = function() {
      self$data <- NULL
      self$score <- NULL
      self$test <- NULL
      self$mc_test <- NULL
      self$knowledge <- .jnew("edu/cmu/tetrad/data/Knowledge")
      self$params <- .jnew("edu/cmu/tetrad/util/Parameters")
      self$bootstrap_graphs <- NULL
    },
    set_test = function(method, ..., mc = FALSE) {
      method <- tolower(method)
      switch(method,
        "chi_square" = {
          private$use_chi_square(..., use_for_mc = mc)
        },
        "fisher_z" = {
          private$use_fisher_z(..., use_for_mc = mc)
        },
        "cci" = {
          private$use_cci(..., use_for_mc = mc)
        },
        "basis_function_lrt" = {
          private$use_basis_function_lrt(..., use_for_mc = mc)
        },
        "basis_function_lrt_fs" = {
          private$use_basis_function_lrt_fs(..., use_for_mc = mc)
        },
        "conditional_gaussian" = {
          private$use_conditional_gaussian_test(..., use_for_mc = mc)
        },
        "degenerate_gaussian" = {
          private$use_degenerate_gaussian_test(..., use_for_mc = mc)
        },
        "g_square" = {
          private$use_g_square(..., use_for_mc = mc)
        },
        "kci" = {
          private$use_kci(..., use_for_mc = mc)
        },
        "probabilistic" = {
          private$use_probabilistic_test(..., use_for_mc = mc)
        },
        {
          stop("Unknown test type: ", method)
        }
      )
      invisible(self)
    },
    set_score = function(method, ...) {
      method <- tolower(method)
      switch(method,
        "sem_bic" = {
          private$use_sem_bic(...)
        },
        "ebic" = {
          private$use_ebic(...)
        },
        "bdeu" = {
          private$use_bdeu(...)
        },
        "basis_function_bic" = {
          private$use_basis_function_bic(...)
        },
        "basis_function_bic_fs" = {
          private$use_basis_function_bic_fs(...)
        },
        "conditional_gaussian" = {
          private$use_conditional_gaussian_score(...)
        },
        "degenerate_gaussian" = {
          private$use_degenerate_gaussian_score(...)
        },
        "gic" = {
          private$use_gic_score(...)
        },
        "mixed_variable_polynomial" = {
          private$use_mixed_variable_polynomial(...)
        },
        "poisson_prior" = {
          private$use_poisson_prior_score(...)
        },
        "zhang_shen_bound" = {
          private$use_zhang_shen_bound(...)
        },
        {
          stop("Unknown score type: ", method)
        }
      )
      invisible(self)
    },
    set_alg = function(method, ...) {
      method <- tolower(method)
      switch(method,
        "fges" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          private$set_fges(...)
        },
        "fges_mb" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          private$set_fges_mb(...)
        },
        "boss" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          private$set_boss(...)
        },
        "restricted_boss" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_restricted_boss(...)
        },
        "cstar" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_cstar(...)
        },
        "sp" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          private$set_sp(...)
        },
        "grasp" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_grasp(...)
        },
        "pc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_pc(...)
        },
        "cpc" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_cpc(...)
        },
        "pcmax" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_pcmax(...)
        },
        "fci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_fci(...)
        },
        "rfci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_rfci(...)
        },
        "cfci" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_cfci(...)
        },
        "gfci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_gfci(...)
        },
        "bfci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_bfci(...)
        },
        "lv_lite" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_lv_lite(...)
        },
        "grasp_fci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_grasp_fci(...)
        },
        "spfci" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          private$set_spfci(...)
        },
        "ica_lingam" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_ica_lingam(...)
        },
        "ica_lingd" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_ica_lingd(...)
        },
        "fask" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          private$set_fask(...)
        },
        "fofc" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_fofc(...)
        },
        "ccd" = {
          if (is.null(self$test)) {
            stop("No test is set. Use set_test() first.")
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_ccd(...)
        },
        "svar_fci" = {
          private$set_svar_fci(...)
        },
        "direct_lingam" = {
          if (is.null(self$score)) {
            stop("No score is set. Use set_score() first.")
          }
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_direct_lingam(...)
        },
        "dagma" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_dagma(...)
        },
        "pc_lingam" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_pc_lingam(...)
        },
        "svar_gfci" = {
          if (!is.null(self$knowledge)) {
            warning("Background knowledge is set.
                    This algorithm does not use background knowledge.")
          }
          private$set_svar_gfci(...)
        },
        {
          stop("Unknown method type using tetrad engine: ", method)
        }
      )
      invisible(self)
    },
    # Knowledge management methods.
    add_to_tier = function(tier, var_name) {
      .jcall(
        self$knowledge,
        "V",
        "addToTier",
        .jnew("java/lang/Integer", tier),
        .jnew("java/lang/String", var_name)
      )
    },
    set_tier_forbidden_within = function(tier, forbiddenWithin = TRUE) {
      .jcall(
        self$knowledge,
        "V",
        "setTierForbiddenWithin",
        .jnew("java/lang/Integer", tier),
        forbiddenWithin
      )
    },
    set_forbidden = function(var_name_1, var_name_2) {
      .jcall(
        self$knowledge,
        "V",
        "setForbidden",
        .jnew("java/lang/String", var_name_1),
        .jnew("java/lang/String", var_name_2)
      )
    },
    set_required = function(var_name_1, var_name_2) {
      .jcall(
        self$knowledge,
        "V",
        "setRequired",
        .jnew("java/lang/String", var_name_1),
        .jnew("java/lang/String", var_name_2)
      )
    },
    set_knowledge = function(knowledge) {
      if (!is.null(self$alg)) {
        self.alg$setKnowledge(knowledge)
      }
      self$knowledge <- knowledge
    },
    clear_knowledge = function() {
      .jcall(self$knowledge, "V", "clear")
    },
    load_knowledge = function(path) {
      know_file <- .jnew("java/io/File", path)
      know_delim <- .jfield(
        "edu/cmu/tetrad/data/DelimiterType",
        "S",
        "WHITESPACE"
      )
      self$knowledge <- .jcall(
        "edu/cmu/tetrad/data/SimpleDataLoader",
        "Ledu/cmu/tetrad/data/Knowledge;",
        "loadKnowledge",
        know_file,
        know_delim,
        "//"
      )
    },
    check_knowledge = function() {
      xList <- .jcall(self$knowledge, "Ljava/util/List;", "getVariables")
      yList <- .jcall(self$data, "Ljava/util/List;", "getVariableNames")
      X <- sapply(
        seq_len(.jcall(xList, "I", "size")),
        function(i) .jcall(xList, "Ljava/lang/Object;", "get", i - 1)$toString()
      )
      Y <- sapply(
        seq_len(.jcall(yList, "I", "size")),
        function(i) .jcall(yList, "Ljava/lang/Object;", "get", i - 1)$toString()
      )
      setdiff(X, Y)
    },
    print_knowledge = function() {
      print(self$knowledge$toString())
    },
    run_search = function(data = NULL, bootstrap = FALSE, bhat = FALSE, unstable_bhat = FALSE, stable_bhat = FALSE) {
      if (!is.null(data)) {
        self$set_data(data)
      }
      if (is.null(self$data)) {
        stop("No data is set. Use set_data() first or input data directly into run_search().")
      }
      if (is.null(self$alg)) {
        stop("No algorithm is set. Use set_alg() first.")
      }
      self$java <- self$alg$search(self$data, self$params)
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
    },
    run_gango = function(score, data) {
      # This is a static-like method.
      fges_graph <- tetrad_search$public_methods$run_fges(score)
      datasets <- .jnew("java/util/ArrayList")
      datasets$add(data)
      rskew <- .jnew("edu/cmu/tetrad/search/Lofs2", fges_graph, datasets)
      rskew$setKnowledge(self$knowledge)
      rskew$setRule(.jfield("edu/cmu/tetrad/search/Lofs2$Rule", "S", "RSkew"))
      gango_graph <- rskew$orient()
      return(gango_graph)
    },
    set_bootstrapping = function(number_resampling = 0,
                                 percent_resample_size = 100,
                                 add_original = TRUE,
                                 with_replacement = TRUE,
                                 resampling_ensemble = 1,
                                 seed = -1) {
      set_params(
        self$params,
        NUMBER_RESAMPLING = number_resampling,
        PERCENT_RESAMPLE_SIZE = percent_resample_size,
        ADD_ORIGINAL_DATASET = add_original,
        RESAMPLING_WITH_REPLACEMENT = with_replacement,
        RESAMPLING_ENSEMBLE = resampling_ensemble,
        SEED = seed
      )
    },
    set_data = function(data) {
      # Only load the data if we haven't already
      # or if the data is different from already loaded data.
      if (is.null(self$data) || is.null(self$rdata) ||
        !isTRUE(all.equal(self$rdata, data))) {
        self$rdata <- data
        self$data <- rdata_to_tetrad(data)
      }
    },
    set_verbose = function(verbose) {
      .jcall(
        self$params,
        "V",
        "set",
        .jfield("edu/cmu/tetrad/util/Params", "S", "VERBOSE"),
        verbose
      )
    },
    set_time_lag = function(time_lag = 0) {
      .jcall(
        self$params,
        "V",
        "set",
        .jfield("edu/cmu/tetrad/util/Params", "S", "TIME_LAG"),
        time_lag
      )
    },
    get_data = function() {
      return(self$data)
    },
    get_verbose = function() {
      return(.jcall(
        self$params,
        "Z",
        "getBoolean",
        .jfield("edu/cmu/tetrad/util/Params", "S", "VERBOSE")
      ))
    },
    get_knowledge = function() {
      return(self$knowledge)
    },
    get_java = function() {
      return(self$java)
    },
    get_string = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        return(.jcall(self$java, "S", "toString"))
      } else {
        return(.jcall(java_obj, "S", "toString"))
      }
    },
    get_dag_string = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        dag <- .jcall(
          "edu/cmu/tetrad/graph/GraphTransforms",
          "Ljava/lang/Object;",
          "dagFromCpdag",
          self$java
        )
        return(.jcall(dag, "S", "toString"))
      } else {
        dag <- .jcall(
          "edu/cmu/tetrad/graph/GraphTransforms",
          "Ljava/lang/Object;",
          "dagFromCpdag",
          java_obj
        )
        return(.jcall(dag, "S", "toString"))
      }
    },
    get_dag_java = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphTransforms",
          "Ljava/lang/Object;",
          "dagFromCpdag",
          self$java
        ))
      } else {
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphTransforms",
          "Ljava/lang/Object;",
          "dagFromCpdag",
          java_obj
        ))
      }
    },
    get_causal_learn = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        return(.jcall(
          "pytetrad/tools/translate",
          "Ljava/lang/Object;",
          "tetrad_graph_to_causal_learn",
          self$java
        ))
      } else {
        return(.jcall(
          "pytetrad/tools/translate",
          "Ljava/lang/Object;",
          "tetrad_graph_to_causal_learn",
          java_obj
        ))
      }
    },
    get_graph_to_matrix = function(java_obj = NULL,
                                   null_ept = 0,
                                   circle_ept = 1,
                                   arrow_ept = 2,
                                   tail_ept = 3) {
      if (is.null(java_obj)) {
        java_obj <- self$java
      }
      graph_to_matrix(
        g = java_obj,
        null_ept = null_ept,
        circle_ept = circle_ept,
        arrow_ept = arrow_ept,
        tail_ept = tail_ept
      )
    },
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
    get_xml = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToXml",
          self$java
        ))
      } else {
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToXml",
          java_obj
        ))
      }
    },
    get_lavaan = function(java_obj = NULL) {
      if (is.null(java_obj)) {
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToLavaan",
          cast_obj(self$java)
        ))
      } else {
        return(.jcall(
          "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
          "S",
          "graphToLavaan",
          cast_obj(java_obj)
        ))
      }
    },
    bootstrap_graph = function(index) {
      size <- .jcall(self$bootstrap_graphs, "I", "size")
      if (index < 0 || index >= size) {
        stop("index out of bounds (0-indexed)")
      }
      return(.jcall(
        self$bootstrap_graphs,
        "Ljava/lang/Object;",
        "get",
        as.integer(index)
      ))
    },
    bootstrap_dot = function(index) {
      size <- .jcall(self$bootstrap_graphs, "I", "size")
      if (index < 0 || index >= size) {
        stop("index out of bounds")
      }
      java_obj <- .jcall(
        self$bootstrap_graphs,
        "Ljava/lang/Object;",
        "get",
        as.integer(index)
      )
      return(.jcall(
        "edu/cmu/tetrad/graph/GraphSaveLoadUtils",
        "S",
        "graphToDot",
        java_obj
      ))
    },
    is_legal_pag = function(graph) {
      return(.jcall(
        "edu/cmu/tetrad/search/utils/GraphSearchUtils",
        "Z",
        "isLegalPag",
        graph
      ))
    },
    is_legal_pag_reason = function(graph) {
      cat(
        .jcall(
          "edu/cmu/tetrad/search/utils/GraphSearchUtils",
          "S",
          "isLegalPag",
          graph,
          "getReason"
        ),
        "\n"
      )
    },
    all_subsets_independencefact_s = function(graph) {
      msep <- .jcall(
        "edu/cmu/tetrad/search/MarkovCheck",
        "Ljava/lang/Object;",
        "new",
        graph,
        .jnew(
          "edu/cmu/tetrad/algcomparison/independence/IndTestFisherZ",
          self$data,
          0.01
        ),
        .jfield(
          "edu/cmu/tetrad/search/utils/ConditioningSetType",
          "S",
          "LOCAL_MARKOV"
        ),
        "getAllSubsetsIndependenceFacts"
      )$getMsep()
      facts <- list()
      msep_size <- .jcall(msep, "I", "size")
      for (i in 0:(msep_size - 1)) {
        fact <- .jcall(msep, "Ljava/lang/Object;", "get", as.integer(i))
        x <- .jcall(fact, "S", "getX")
        y <- .jcall(fact, "S", "getY")
        zlist <- .jnew(
          "java/util/ArrayList",
          .jcall(fact, "Ljava/util/List;", "getZ")
        )
        fact_ <- c(x, y)
        z_size <- .jcall(zlist, "I", "size")
        if (z_size > 0) {
          for (j in 0:(z_size - 1)) {
            fact_ <- c(
              fact_,
              .jcall(
                zlist,
                "Ljava/lang/Object;",
                "get",
                as.integer(j)
              )$toString()
            )
          }
        }
        facts[[length(facts) + 1]] <- fact_
      }
      return(facts)
    },
    all_subsets_dependencefact_s = function(graph) {
      mconn <- .jcall(
        "edu/cmu/tetrad/search/MarkovCheck",
        "Ljava/lang/Object;",
        "getAllSubsetsIndependenceFacts",
        graph,
        self$test,
        .jfield(
          "edu/cmu/tetrad/search/utils/ConditioningSetType",
          "S",
          "LOCAL_MARKOV"
        )
      )$getMconn()
      facts <- list()
      mconn_size <- .jcall(mconn, "I", "size")
      for (i in 0:(mconn_size - 1)) {
        fact <- .jcall(mconn, "Ljava/lang/Object;", "get", as.integer(i))
        x <- .jcall(fact, "S", "getX")
        y <- .jcall(fact, "S", "getY")
        zlist <- .jnew(
          "java/util/ArrayList",
          .jcall(fact, "Ljava/util/List;", "getZ")
        )
        fact_ <- c(x, y)
        z_size <- .jcall(zlist, "I", "size")
        if (z_size > 0) {
          for (j in 0:(z_size - 1)) {
            fact_ <- c(
              fact_,
              .jcall(
                zlist,
                "Ljava/lang/Object;",
                "get",
                as.integer(j)
              )$toString()
            )
          }
        }
        facts[[length(facts) + 1]] <- fact_
      }
      return(facts)
    },
    markov_check = function(graph,
                            percent_resample = 1,
                            condition_set_type = .jfield(
                              "edu/cmu/tetrad/search/utils/ConditioningSetType",
                              "S",
                              "ORDERED_LOCAL_MARKOV"
                            ),
                            remove_extraneous = FALSE,
                            parallelized = TRUE,
                            sample_size = -1) {
      if (is.null(self$mc_test)) {
        stop(
          "A test for the Markov Checker has not been set. Please call a use_{test name} method with use_for_mc=TRUE."
        )
      }
      test_obj <- self$mc_test$getTest(self$data, self$params)
      mc <- .jnew(
        "edu/cmu/tetrad/search/MarkovCheck",
        graph,
        test_obj,
        condition_set_type
      )
      mc$setKnowledge(self$knowledge)
      mc$setPercentResample(percent_resample)
      mc$setFindSmallestSubset(remove_extraneous)
      mc$setParallelized(parallelized)
      mc$generateResults(TRUE)
      self$mc_ind_results <- mc$getResults(TRUE)
      if (sample_size != -1) {
        mc$setSampleSize(sample_size)
      }
      ad_ind <- mc$getAndersonDarlingP(TRUE)
      ad_dep <- mc$getAndersonDarlingP(FALSE)
      ks_ind <- mc$getKsPValue(TRUE)
      ks_dep <- mc$getKsPValue(FALSE)
      bin_indep <- mc$getBinomialPValue(TRUE)
      bin_dep <- mc$getBinomialPValue(FALSE)
      frac_dep_ind <- mc$getFractionDependent(TRUE)
      frac_dep_dep <- mc$getFractionDependent(FALSE)
      num_tests_ind <- mc$getNumTests(TRUE)
      num_tests_dep <- mc$getNumTests(FALSE)
      return(list(
        ad_ind = ad_ind,
        ad_dep = ad_dep,
        ks_ind = ks_ind,
        ks_dep = ks_dep,
        bin_indep = bin_indep,
        bin_dep = bin_dep,
        frac_dep_ind = frac_dep_ind,
        frac_dep_dep = frac_dep_dep,
        num_tests_ind = num_tests_ind,
        num_tests_dep = num_tests_dep,
        mc = mc
      ))
    },
    get_mc_ind_pvalues = function() {
      pvalues <- c()
      size <- .jcall(self$mc_ind_results, "I", "size")
      for (i in 0:(size - 1)) {
        r <- .jcall(
          self$mc_ind_results,
          "Ljava/lang/Object;",
          "get",
          as.integer(i)
        )
        pvalues <- c(pvalues, .jcall(r, "D", "getPValue"))
      }
      return(pvalues)
    },
    get_adjustment_sets = function(graph,
                                   source,
                                   target,
                                   max_num_sets = 10,
                                   max_distance_from_point = 5,
                                   near_which_endpoint = 1,
                                   max_path_length = 20) {
      return(.jcall(graph, "Ljava/lang/Object;", "paths")$adjustmentSets(
        source,
        target,
        max_num_sets,
        max_distance_from_point,
        near_which_endpoint,
        max_path_length
      ))
    }
  ),
  # Scores and tests are private
  # and should be called through
  # set_score and set_test.
  private = list(
    use_sem_bic = function(penalty_discount = 2,
                           structure_prior = 0,
                           sem_bic_rule = 1) {
      set_params(
        self$params,
        PENALTY_DISCOUNT = penalty_discount,
        SEM_BIC_STRUCTURE_PRIOR = structure_prior,
        SEM_BIC_RULE = sem_bic_rule
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/SemBicScore")
      self$score <- cast_obj(self$score)
    },
    use_ebic = function(gamma = 0.8,
                        precompute_covariances = TRUE,
                        singularity_lambda = 0.0) {
      set_params(
        self$params,
        EBIC_GAMMA = gamma,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        SINGULARITY_LAMBDA = singularity_lambda
      )

      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/EbicScore")
      self$score <- cast_obj(self$score)
    },
    use_gic_score = function(penalty_discount = 1, sem_gic_rule = 4) {
      set_params(
        self$params,
        SEM_GIC_RULE = sem_gic_rule,
        PENALTY_DISCOUNT_ZS = penalty_discount
      )
      self$score <- .jnew("edu/cmu/tetrad/algcomparison/score/GicScores")
      self$score <- cast_obj(self$score)
    },
    use_mixed_variable_polynomial = function(structure_prior = 0,
                                             f_degree = 0,
                                             discretize = FALSE) {
      set_params(
        self$params,
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
    use_poisson_prior_score = function(lambda_ = 2,
                                       precompute_covariances = TRUE) {
      set_params(
        self$params,
        PRECOMPUTE_COVARIANCES = precompute_covariances,
        POISSON_LAMBDA = lambda_
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/PoissonPriorScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_zhang_shen_bound = function(risk_bound = 0.2) {
      set_params(self$params, ZS_RISK_BOUND = risk_bound)
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/ZhangShenBoundScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_bdeu = function(sample_prior = 10, structure_prior = 0) {
      set_params(
        self$params,
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
      set_params(
        self$params,
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
                                             structure_prior = 0) {
      set_params(
        self$params,
        PENALTY_DISCOUNT = penalty_discount,
        STRUCTURE_PRIOR = structure_prior
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/DegenerateGaussianBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_basis_function_bic = function(truncation_limit = 3,
                                      penalty_discount = 2) {
      set_params(
        self$params,
        TRUNCATION_LIMIT = truncation_limit,
        PENALTY_DISCOUNT = penalty_discount
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/BasisFunctionBicScore"
      )
      self$score <- cast_obj(self$score)
    },
    use_basis_function_bic_fs = function(truncation_limit = 3,
                                         penalty_discount = 2) {
      set_params(
        self$params,
        TRUNCATION_LIMIT = truncation_limit,
        PENALTY_DISCOUNT = penalty_discount
      )
      self$score <- .jnew(
        "edu/cmu/tetrad/algcomparison/score/BasisFunctionBicScoreFullSample"
      )
      self$score <- cast_obj(self$score)
    },
    use_basis_function_lrt = function(truncation_limit = 3,
                                      alpha = 0.01,
                                      use_for_mc = FALSE) {
      set_params(
        self$params,
        ALPHA = alpha,
        TRUNCATION_LIMIT = truncation_limit
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
    use_basis_function_lrt_fs = function(truncation_limit = 3,
                                         alpha = 0.01,
                                         use_for_mc = FALSE) {
      set_params(
        self$params,
        ALPHA = alpha,
        TRUNCATION_LIMIT = truncation_limit
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/BasisFunctionLrtFullSample"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/BasisFunctionLrtFullSample"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_fisher_z = function(alpha = 0.01, use_for_mc = FALSE) {
      set_params(self$params, ALPHA = alpha)
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
    use_chi_square = function(min_count = 1,
                              alpha = 0.01,
                              cell_table_type = 1,
                              use_for_mc = FALSE) {
      set_params(
        self$params,
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
    use_g_square = function(min_count = 1,
                            alpha = 0.01,
                            cell_table_type = 1,
                            use_for_mc = FALSE) {
      set_params(
        self$params,
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
      set_params(
        self$params,
        ALPHA = alpha,
        DISCRETIZE = discretize,
        NUM_CATEGORIES_TO_DISCRETIZE = num_categories_to_discretize
      )
      if (use_for_mc) {
        self$mc_test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLRT"
        )
        self$mc_test <- cast_obj(self$test)
      } else {
        self$test <- .jnew(
          "edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLRT"
        )
        self$test <- cast_obj(self$test)
      }
    },
    use_degenerate_gaussian_test = function(alpha = 0.01, use_for_mc = FALSE) {
      set_params(self$params, ALPHA = alpha)
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
      # Note: Ensure the field names match exactly those in Tetrad.
      set_params(
        self$params,
        NO_RANDOMLY_DETERMINED_INDEPENDENCE = threshold,
        CUTOFF_IND_TEST = cutoff, # adjust field name if necessary
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
    use_kci = function(alpha = 0.01,
                       approximate = TRUE,
                       scalingfact_or = 1,
                       num_bootstraps = 5000,
                       threshold = 1e-3,
                       epsilon = 1e-3,
                       kernel_type = 1,
                       polyd = 5,
                       polyc = 1,
                       use_for_mc = FALSE) {
      set_params(
        self$params,
        KCI_USE_APPROXIMATION = approximate,
        ALPHA = alpha,
        SCALING_FACTOR = scalingfact_or,
        KCI_NUM_BOOTSTRAPS = num_bootstraps,
        THRESHOLD_FOR_NUM_EIGENVALUES = threshold,
        KCI_EPSILON = epsilon,
        KERNEL_TYPE = kernel_type,
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
    use_cci = function(alpha = 0.01,
                       scalingfact_or = 2,
                       num_basis_functions = 3,
                       basis_type = 4,
                       basis_scale = 0.0,
                       use_for_mc = FALSE) {
      set_params(
        self$params,
        ALPHA = alpha,
        SCALING_FACTOR = scalingfact_or,
        NUM_BASIS_FUNCTIONS = num_basis_functions,
        BASIS_TYPE = basis_type,
        BASIS_SCALE = basis_scale
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
    set_fges = function(symmetric_first_step = FALSE,
                        max_degree = -1,
                        parallelized = FALSE,
                        faithfulness_assumed = FALSE) {
      set_params(
        self$params,
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
    set_fges_mb = function(targets = "",
                           max_degree = -1,
                           trimming_style = 3,
                           number_of_expansions = 2,
                           faithfulness_assumed = FALSE) {
      set_params(
        self$params,
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
    set_boss = function(num_starts = 1,
                        use_bes = FALSE,
                        time_lag = 0,
                        use_data_order = TRUE,
                        output_cpdag = TRUE) {
      set_params(
        self$params,
        USE_BES = use_bes,
        NUM_STARTS = num_starts,
        TIME_LAG = time_lag,
        USE_DATA_ORDER = use_data_order,
        OUTPUT_CPDAG = output_cpdag
      )

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Boss",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_restricted_boss = function(targets = "",
                                   use_bes = FALSE,
                                   num_starts = 1,
                                   allow_internal_randomness = TRUE) {
      set_params(
        self$params,
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
    set_cstar = function(targets = "",
                         file_out_path = "cstar-out",
                         selection_min_effect = 0.0,
                         num_subsamples = 50,
                         top_bracket = 10,
                         parallelized = FALSE,
                         cpdag_algorithm = 4,
                         remove_effect_nodes = TRUE,
                         sample_style = 1) {
      set_params(
        self$params,
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

      # Note: For Cstar, we pass both a test and score to the constructor.
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Cstar",
        self$test,
        self$score
      )
    },
    set_sp = function() {
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Sp",
        self$score
      )
      self$alg$setKnowledge(self$knowledge)
    },
    set_grasp = function(covered_depth = 4,
                         singular_depth = 1,
                         nonsingular_depth = 1,
                         ordered_alg = FALSE,
                         raskutti_uhler = FALSE,
                         use_data_order = TRUE,
                         num_starts = 1) {
      set_params(
        self$params,
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
    set_pc = function(conflict_rule = 1,
                      depth = -1,
                      stable_fas = TRUE,
                      guarantee_cpdag = FALSE) {
      stopifnot(
        is.numeric(conflict_rule), length(conflict_rule) == 1,
        is.numeric(depth), length(depth) == 1,
        is.logical(stable_fas), length(stable_fas) == 1,
        is.logical(guarantee_cpdag), length(guarantee_cpdag) == 1
      )

      set_params(
        self$params,
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
    set_cpc = function(conflict_rule = 1,
                       depth = -1,
                       stable_fas = TRUE,
                       guarantee_cpdag = TRUE) {
      set_params(
        self$params,
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
    set_pcmax = function(conflict_rule = 1,
                         depth = -1,
                         use_heuristic = TRUE,
                         max_disc_path_length = -1,
                         stable_fas = TRUE) {
      set_params(
        self$params,
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
    set_fci = function(depth = -1,
                       stable_fas = TRUE,
                       max_disc_path_length = -1,
                       complete_rule_set_used = TRUE,
                       guarantee_pag = FALSE) {
      set_params(
        self$params,
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
    set_rfci = function(depth = -1,
                        stable_fas = TRUE,
                        max_disc_path_length = -1,
                        complete_rule_set_used = TRUE) {
      set_params(
        self$params,
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
    set_cfci = function(depth = -1,
                        max_disc_path_length = -1,
                        complete_rule_set_used = TRUE) {
      set_params(
        self$params,
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
    set_gfci = function(depth = -1,
                        max_degree = -1,
                        max_disc_path_length = -1,
                        complete_rule_set_used = TRUE,
                        guarantee_pag = FALSE) {
      set_params(
        self$params,
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
    set_bfci = function(depth = -1,
                        max_disc_path_length = -1,
                        complete_rule_set_used = TRUE,
                        guarantee_pag = FALSE) {
      set_params(
        self$params,
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
    set_lv_lite = function(num_starts = 1,
                           max_blocking_path_length = 5,
                           depth = 5,
                           max_disc_path_length = 5,
                           guarantee_pag = TRUE) {
      set_params(
        self$params,
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
    set_grasp_fci = function(depth = -1,
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
      set_params(
        self$params,
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
    set_spfci = function(max_disc_path_length = -1,
                         complete_rule_set_used = TRUE,
                         depth = -1,
                         guarantee_pag = FALSE) {
      set_params(
        self$params,
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
    set_ica_lingam = function(ica_a = 1.1,
                              ica_max_iter = 5000,
                              ica_tolerance = 1e-8,
                              threshold_b = 0.1) {
      set_params(
        self$params,
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
    set_ica_lingd = function(ica_a = 1.1,
                             ica_max_iter = 5000,
                             ica_tolerance = 1e-8,
                             threshold_b = 0.1,
                             threshold_w = 0.1) {
      set_params(
        self$params,
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
    set_fask = function(alpha = 0.05,
                        depth = -1,
                        fask_delta = -0.3,
                        left_right_rule = 1,
                        skew_edge_threshold = 0.3) {
      set_params(
        self$params,
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
    set_fofc = function(alpha = 0.001,
                        penalty_discount = 2.0,
                        tetrad_test = 1,
                        include_structure_model = TRUE,
                        precompute_covariances = TRUE) {
      set_params(
        self$params,
        ALPHA = alpha,
        PENALTY_DISCOUNT = penalty_discount,
        TETRAD_TEST_FOFC = tetrad_test,
        INCLUDE_STRUCTURE_MODEL = include_structure_model,
        PRECOMPUTE_COVARIANCES = precompute_covariances
      )

      self$alg <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/cluster/Fofc")
    },
    set_ccd = function(depth = -1, apply_r1 = TRUE) {
      if (.jcall(self$knowledge, "Z", "isEmpty") == FALSE) {
        cat("CCD does not use knowledge.\n")
        return()
      }
      set_params(self$params, DEPTH = depth, APPLY_R1 = apply_r1)

      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Ccd",
        self$test
      )
    },
    set_svar_fci = function(penalty_discount = 2) {
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
      # Wrap penalty_discount and set it on ts_score.
      ts_score$setPenaltyDiscount(.jcast(
        .jnew("java/lang/Double", as.double(penalty_discount)),
        "java/lang/Object"
      ))
      svar_fci <- .jnew("edu/cmu/tetrad/search/utils/SvarFci", ts_test)
      svar_fci$setKnowledge(lagged_data$getKnowledge())
      svar_fci$setVerbose(TRUE)
      self$alg <- svar_fci
    },
    set_direct_lingam = function() {
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/DirectLingam",
        self$score
      )
    },
    set_dagma = function(lambda1 = 0.05, w_threshold = 0.1, cpdag = TRUE) {
      set_params(
        self$params,
        LAMBDA1 = lambda1,
        W_THRESHOLD = w_threshold,
        CPDAG = cpdag
      )
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/Dagma"
      )
    },
    set_pc_lingam = function() {
      self$alg <- .jnew(
        "edu/cmu/tetrad/algcomparison/algorithm/continuous/dag/PcLingam"
      )
    },
    set_svar_gfci = function(penalty_discount = 2) {
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
      svar_gfci$setVerbose(TRUE)
      self$alg <- svar_gfci
    }
  )
)

mimbuild <- function(
    clustering,
    measure_names,
    latent_names,
    cov,
    full_graph = FALSE) {
  mb <- .jnew("edu/cmu/tetrad/search/cluster/Mimbuild")
  graph <- .jcall(
    mb,
    "Ljava/lang/Object;",
    "search",
    clustering,
    measure_names,
    latent_names,
    cov
  )
  if (full_graph) {
    return(.jcall(mb, "Ljava/lang/Object;", "getFullGraph"))
  } else {
    return(graph)
  }
}
