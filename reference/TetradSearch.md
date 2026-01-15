# R6 Interface to Tetrad Search Algorithms

High-level wrapper around the Java-based **Tetrad** causal-discovery
library. The class lets you choose independence tests, scores, and
search algorithms from Tetrad, run them on an R data set, and retrieve
the resulting graph or statistics.

## Public fields

- `data`:

  Java object that stores the (possibly converted) data set used by
  Tetrad.

- `rdata`:

  Original **R** `data.frame` supplied by the user.

- `score`:

  Java object holding the scoring function selected with `set_score()`.
  Supply one of the method strings for `set_score()`. Recognised values
  are:

  - `"sem_bic"` - SEM BIC score.

  - `"ebic"` - Extended BIC score.

  - `"bdeu"` - Bayes Dirichlet Equivalent score with uniform priors.

  - `"basis_function_bic"` - BIC score for basis-function models. This
    is a generalization of the Degenerate Gaussian score.

  - `"conditional_gaussian"` - Mixed discrete/continuous BIC score.

  - `"degenerate_gaussian"` - Degenerate Gaussian BIC score.

  - `"discrete_bic"` - BIC score for discrete data.

  - `"gic"` - Generalized Information Criterion (GIC) score.

  - `"mag_degenerate_gaussian_bic"` - MAG Degenerate Gaussian BIC Score.

  - `"mixed_variable_polynomial"` - Mixed variable polynomial BIC score.

  - `"poisson_prior"` - Poisson prior score.

  - `"zhang_shen_bound"` - Gaussian Extended BIC score.

- `test`:

  Java object holding the independence test selected with `set_test()`.
  Supply one of the method strings for `set_test()`. Recognised values
  are:

  - `"chi_square"` - chi-squared test

  - `"g_square"` - likelihood-ratio \\G^2\\ test

  - `"basis_function_lrt"` - basis-function likelihood-ratio

  - `"probabilistic"` - Uses BCInference by Cooper and Bui to calculate
    probabilistic conditional independence judgments.

  - `"fisher_z"` - Fisher \\Z\\ (partial correlation) test

  - `"degenerate_gaussian"` - Degenerate Gaussian test as a likelihood
    ratio test

  - `"conditional_gaussian"` - Mixed discrete/continuous test

  - `"kci"` - Kernel Conditional Independence Test (KCI) by Kun Zhang

- `alg`:

  Java object representing the search algorithm. Supply one of the
  method strings for `set_alg()`. Recognised values are:

  - `"boss"` - BOSS algorithm.

  - `"boss_fci"` - BOSS-FCI algorithm.

  - `"ccd"` - Cyclic Causal Discovery.

  - `"cfci"` - Adjusts FCI to use conservative orientation as in CPC.

  - `"cpc"` - Conservative PC algorithm.

  - `"cstar"` - CStaR algorithm (Causal Stability Ranking).

  - `"dagma"` - DAGMA algorithm.

  - `"direct_lingam"` - DirectLiNGAM algorithm.

  - `"fask"` - FASK algorithm.

  - `"fci"` - FCI algorithm.

  - `"fges"` - Fast Greedy Equivalence Search (FGES) algorithm.

  - `"fges_mb"` - Fast Greedy Equivalence Search with Markov Blanket
    (FGES-MB) algorithm.

  - `"gfci"` - GFCI algorithm. Combines FGES and FCI.

  - `"grasp"` - GRaSP (Greedy Relations of Sparsest Permutation)
    algorithm.

  - `"grasp_fci"` - GRaSP-FCI algorithm. Combines GRaSP and FCI.

  - `"ica_lingam"` - ICA LiNGAM algorithm.

  - `"ica_lingd"` - ICA-LiNG-D algorithm

  - `"fcit"` - FCI Targeted Testing (FCIT) algorithm

  - `"pc"` - Peter-Clark (PC) algorithm

  - `"pc_max"` - PCMax algorithm

  - `"restricted_boss"` - Restricted BOSS algorithm

  - `"rfci"` - Restricted FCI algorithm

  - `"sp"` - Sparsest Permutation algorithm

  - `"sp_fci"` - Sparsest Permutation using FCI

- `mc_test`:

  Java independence-test object used by the Markov checker.

- `java`:

  Java object returned by the search (typically a graph).

- `result`:

  Convenience alias for `java`; may store additional metadata depending
  on the search type.

- `knowledge`:

  Java `Knowledge` object carrying background constraints
  (required/forbidden edges).

- `params`:

  Java `Parameters` object holding algorithm settings.

- `bootstrap_graphs`:

  Java `List` of graphs produced by bootstrap resampling, if that
  feature was requested.

- `mc_ind_results`:

  Java `List` with Markov-checker test results.

## Methods

### Public methods

- [`TetradSearch$new()`](#method-TetradSearch-new)

- [`TetradSearch$set_test()`](#method-TetradSearch-set_test)

- [`TetradSearch$set_score()`](#method-TetradSearch-set_score)

- [`TetradSearch$set_alg()`](#method-TetradSearch-set_alg)

- [`TetradSearch$set_knowledge()`](#method-TetradSearch-set_knowledge)

- [`TetradSearch$set_params()`](#method-TetradSearch-set_params)

- [`TetradSearch$get_parameters_for_function()`](#method-TetradSearch-get_parameters_for_function)

- [`TetradSearch$run_search()`](#method-TetradSearch-run_search)

- [`TetradSearch$set_bootstrapping()`](#method-TetradSearch-set_bootstrapping)

- [`TetradSearch$set_data()`](#method-TetradSearch-set_data)

- [`TetradSearch$set_verbose()`](#method-TetradSearch-set_verbose)

- [`TetradSearch$set_time_lag()`](#method-TetradSearch-set_time_lag)

- [`TetradSearch$get_data()`](#method-TetradSearch-get_data)

- [`TetradSearch$get_knowledge()`](#method-TetradSearch-get_knowledge)

- [`TetradSearch$get_java()`](#method-TetradSearch-get_java)

- [`TetradSearch$get_string()`](#method-TetradSearch-get_string)

- [`TetradSearch$get_dot()`](#method-TetradSearch-get_dot)

- [`TetradSearch$get_amat()`](#method-TetradSearch-get_amat)

- [`TetradSearch$clone()`](#method-TetradSearch-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the `TetradSearch` object, creating new Java objects for
`knowledge` and `params`.

#### Usage

    TetradSearch$new()

------------------------------------------------------------------------

### Method `set_test()`

Sets the independence test to use in Tetrad.

#### Usage

    TetradSearch$set_test(method, ..., mc = FALSE)

#### Arguments

- `method`:

  (character) Name of the test method (e.g., "chi_square", "fisher_z").

  - `"chi_square"` - chi-squared test

  - `"g_square"` - likelihood-ratio \\G^2\\ test

  - `"basis_function_lrt"` - basis-function likelihood-ratio

  - `"probabilistic"` - Uses BCInference by Cooper and Bui to calculate
    probabilistic conditional independence judgments.

  - `"fisher_z"` - Fisher \\Z\\ (partial correlation) test

  - `"degenerate_gaussian"` - Degenerate Gaussian test as a likelihood
    ratio test

  - `"conditional_gaussian"` - Mixed discrete/continuous test

  - `"kci"` - Kernel Conditional Independence Test (KCI) by Kun Zhang

- `...`:

  Additional arguments passed to the private test-setting methods. For
  the following tests, the following parameters are available:

  - `"chi_square"` - chi-squared test

    - `min_count = 1` - Minimum count for the chi-squared test per cell.
      Increasing this can improve accuracy of the test estimates,

    - `alpha = 0.01` - Significance level for the independence test,

    - `cell_table_type = "ad"` - The type of cell table to use for
      optimization. Available types are: `"ad"` - AD tree, `"count"` -
      Count sample.

  - `"g_square"` - likelihood-ratio \\G^2\\ test

    - `min_count = 1` - Minimum count for the independence test.
      Increasing this can improve accuracy of chi square estimates,

    - `alpha = 0.01` - Significance level for the chi-squared test,

    - `cell_table_type = "ad"` - The type of cell table to use for
      optimization. Available types are: `"ad"` - AD tree, `"count"` -
      Count sample.

  - `"basis_function_lrt"` - basis-function likelihood-ratio

    - `truncation_limit = 3` - Basis functions 1 through this number
      will be used. The Degenerate Gaussian category indicator variables
      for mixed data are also used,

    - `alpha = 0.01` - Significance level for the likelihood-ratio test,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse,

    - `do_one_equation_only = FALSE` - If TRUE, only one equation should
      be used when expanding the basis.

  - `"probabilistic"` - Uses BCInference by Cooper and Bui to calculate
    probabilistic conditional independence judgments.

    - `threshold = FALSE` - Set to TRUE if using the cutoff threshold
      for the independence test,

    - `cutoff = 0.5` - Cutoff for the independence test,

    - `prior_ess = 10` - Prior equivalent sample size for the
      independence test. This number is added to the sample size for
      each conditional probability table in the model and is divided
      equally among the cells in the table.

  - `"fisher_z"` - Fisher \\Z\\ (partial correlation) test

    - `alpha = 0.01` - Significance level for the independence test,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

  - `"degenerate_gaussian"` - Degenerate Gaussian likelihood ratio test

    - `alpha = 0.01` - Significance level for the independence test,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

  - `"conditional_gaussian"` - Mixed discrete/continuous test

    - `alpha = 0.01` - Significance level for the independence test,

    - `discretize = TRUE` - If TRUE for the conditional Gaussian
      likelihood, when scoring X –\> D where X is continuous and D
      discrete, one should to simply discretize X for just those cases.
      If FALSE, the integration will be exact,

    - `num_categories_to_discretize = 3` - In case the exact algorithm
      is not used for discrete children and continuous parents is not
      used, this parameter gives the number of categories to use for
      this second (discretized) backup copy of the continuous variables,

    - `min_sample_size_per_cell = 4` - Minimum sample size per cell for
      the independence test.

  - `"kci"` - Kernel Conditional Independence Test (KCI) by Kun Zhang

    - `alpha = 0.01` - Significance level for the independence test,

    - `approximate = TRUE` - If TRUE, use the approximate Gamma
      approximation algorithm. If FALSE, use the exact,

    - `scaling_factor = 1` - For Gaussian kernel: The scaling factor \*
      Silverman bandwidth.

    - `num_bootstraps = 5000` - Number of bootstrap samples to use for
      the KCI test.

    - `threshold = 1e-3` - Threshold for the KCI test. Threshold to
      determine how many eigenvalues to use – the lower the more (0 to
      1).

    - `kernel_type = "gaussian"` - The type of kernel to use. Available
      types are `"gaussian"`, `"linear"`, or `"polynomial"`.

    - `polyd = 5` - The degree of the polynomial kernel, if used.

    - `polyc = 1` - The constant of the polynomial kernel, if used.

- `mc`:

  (logical) If TRUE, sets this test for the Markov checker `mc_test`.

#### Returns

Invisibly returns `self`, for chaining.

------------------------------------------------------------------------

### Method `set_score()`

Sets the scoring function to use in Tetrad.

#### Usage

    TetradSearch$set_score(method, ...)

#### Arguments

- `method`:

  (character) Name of the score (e.g., "sem_bic", "ebic", "bdeu").

  - `"sem_bic"` - SEM BIC score.

  - `"ebic"` - Extended BIC score.

  - `"bdeu"` - Bayes Dirichlet Equivalent score with uniform priors.

  - `"basis_function_bic"` - BIC score for basis-function models. This
    is a generalization of the Degenerate Gaussian score.

  - `"conditional_gaussian"` - Mixed discrete/continuous BIC score.

  - `"degenerate_gaussian"` - Degenerate Gaussian BIC score.

  - `"discrete_bic"` - BIC score for discrete data.

  - `"gic"` - Generalized Information Criterion (GIC) score.

  - `"mag_degenerate_gaussian_bic"` - MAG Degenerate Gaussian BIC Score.

  - `"poisson_prior"` - Poisson prior score.

  - `"zhang_shen_bound"` - Gaussian Extended BIC score.

- `...`:

  Additional arguments passed to the private score-setting methods. For
  the following scores, the following parameters are available:

  - `sem_bic` - SEM BIC score.

    - `penalty_discount = 2` - Penalty discount factor used in BIC =
      2L - ck log N, where c is the penalty. Higher c yield sparser
      graphs,

    - `structure_prior = 0` - The default number of parents for any
      conditional probability table. Higher weight is accorded to tables
      with about that number of parents. The prior structure weights are
      distributed according to a binomial distribution,

    - `sem_bic_rule = 1` - The Chickering Rule uses the difference of
      BIC scores to add or remove edges. The Nandy et al. rule uses a
      single calculation of a partial correlation in place of the
      likelihood difference,

    - `precompute_covariances = TRUE` - For more than 5000 variables or
      so, set this to FALSE in order to calculate covariances on the fly
      from data,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse

  - `ebic` - Extended BIC score.

    - `gamma` - The gamma parameter in the EBIC score.

    - `precompute_covariances = TRUE` - For more than 5000 variables or
      so, set this to FALSE in order to calculate covariances on the fly
      from data,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

  - `bdeu` - Bayes Dirichlet Equivalent score with uniform priors.

    - `sample_prior = 10` - This sets the prior equivalent sample size.
      This number is added to the sample size for each conditional
      probability table in the model and is divided equally among the
      cells in the table,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

  - `basis_function_bic` - BIC score for basis-function models. This is
    a generalization of the Degenerate Gaussian score.

    - `truncation_limit = 3` - Basis functions 1 though this number will
      be used. The Degenerate Gaussian category indicator variables for
      mixed data are also used,

    - `penalty_discount = 2` - Penalty discount. Higher penalty yields
      sparser graphs,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse,

    - `do_one_equation_only = FALSE` - If TRUE, only one equation should
      be used when expanding the basis.

  - `conditional_gaussian` - Mixed discrete/continuous BIC score.

    - `penalty_discount = 1` - Penalty discount. Higher penalty yields
      sparser graphs,

    - `discretize = TRUE` - If TRUE for the conditional Gaussian
      likelihood, when scoring X –\> D where X is continuous and D
      discrete, one should to simply discretize X for just those cases.
      If FALSE, the integration will be exact,

    - `num_categories_to_discretize = 3` - In case the exact algorithm
      is not used for discrete children and continuous parents is not
      used, this parameter gives the number of categories to use for
      this second (discretized) backup copy of the continuous variables,

    - `structure_prior = 0` - The default number of parents for any
      conditional probability table. Higher weight is accorded to tables
      with about that number of parents. The prior structure weights are
      distributed according to a binomial distribution.

  - `"degenerate_gaussian"` - Degenerate Gaussian BIC score.

    - `penalty_discount = 1` - Penalty discount. Higher penalty yields
      sparser graphs,

    - `structure_prior = 0` - The default number of parents for any
      conditional probability table. Higher weight is accorded to tables
      with about that number of parents. The prior structure weights are
      distributed according to a binomial distribution,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

    - `precompute_covariances = TRUE` - For more than 5000 variables or
      so, set this to FALSE in order to calculate covariances on the fly
      from data.

  - `"discrete_bic"` - BIC score for discrete data.

    - `structure_prior = 0` - The default number of parents for any
      conditional probability table. Higher weight is accorded to tables
      with about that number of parents. The prior structure weights are
      distributed according to a binomial distribution.

  - `"gic"` - Generalized Information Criterion (GIC) score.

    - `penalty_discount = 1` - Penalty discount. Higher penalty yields
      sparser graphs,

    - `sem_gic_rule = "bic"` - The following rules are available:
      `"bic"` - \\\ln n\\, `"gic2"` - \\p n^{1/3}\\, `"ric"` - \\2 \ln(p
      n)\\, `"ricc"` - \\2(\ln(p n) + \ln\ln(p n))\\, `"gic6"` - \\\ln n
      \ln(p n)\\.

    - `precompute_covariances = TRUE` - For more than 5000 variables or
      so, set this to FALSE in order to calculate covariances on the fly
      from data,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

  - `"mag_degenerate_gaussian_bic"` - MAG Degenerate Gaussian BIC Score.

    - `penalty_discount = 1` - Penalty discount. Higher penalty yields
      sparser graphs,

    - `structure_prior = 0` - The default number of parents for any
      conditional probability table. Higher weight is accorded to tables
      with about that number of parents. The prior structure weights are
      distributed according to a binomial distribution,

    - `precompute_covariances = TRUE` - For more than 5000 variables or
      so, set this to FALSE in order to calculate covariances on the fly
      from data.

    &nbsp;

    - `structure_prior = 0` - The default number of parents for any
      conditional probability table. Higher weight is accorded to tables
      with about that number of parents. The prior structure weights are
      distributed according to a binomial distribution,

    - `f_degree = 0` - The f degree.

    - `discretize = FALSE` - If TRUE for the conditional Gaussian
      likelihood, when scoring X –\> D where X is continuous and D
      discrete, one should to simply discretize X for just those cases.
      If FALSE, the integration will be exact.

  - `"poisson_prior"` - Poisson prior score.

    - `poission_lambda = 2` - Lambda parameter for the Poisson
      distribution (\> 0),

    - `precompute_covariances = TRUE` - For more than 5000 variables or
      so, set this to FALSE in order to calculate covariances on the fly
      from data,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

  - `"zhang_shen_bound"` - Gaussian Extended BIC score.

    - `risk_bound = 0.2` - This is the probability of getting the true
      model if a correct model is discovered. Could underfit.

    - `precompute_covariances = TRUE` - For more than 5000 variables or
      so, set this to FALSE in order to calculate covariances on the fly
      from data,

    - `singularity_lambda = 0.0` - Small number \>= 0: Add lambda to the
      diagonal, \< 0 Pseudoinverse.

#### Returns

Invisibly returns `self`.

------------------------------------------------------------------------

### Method `set_alg()`

Sets the causal discovery algorithm to use in Tetrad.

#### Usage

    TetradSearch$set_alg(method, ...)

#### Arguments

- `method`:

  (character) Name of the algorithm (e.g., "fges", "pc", "fci", etc.).

- `...`:

  Additional parameters passed to the private algorithm-setting methods.
  For the following algorithms, the following parameters are available:

  - `"boss"` - BOSS algorithm.

    - `num_starts = 1` - The number of times the algorithm should be
      started from different initializations. By default, the algorithm
      will be run through at least once using the initialized
      parameters,

    - `use_bes = TRUE` - If TRUE, the algorithm uses the backward
      equivalence search from the GES algorithm as one of its steps,

    - `use_data_order = TRUE` - If TRUE, the data variable order should
      be used for the first initial permutation,

    - `output_cpdag = TRUE` - If TRUE, the DAG output of the algorithm
      is converted to a CPDAG.

  - `"boss_fci"` - BOSS-FCI algorithm.

    - `depth = -1` - Maximum size of conditioning set, Set to -1 for
      unlimited,

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path, Set to -1 for unlimited,

    - `use_bes = TRUE` - If TRUE, the algorithm uses the backward
      equivalence search from the GES algorithm as one of its steps,

    - `use_heuristic` - If TRUE, use the max p heuristic version,

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness,

    - `guarantee_pag = FALSE` - Ensure the output is a legal PAG (where
      feasible).

  - `"ccd"` - Cyclic Causal Discovery.

    - `depth = -1` - Maximum size of conditioning set,

    - `apply_r1 = TRUE` - Set this parameter to FALSE if a chain of
      directed edges pointing in the same direction, when only the first
      few such orientations are justified based on the data.

  - `"cfci"` - Adjusts FCI to use conservative orientation as in CPC.

    - `depth = -1` - Maximum size of conditioning set,

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path,

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness.

  - `"cpc"` - Conservative PC algorithm.

    - `conflict_rule = 1` - The value of `conflict_rule` determines how
      collider conflicts are handled. `1` corresponds to the "overwrite"
      rule as introduced in the pcalg package, see
      [`pcalg::pc()`](https://rdrr.io/pkg/pcalg/man/pc.html). `2` means
      that all collider conflicts using bidirected edges should be
      prioritized, while `3` means that existing colliders should be
      prioritized, ignoring subsequent conflicting information.

    - `depth = -1` - Maximum size of conditioning set,

    - `stable_fas = TRUE` - If TRUE, the "stable" version of the PC
      adjacency search is used, which for k \> 0 fixes the graph for
      depth k + 1 to that of the previous depth k.

    - `guarantee_cpdag = FALSE` - If TRUE, ensure the output is a legal
      CPDAG.

  - `"cstar"` - CStaR algorithm (Causal Stability Ranking).

    - `targets = ""` - Target names (comma or space separated),

    - `file_out_path = "cstar_out"` - Path to a directory in which
      results can be stored

    - `selection_min_effect = 0.0` - Minimum effect size for listing
      effects in the CStaR table

    - `num_subsamples = 50` - CStaR works by generating subsamples and
      summarizing across them; this specifies the number of subsamples
      to generate. Must be \>= 1,

    - `top_bracket = 10` - Top bracket to look for causes in,

    - `parallelized = FALSE` - If TRUE, the algorithm should be
      parallelized,

    - `cpdag_algorithm = "restricted_boss"` - The CPDAG algorithm to
      use. `"pc"` corresponds to PC Stable, `"fges"` selects the FGES
      algorithm, `"boss"` selects the BOSS algorithm, and
      `"restricted_boss"` selects the restricted BOSS variant.

    - `remove_effect_nodes = TRUE` - If TRUE, the effect nodes should be
      removed from possible causes,

    - `sample_style = "subsample"` - The sampling style to use.
      Available options are `"subsample"` and `"bootstrap"`.

  - `"dagma"` - DAGMA algorithm.

    - `lambda1 = 0.05` - Tuning parameter for DAGMA,

    - `w_threshold = 0.1` - Second tuning parameter for DAGMA,

    - `cpdag = TRUE` - The algorithm returns a DAG; if this is set to
      TRUE, this DAG is converted to a CPDAG.

  - `"direct_lingam"` - DirectLiNGAM algorithm. No parameters.

  - `"fask"` - FASK algorithm.

    - `alpha = 0.05` - Significance level for the independence test,

    - `depth = -1` - Maximum size of conditioning set,

    - `fask_delta = -0.3` - The bias for orienting with negative
      coefficients (`0` means no bias) for `FASK v1`,

    - `left_right_rule = 1` - The FASK left right rule v2 is default,
      but two other (related) left-right rules are given for relation to
      the literature, and the v1 FASK rule is included for backward
      compatibility,

    - `skew_edge_threshold = 0.3` - For FASK, this includes an adjacency
      X — Y in the model if \|corr(X, Y \| X \> 0) - corr(X, Y \| Y \>
      0)\| exceeds some threshold.

  - `"fci"` - FCI algorithm.

    - `depth = -1` - Maximum size of conditioning set,

    - `stable_fas = TRUE` - If TRUE, the "stable" version of the PC
      adjacency search is used, which for k \> 0 fixes the graph for
      depth k + 1 to that of the previous depth k.

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path,

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness.

    - `guarantee_pag = FALSE` - Ensure the output is a legal PAG (where
      feasible).

  - `"fcit"` - FCI Targeted Testing (FCIT) algorithm

    - `use_bes = TRUE` - If TRUE, the algorithm uses the backward
      equivalence search from the GES algorithm as one of its steps,

    - `use_data_order = TRUE` - If TRUE, the data variable order should
      be used for the first initial permutation,

    - `num_starts = 1` - The number of times the algorithm should be
      started from different initializations. By default, the algorithm
      will be run through at least once using the initialized
      parameters,

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path,

    - `start_with = "BOSS"` - What algorithm to run first to get the
      initial CPDAG that the rest of the FCIT procedure refines.
      Available options are: `"BOSS"`, `"GRaSP"`, and `"SP"`.

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness,

    - `depth = -1` - Maximum size of conditioning set,

    - `guarantee_pag = FALSE` - Ensure the output is a legal PAG (where
      feasible).

  - `"fges"` - Fast Greedy Equivalence Search (FGES) algorithm.

    - `symmetric_first_step = FALSE` - If TRUE, scores for both X –\> Y
      and X \<– Y will be calculated and the higher score used.

    - `max_degree = -1` - Maximum degree of any node in the graph. Set
      to -1 for unlimited,

    - `parallelized = FALSE` - If TRUE, the algorithm should be
      parallelized,

    - `faithfulness_assumed = FALSE` - If TRUE, assume that if \\X
      \perp\\\\\\\perp Y\\ (by an independence test) then \\X
      \perp\\\\\\\perp Y\\ \| Z for nonempty Z.

  - `"fges_mb"` - Fast Greedy Equivalence Search with Markov Blanket
    (FGES-MB) algorithm.

    - `targets = ""` - Target names (comma or space separated),

    - `max_degree = -1` - Maximum degree of any node in the graph. Set
      to -1 for unlimited,

    - `trimming_style = "mb_dags"` - The trimming style to use: `"none"`
      applies no trimming. `"adj"` trims to the adjacencies of the
      targets. `"mb_dags"` trims to Union(MB(targets)) U targets.
      `"semidir_paths"` trims to nodes with semidirected paths to the
      targets.

    - `number_of_expansions = 2` - Number of expansions of the algorithm
      away from the target,

    - `faithfulness_assumed = FALSE` - If TRUE, assume that if \\X
      \perp\\\\\\\perp Y\\ (by an independence test) then \\X
      \perp\\\\\\\perp Y\\ \| Z for nonempty Z.

  - `"gfci"` - GFCI algorithm. Combines FGES and FCI.

    - `depth = -1` - Maximum size of conditioning set,

    - `max_degree = -1` - Maximum degree of any node in the graph. Set
      to -1 for unlimited,

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path,

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness.

    - `guarantee_pag = FALSE` - Ensure the output is a legal PAG (where
      feasible).

  - `"grasp"` - GRaSP (Greedy Relations of Sparsest Permutation)
    algorithm.

    - `covered_depth = 4` - The depth of recursion for first search,

    - `singular_depth = 1` - Recursion depth for singular tucks,

    - `nonsingular_depth = 1` - Recursion depth for nonsingular tucks,

    - `ordered_alg = FALSE` - If TRUE, earlier GRaSP stages should be
      performed before later stages,

    - `raskutti_uhler = FALSE` - If TRUE, use Raskutti and Uhler's
      DAG-building method (test); if FALSE, use Grow-Shrink (score).

    - `use_data_order = TRUE` - If TRUE, the data variable order should
      be used for the first initial permutation,

    - `num_starts = 1` - The number of times the algorithm should be
      started from different initializations. By default, the algorithm
      will be run through at least once using the initialized
      parameters.

  - `"grasp_fci"` - GRaSP-FCI algorithm. Combines GRaSP and FCI.

    - `depth = -1` - Maximum size of conditioning set,

    - `stable_fas = TRUE` - If TRUE, the "stable" version of the PC
      adjacency search is used, which for k \> 0 fixes the graph for
      depth k + 1 to that of the previous depth k.

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path,

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness,

    - `covered_depth = 4` - The depth of recursion for first search,

    - `singular_depth = 1` - Recursion depth for singular tucks,

    - `nonsingular_depth = 1` - Recursion depth for nonsingular tucks,

    - `ordered_alg = FALSE` - If TRUE, earlier GRaSP stages should be
      performed before later stages,

    - `raskutti_uhler = FALSE` - If TRUE, use Raskutti and Uhler's
      DAG-building method (test); if FALSE, use Grow-Shrink (score).

    - `use_data_order = TRUE` - If TRUE, the data variable order should
      be used for the first initial permutation,

    - `num_starts = 1` - The number of times the algorithm should be
      started from different initializations. By default, the algorithm
      will be run through at least once using the initialized
      parameters,

    - `guarantee_pag = FALSE` - If TRUE, ensure the output is a legal
      PAG (where feasible).

  - `"ica_lingam"` - ICA LiNGAM algorithm.

    - `ica_a = 1.1` - The 'a' parameter of Fast ICA (see Hyvarinen, A.
      (2001)). It ranges between 1 and 2.

    - `ica_max_iter = 5000` - Maximum number if iterations of the
      optimization procedure of ICA.

    - `ica_tolerance = 1e-8` - Fast ICA tolerance parameter.

    - `threshold_b = 0.1` - The estimated B matrix is thresholded by
      setting small entries less than this threshold to zero.

  - `"ica_lingd"` - ICA-LiNG-D algorithm

    - `ica_a = 1.1` - The 'a' parameter of Fast ICA (see Hyvarinen, A.
      (2001)). It ranges between 1 and 2.

    - `ica_max_iter = 5000` - Maximum number if iterations of the
      optimization procedure of ICA.

    - `ica_tolerance = 1e-8` - Fast ICA tolerance parameter.

    - `threshold_b = 0.1` - The estimated B matrix is thresholded by
      setting small entries less than this threshold to zero.

    - `threshold_w` - The estimated W matrix is thresholded by setting
      small entries less than this threshold to zero.

  - `"pc"` - Peter-Clark (PC) algorithm

    - `conflict_rule = 1` - The value of `conflict_rule` determines how
      collider conflicts are handled. `1` corresponds to the "overwrite"
      rule as introduced in the pcalg package, see
      [`pcalg::pc()`](https://rdrr.io/pkg/pcalg/man/pc.html). `2` means
      that all collider conflicts using bidirected edges should be
      prioritized, while `3` means that existing colliders should be
      prioritized, ignoring subsequent conflicting information.

    - `depth = -1` - Maximum size of conditioning set,

    - `stable_fas = TRUE` - If TRUE, the "stable" version of the PC
      adjacency search is used, which for k \> 0 fixes the graph for
      depth k + 1 to that of the previous depth k.

    - `guarantee_cpdag = FALSE` - If TRUE, ensure the output is a legal
      CPDAG.

  - `"pc_max"` - PCMax algorithm

    - `conflict_rule = 1` - The value of `conflict_rule` determines how
      collider conflicts are handled. `1` corresponds to the "overwrite"
      rule as introduced in the pcalg package, see
      [`pcalg::pc()`](https://rdrr.io/pkg/pcalg/man/pc.html). `2` means
      that all collider conflicts using bidirected edges should be
      prioritized, while `3` means that existing colliders should be
      prioritized, ignoring subsequent conflicting information.

    - `depth = -1` - Maximum size of conditioning set,

    - `use_heuristic = TRUE` - If TRUE, use the max p heuristic version

    - `max_disc_path_length = -1` - The maximum path length to use for
      the max p heuristic version. If -1, no limit is used.

    - `stable_fas = TRUE` - If TRUE, the "stable" version of the PC
      adjacency search is used, which for k \> 0 fixes the graph for
      depth k + 1 to that of the previous depth k.

  - `"restricted_boss"` - Restricted BOSS algorithm

    - `targets = ""` - Target names (comma or space separated),

    - `use_bes = TRUE` - If TRUE, the algorithm uses the backward
      equivalence search from the GES algorithm as one of its steps,

    - `num_starts = 1` - The number of times the algorithm should be
      started from different initializations. By default, the algorithm
      will be run through at least once using the initialized
      parameters,

    - `allow_internal_randomness = TRUE` - If TRUE, the algorithm allow
      the algorithm to use certain heuristic random steps. This can
      improve performance, but may make the algorithm non-deterministic.

  - `"rfci"` - Restricted FCI algorithm

    - `depth = -1` - Maximum size of conditioning set,

    - `stable_fas = TRUE` - If TRUE, the "stable" version of the PC
      adjacency search is used, which for k \> 0 fixes the graph for
      depth k + 1 to that of the previous depth k.

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path,

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness.

    - `guarantee_pag = FALSE` - Ensure the output is a legal PAG (where
      feasible).

  - `"sp"` - Sparsest Permutation algorithm. No parameters.

  - `"sp_fci"` - Sparsest Permutation using FCI

    - `depth = -1` - Maximum size of conditioning set,

    - `max_disc_path_length = -1` - Maximum length for any
      discriminating path,

    - `complete_rule_set_used = TRUE` - FALSE if the (simpler) final
      orientation rules set due to P. Spirtes, guaranteeing arrow
      completeness, should be used; TRUE if the (fuller) set due to J.
      Zhang, should be used guaranteeing additional tail completeness,

    - `guarantee_pag = FALSE` - Ensure the output is a legal PAG (where
      feasible).

#### Returns

Invisibly returns `self`.

------------------------------------------------------------------------

### Method [`set_knowledge()`](https://bjarkehautop.github.io/causalDisco/reference/set_knowledge.md)

Sets the background knowledge object.

#### Usage

    TetradSearch$set_knowledge(knowledge_obj)

#### Arguments

- `knowledge_obj`:

  An object containing Tetrad knowledge (must implement
  `get_tetrad_knowledge`).

------------------------------------------------------------------------

### Method `set_params()`

Sets parameters for the Tetrad search.

#### Usage

    TetradSearch$set_params(...)

#### Arguments

- `...`:

  Named arguments for the parameters to set.

------------------------------------------------------------------------

### Method `get_parameters_for_function()`

Retrieves the argument names of a matching private function.

#### Usage

    TetradSearch$get_parameters_for_function(
      fn_pattern,
      score = FALSE,
      test = FALSE,
      alg = FALSE
    )

#### Arguments

- `fn_pattern`:

  (character) A pattern that should match a private method name.

- `score`:

  If TRUE, retrieves parameters for a scoring function.

- `test`:

  If TRUE, retrieves parameters for a test function.

- `alg`:

  If TRUE, retrieves parameters for an algorithm.

#### Returns

(character) The names of the parameters.

------------------------------------------------------------------------

### Method `run_search()`

Runs the chosen Tetrad algorithm on the data.

#### Usage

    TetradSearch$run_search(
      data = NULL,
      bootstrap = FALSE,
      int_cols_as_cont = TRUE
    )

#### Arguments

- `data`:

  (optional) If provided, overrides the previously set data.

- `bootstrap`:

  (logical) If TRUE, bootstrapped graphs will be generated.

- `int_cols_as_cont`:

  (logical) If `TRUE`, integer columns are treated as continuous, since
  Tetrad does not support ordinal data, but only either continuous or
  nominal data. Default is `TRUE.`

#### Returns

A `caugi` and a `knowledge` (`knowledgeable_caugi`) object. Also
populates `self$java`.

------------------------------------------------------------------------

### Method `set_bootstrapping()`

Configures bootstrapping parameters for the Tetrad search.

#### Usage

    TetradSearch$set_bootstrapping(
      number_resampling = 0,
      percent_resample_size = 100,
      add_original = TRUE,
      with_replacement = TRUE,
      resampling_ensemble = 1,
      seed = -1
    )

#### Arguments

- `number_resampling`:

  (integer) Number of bootstrap samples.

- `percent_resample_size`:

  (numeric) Percentage of sample size for each bootstrap.

- `add_original`:

  (logical) If TRUE, add the original dataset to the bootstrap set.

- `with_replacement`:

  (logical) If TRUE, sampling is done with replacement.

- `resampling_ensemble`:

  (integer) How the resamples are used or aggregated.

- `seed`:

  (integer) Random seed, or -1 for none.

------------------------------------------------------------------------

### Method `set_data()`

Sets or overrides the data used by Tetrad.

#### Usage

    TetradSearch$set_data(data, int_cols_as_cont = TRUE)

#### Arguments

- `data`:

  (data.frame) The new data to load.

- `int_cols_as_cont`:

  (logical) If `TRUE`, integer columns are treated as continuous, since
  Tetrad does not support ordinal data, but only either continuous or
  nominal data. Default is `TRUE.`

------------------------------------------------------------------------

### Method `set_verbose()`

Toggles the verbosity in Tetrad.

#### Usage

    TetradSearch$set_verbose(verbose)

#### Arguments

- `verbose`:

  (logical) TRUE to enable verbose logging, FALSE otherwise.

------------------------------------------------------------------------

### Method `set_time_lag()`

Sets an integer time lag for time-series algorithms.

#### Usage

    TetradSearch$set_time_lag(time_lag = 0)

#### Arguments

- `time_lag`:

  (integer) The time lag to set.

------------------------------------------------------------------------

### Method `get_data()`

Retrieves the current Java data object.

#### Usage

    TetradSearch$get_data()

#### Returns

(Java object) Tetrad dataset.

------------------------------------------------------------------------

### Method `get_knowledge()`

Returns the background knowledge object.

#### Usage

    TetradSearch$get_knowledge()

#### Returns

(Java object) Tetrad Knowledge.

------------------------------------------------------------------------

### Method `get_java()`

Gets the main Java result object (usually a graph) from the last search.

#### Usage

    TetradSearch$get_java()

#### Returns

(Java object) The Tetrad result graph or model.

------------------------------------------------------------------------

### Method `get_string()`

Returns the string representation of a given Java object or `self$java`.

#### Usage

    TetradSearch$get_string(java_obj = NULL)

#### Arguments

- `java_obj`:

  (Java object, optional) If NULL, uses `self$java`.

#### Returns

(character) The [`toString()`](https://rdrr.io/r/base/toString.html) of
that Java object.

------------------------------------------------------------------------

### Method `get_dot()`

Produces a DOT (Graphviz) representation of the graph.

#### Usage

    TetradSearch$get_dot(java_obj = NULL)

#### Arguments

- `java_obj`:

  (Java object, optional) If NULL, uses `self$java`.

#### Returns

(character) The DOT-format string.

------------------------------------------------------------------------

### Method `get_amat()`

Produces an amat representation of the graph.

#### Usage

    TetradSearch$get_amat(java_obj = NULL)

#### Arguments

- `java_obj`:

  (Java object, optional) If NULL, uses `self$java`.

#### Returns

(character) The adjacency matrix.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TetradSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
### tetrad_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Requires Tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  data("tpc_example")

  # Recommended:
  my_pc <- pc(engine = "tetrad", test = "conditional_gaussian")
  my_pc(tpc_example)

  # or
  disco(data = tpc_example, method = my_pc)

  # Using R6 class:
  s <- TetradSearch$new()

  s$set_data(tpc_example)
  s$set_test(method = "conditional_gaussian", alpha = 0.05)
  s$set_alg("pc")

  g <- s$run_search()

  print(g)
}
```
