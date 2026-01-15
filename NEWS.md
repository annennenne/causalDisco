# causalDisco (development version)

* This release should be considered essentially a **new package** with many new features.
* For usage examples and detailed explanations, please see the getting started vignette (TODO: add link to vignette).

## Breaking Changes

* All functions have been renamed to follow **snake_case**:
  - `graph2amat` to `graph_to_amat`
  - `essgraph2amat` to `essgraph_to_amat`
  - `probmat2amat` to `probmat_to_amat`
  - `nedges` to `n_edges`
  - `maxnedges` to `max_edges`
  - `nDAGs` to `n_dags`
  - `corTest` to `cor_test`
  - `regTest` to `reg_test`
  - `F1` to `f1_score`
  - `G1` to `g1_score`
  - `FDR` to `fdr`
  - `NPV` to `npv`
  - `FOR` to `false_omission_rate`
  - `simGausFromDAG` to `sim_gaus_from_dag`
  - `simDAG` to `sim_dag`
  - `gausCorScore` to `gaus_cor_score`
  - `plotTempoMech` to `plot_tempo_mech`
  - `maketikz` to `make_tikz`

* All R6 classes have been renamed to **CamelCase**:
  - `bnlearnSearch` to `BnlearnSearch`
  - `causalDiscoSearch` to `CausalDiscoSearch`
  - `pcalgSearch` to `PcalgSearch`

* All datasets have been renamed to **snake_case**:
  - `tpcExample` to `tpc_example`
  
* All functions (WIP) now take **snake_case** arguments.

* Changed structure of tpdag and tskeleton objects so they include tamat objects
rather than amat and order slots.

## Bug fixes

* Fixed bug in maketikz() function so it now works with tpdag and tamat objects,
  and added examples to the documentation.

## New features for old functions

* tpc now passes ... argument to skeleton() function call 
* Added functionality in tpc to handle missing information when using built-in
tests. Three options are now available: complete case analysis, test wise deletion,
or no handling (i.e., and error is thrown if there are NAs, this is the default and
it is backwards compatible).

# causalDisco 0.9.1
* Added three new functions: simDAG, simGausFromDAG, nDAGs.
* Changed structure of tamat objects so they inherit directly from matrix.

# causalDisco 0.9.0
* Added a NEWS.md file to track changes to the package.
* First CRAN submission.
