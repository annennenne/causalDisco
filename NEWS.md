# causalDisco (development version)

* This release should be considered essentially a **new package** with many new features.
* For usage examples and detailed explanations, please see the getting started vignette (TODO: add link to vignette).

## Breaking Changes

* All functions have been renamed to follow **snake_case**:
  - `graph2amat` → `graph_to_amat`
  - `essgraph2amat` → `essgraph_to_amat`
  - `probmat2amat` → `probmat_to_amat`
  - `nedges` → `n_edges`
  - `maxnedges` → `max_edges`
  - `nDAGs` → `n_dags`
  - `corTest` → `cor_test`
  - `regTest` → `reg_test`
  - `F1` → `f1_score`
  - `G1` → `g1_score`
  - `FDR` → `fdr`
  - `NPV` → `npv`
  - `FOR` → `false_omission_rate`
  - `simGausFromDAG` → `sim_gaus_from_dag`
  - `simDAG` → `sim_dag`
  - `gausCorScore` → `gaus_cor_score`

* All R6 classes have been renamed to **CamelCase**:
  - `bnlearnSearch` → `BnlearnSearch`
  - `causalDiscoSearch` → `CausalDiscoSearch`
  - `pcalgSearch` → `PcalgSearch`

* All datasets have been renamed to **snake_case**:
  - `tpcExample` → `tpc_example`

# causalDisco 0.9.3
* Changed structure of tpdag and tskeleton objects so they include tamat objects
rather than amat and order slots. This is a major change, not backwards 
compatible! 
* tpc now passes ... argument to skeleton() function call 
* Added functionality in tpc to handle missing information when using built-in
tests. Three options are now available: complete case analysis, test wise deletion,
or no handling (i.e., and error is thrown if there are NAs, this is the default and
it is backwards compatible). 

# causalDisco 0.9.2
* Fixed bug in maketikz() function so it now works with tpdag and tamat objects,
  and added examples to the documentation. 

# causalDisco 0.9.1
* Added three new functions: simDAG, simGausFromDAG, nDAGs.
* Changed structure of tamat objects so they inherit directly from matrix.

# causalDisco 0.9.0
* Added a NEWS.md file to track changes to the package.
* First CRAN submission.
