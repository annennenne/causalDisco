# Add Tetrad Algorithms

This document tracks Tetrad algorithms with respect to their status in this project:
whether they are already documented, should be implemented, or should be removed.
Notes include limitations such as support for background knowledge, score/test
requirements, and nonstandard outputs.

---

## Documented and Present in Tetrad

### Do **not** implement (for now) (commented out from doc)

- `ccd` (test-based)
  Produces a **cyclic PAG**, where edge semantics differ from standard PAGs.
  Tetrad says no background knowledge, but seems only required knowledge is not allowed?

- `CStaR` (score + test)
  Unclear how to run this algorithm in Tetrad (nonstandard workflow).
  Does not accept background knowledge.

- `DAGMA` (neither score- nor test-based)
  Does not accept background knowledge.

- `Direct-LiNGAM` (score-based)
  Does not accept background knowledge.

- `ica_lingam` (neither score- nor test-based)
  Does not accept background knowledge.

- `ica_lingd` (neither score- nor test-based)
  Does not return any output when run in Tetrad.  
  Does not accept background knowledge.
  
- `FASK` (score-based)  
  Returns a **cyclic directed graph**, not a DAG/...

- `ges_mb` (score-based)
  Requires explicit specification of a Markov blanket.

---

## Not Present in Current Tetrad but Listed in Our Documentation

### Commented out from documentation

- `cfci`
- `cpc`
- `fcit`
- `pc_max`
- `sp`

---

## Present in Tetrad but Missing from Our Documentation

### Do **not** implement (for now)

- `DM-PC` (test-based)  
  Searches for intermediate latent variables.  
  Example output may include structures like `v → L1 → x` on `non_linear.csv`.

- `FAS` (test-based)  
  Adjacency (skeleton) search only; corresponds to the first phase of PC.

- `FASK-PW` (neither score- nor test-based)
  Requires both data and an **undirected graph** as input.  
  Orients edges in the supplied graph.
  
- `Factor Analysis` (neither score- nor test-based)
  Currently not functional in Tetrad and undocumented.

- `GIN` (test-based)  
  Does not accept background knowledge.

- `ImaGES` (score-based)  
  Accepts multiple datasets under the assumption of a shared graph structure.  
  Potentially useful for multiple imputation settings.

- `ImaGES-BOSS` (score-based)  
  Accepts multiple datasets under the assumption of a shared graph structure.  
  Potentially useful for multiple imputation settings.

- `IOD` (test-based)  
  Running in Tetrad fails with:  
  `illegal state: continued fraction diverged to NaN for value inf`.

- `R3` (no score/test separation)  
  Requires data and an **undirected graph** as input.  
  Orients edges in the supplied graph.
  
- `PC-MB` (test-based)  
  Requires explicit specification of a Markov blanket.
  
- `rfci` (test-based)  
  Returns an **RFCI-PAG**, not a standard PAG.  
  Already implemented in `pcalg`.
  
- `dPAG-sampling-rfci` (neither score- nor test-based)
  Returns an **RFCI-PAG**, not a standard PAG (I assume)?
  Discrete datasets only.
  
- `RSkew` (neither score- nor test-based)
  Requires data and an **undirected graph** as input.  
  Orients edges in the supplied graph.
  
- `Skew` (no score/test separation)  
  Requires data and an **undirected graph** as input.  
  Orients edges in the supplied graph.
  
### Implement

- `CDNOD-BOSS` (score-based) (coming in 7.6.11?)
