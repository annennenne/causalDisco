# Add bnlearn Algorithms

This document tracks bnlearn algorithms with respect to their status in this project: whether they are already
documented, should be implemented, or should be removed. Notes include score/test requirements, and nonstandard outputs.

## Do **not** implement (commented out from doc)

**Local / skeleton discovery**
  - `hpc` – Hybrid Parents and Children
  - `mmpc` – Max–Min Parents and Children
  - `si_hiton_pc` – Semi-Interleaved HITON-PC

since skeleton discovery isn't that useful.

**Pairwise mutual-information learners**
  - `aracne` – ARACNE network
  - `chow_liu` – Chow–Liu tree

since they learn undirected dependency structures based solely on pairwise mutual information, rather than
DAGs or CPDAGs, and therefore fall outside the project’s focus on directed graphical model structure learning.


## Maybe add? (commented out from doc for now)

**Hybrid**
- `h2pc` – Hybrid HPC–PC
- `mmhc` – Max–Min Hill-Climbing
- `rsmax2` – Restricted Maximisation (two-stage)
