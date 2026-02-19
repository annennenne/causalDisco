# causalDisco: Causal Discovery in R

Tools for causal structure learning from observational data, with
emphasis on temporally ordered variables. The package implements the
Temporal Peter–Clark (TPC) algorithm (Petersen, Osler & Ekstrøm, 2021;
[doi:10.1093/aje/kwab087](https://doi.org/10.1093/aje/kwab087) ), the
Temporal Greedy Equivalence Search (TGES) algorithm (Larsen, Ekstrøm &
Petersen, 2025;
[doi:10.48550/arXiv.2502.06232](https://doi.org/10.48550/arXiv.2502.06232)
) and Temporal Fast Causal Inference (TFCI). It provides a unified
framework for specifying background knowledge, which can be incorporated
into the implemented algorithms from the R packages 'bnlearn' (Scutari,
2010; [doi:10.18637/jss.v035.i03](https://doi.org/10.18637/jss.v035.i03)
) and 'pcalg' (Kalish et al., 2012;
[doi:10.18637/jss.v047.i11](https://doi.org/10.18637/jss.v047.i11) ), as
well as the Java library 'Tetrad' (Scheines et al., 1998;
[doi:10.1207/s15327906mbr3301_3](https://doi.org/10.1207/s15327906mbr3301_3)
). The package further includes utilities for visualization, comparison,
and evaluation of graph structures, facilitating performance evaluation
and methodological studies.

## System requirements

Java (\>= 21) for Tetrad functionality (optional; includes installer);
Source code: <https://github.com/cmu-phil/tetrad>.

## See also

Useful links:

- <https://github.com/disco-coders/causalDisco>

- <https://disco-coders.github.io/causalDisco/>

- Report bugs at <https://github.com/disco-coders/causalDisco/issues>

## Author

**Maintainer**: Bjarke Hautop Kristensen <bjarke.kristensen@sund.ku.dk>

Authors:

- Frederik Fabricius-Bjerre <frederik@fabriciusbjerre.dk>

- Anne Helby Petersen <ahpe@sund.ku.dk>

Other contributors:

- Claus Thorn Ekstrøm <ekstrom@sund.ku.dk> \[contributor\]

- Tobias Ellegaard Larsen <tobias.ellegaard@sund.ku.dk> \[contributor\]
