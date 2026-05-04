# Causal Discovery Algorithm Return Value RFCI-PAG

Causal Discovery Algorithm Return Value RFCI-PAG

## Value

A function that takes a single argument `data` (a data frame). When
called, this function returns a list containing:

- `knowledge` A `Knowledge` object with the background knowledge used in
  the causal discovery algorithm. See
  [`knowledge()`](https://disco-coders.github.io/causalDisco/reference/knowledge.md)
  for how to construct it.

- `caugi` A [`caugi::caugi`](https://caugi.org/reference/caugi.html)
  object representing the learned causal graph. This graph is an
  RFCI-PAG (RFCI Partial Ancestral Graph), but since RFCI-PAGs are not
  yet natively supported in caugi, it is currently stored with class
  `UNKNOWN`.

Please see the definition 3.2 of the paper referenced for definition of
an RFCI-PAG, and it's differences from a standard PAG.
