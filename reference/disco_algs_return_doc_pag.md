# Causal Discovery Algorithm Return Value PAG

Causal Discovery Algorithm Return Value PAG

## Value

A function that takes a single argument `data` (a data frame). When
called, this function returns a list containing:

- `knowledge` A `Knowledge` object with the background knowledge used in
  the causal discovery algorithm. See
  [`knowledge()`](https://disco-coders.github.io/causalDisco/reference/knowledge.md)
  for how to construct it.

- `caugi` A [`caugi::caugi`](https://caugi.org/reference/caugi.html)
  object representing the learned causal graph. This graph is a PAG
  (Partial Ancestral Graph), but since PAGs are not yet natively
  supported in `caugi`, it is currently stored with class `UNKNOWN`.
