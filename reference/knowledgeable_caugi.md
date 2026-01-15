# A `caugi` with an attached `knowledge` object

This S3 class wraps `caugi` graph object and a `knowledge` object. It is
the output object of causal discovery methods used in `causalDisco`.

## Usage

``` r
knowledgeable_caugi(graph, kn = knowledge(), class = "PDAG")
```

## Arguments

- graph:

  A causal graph object

- kn:

  A `knowledge` object. Default is empty knowledge object.

- class:

  A string describing the graph class.

## Value

A `caugi` and a `knowledge` object in a list.

## Details

The conversion from any graph type to a `caugi` is handled by the
`caugi` package.

## See also

[`caugi::caugi()`](https://caugi.org/reference/caugi.html)
