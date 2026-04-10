#' @rdname iamb-family
#' @export
iamb <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "iamb",
    engine = engine,
    engine_fns = list(
      bnlearn = function(...) {
        make_runner(engine = "bnlearn", alg = "iamb", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}

#' @rdname iamb-family
#' @export
iamb_fdr <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "iamb_fdr",
    engine = engine,
    engine_fns = list(
      bnlearn = function(...) {
        make_runner(engine = "bnlearn", alg = "iamb_fdr", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}

#' @rdname iamb-family
#' @export
fast_iamb <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "fast_iamb",
    engine = engine,
    engine_fns = list(
      bnlearn = function(...) {
        make_runner(engine = "bnlearn", alg = "fast_iamb", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}

#' @rdname iamb-family
#' @export
inter_iamb <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "inter_iamb",
    engine = engine,
    engine_fns = list(
      bnlearn = function(...) {
        make_runner(engine = "bnlearn", alg = "inter_iamb", ...)
      }
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}
