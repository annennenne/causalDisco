###############################################################################################################
# Miscellaneous functions #####################################################################################
###############################################################################################################

#' Convert adjacency matrix to graphNEL object
#'
#' @param amat An adjacency matrix.
#'
#' @return A \code{graphNEL} object, see  [graph::graphNEL-class].
#'
#' @keywords internal
#' @noRd
as.graphNEL <- function(amat) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "methods"
    ),
    function_name = "as.graphNEL"
  )

  this_class <- class(amat)
  if ("tamat" %in% this_class) {
    class(amat) <- "matrix"
  }
  methods::as(t(amat), "graphNEL")
}

#' Translate user-defined test function to bnlearn format
#'
#' Takes a function `f` that has the signature `function(x, y, conditioning_set, suff_stat, args)`
#' (where `args` is optional) and returns a wrapper function that can be used as a custom test in bnlearn, which
#' requires the signature `function(x, y, z, data, args)`, and must return a numeric vector of length 2 containing the
#' test statistic (not used) and the p-value (must be 2nd argument).
#'
#' @param f A user-defined test function with signature `function(x, y, conditioning_set, suff_stat, args)`.
#' @return A wrapper function that can be used as a custom test in bnlearn.
#' @keywords internal
#' @noRd
translate_custom_test_to_bnlearn <- function(f) {
  fmls <- formals(f)
  has_args <- "args" %in% names(fmls)

  wrapper <- function(x, y, z, data, args = NULL) {
    # Prepare mapping: bnlearn names -> possible user names
    arg_map <- list(
      x = x,
      y = y,
      conditioning_set = z,
      z = z,
      suff_stat = data,
      data = data
    )

    # Only keep arguments that exist in user's function
    call_list <- arg_map[names(arg_map) %in% names(fmls)]

    if (has_args) {
      call_list$args <- args
    }

    result <- do.call(f, call_list)

    # Ensure result is length-2 numeric vector: c(statistic, p-value)
    if (length(result) == 1) {
      result <- c(NA, result) # inject dummy statistic
    } else if (length(result) != 2) {
      stop(
        "User-defined test must return a single numeric (p-value) or a numeric vector of length 2"
      )
    }

    result
  }

  wrapper
}
