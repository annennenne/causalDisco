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

#' Rename function arguments
#'
#' @param f A function whose arguments are to be renamed.
#' @param mapping A named character vector, where names are old argument names and values are new argument names.
#' @keywords internal
#' @noRd
rename_args <- function(f, mapping) {
  fmls <- formals(f)

  # mapping: named character vector, old -> new
  for (old in names(mapping)) {
    if (old %in% names(fmls)) {
      names(fmls)[names(fmls) == old] <- mapping[[old]]
    }
  }

  formals(f) <- fmls
  f
}
