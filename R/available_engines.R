#' @export
available_engines <- function(method) {
  available <- attr(method, "engines")
  if (is.null(available)) {
    stop("No engine information available.")
  }
  method_name <- deparse(substitute(method))
  numbered_engines <- paste0(seq_along(available), ". ", available)
  cat(cat("Available engines for ", method_name, ":", sep = ""), paste(numbered_engines, collapse = "\n"), sep = "\n")
}
