#' @importFrom rJava .jniInitialized .jinit
.onLoad <- function(libname, pkgname) {
  # Initialize the JVM if it isn’t already running.
  # todo : how many gb?
  invisible(NULL)
}
