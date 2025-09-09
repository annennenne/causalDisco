# This file defines a custom roxygen2 tag `@r6example` to include R6 class
# examples from external files. To use it, you need to define it in your own
# namespace rather than exported in this package.

roxy_tag_parse.roxy_tag_R6example <- function(x) {
  val <- as.character(x$raw)
  val <- sub("\\r?\\n.*$", "", val) # first line only
  x$val <- trimws(val)
  x
}

roxy_tag_rd.roxy_tag_R6example <- function(x, base_path, env) {
  val <- x$val
  if (grepl("^['\"].*['\"]$", val)) val <- sub("^['\"]|['\"]$", "", val)
  if (!nzchar(val)) {
    return()
  }

  pkg_root <-
    if (is.character(base_path) && length(base_path) == 1 && nzchar(base_path)) {
      base_path
    } else if (!is.null(attr(env, "path"))) {
      attr(env, "path")
    } else {
      getwd()
    }

  path <- file.path(pkg_root, val)
  if (!file.exists(path)) {
    roxygen2:::warn_roxy_tag(x, sprintf("{.path %s} doesn't exist", path))
    return()
  }

  code <- readLines(path, warn = FALSE)
  roxygen2::rd_section("examples", roxygen2::escape_examples(code))
}
