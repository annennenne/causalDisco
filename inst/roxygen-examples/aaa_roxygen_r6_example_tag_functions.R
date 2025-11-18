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

write_roxygen_example_skeletons <- function(file_path) {
  stopifnot(length(file_path) == 1, file.exists(file_path))
  # find package root by walking up until we see DESCRIPTION
  find_pkg_root <- function(start) {
    cur <- normalizePath(start, winslash = "/", mustWork = TRUE)
    repeat {
      if (file.exists(file.path(cur, "DESCRIPTION"))) {
        return(cur)
      }
      parent <- dirname(cur)
      if (identical(parent, cur)) break
      cur <- parent
    }
    stop("Could not find a package root (no DESCRIPTION found) starting from: ", start, call. = FALSE)
  }
  # extract top-level function names assigned via <- or =
  find_functions <- function(path) {
    exprs <- parse(file = path, keep.source = FALSE)
    out <- character()
    for (e in exprs) {
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
        lhs <- e[[2]]
        rhs <- e[[3]]
        if (is.symbol(lhs) && is.call(rhs) && identical(rhs[[1]], as.name("function"))) {
          out <- c(out, as.character(lhs))
        }
      }
    }
    unique(out)
  }
  # turn a function name into a filename base
  # - if it starts with ".", prefix "dot-" and drop the leading "."
  # - replace any characters that are unsafe for filenames with "-"
  to_filename_base <- function(fun) {
    base <- if (startsWith(fun, ".")) paste0("dot-", substring(fun, 2L)) else fun
    # allow letters, numbers, dot, underscore, and hyphen; replace others with "-"
    gsub("[^A-Za-z0-9._-]", "-", base)
  }
  pkg_root <- find_pkg_root(dirname(file_path))
  out_dir <- file.path(pkg_root, "inst", "roxygen-examples")
  funs <- find_functions(file_path)
  if (length(funs) == 0) {
    if (!requireNamespace("tibble", quietly = TRUE)) {
      return(
        structure(
          list(
            function_name = character(),
            path = character()
          ),
          class = "data.frame",
          row.names = integer()
        )
      )
    }
    return(tibble::tibble(function_name = character(), path = character()))
  }
  file_bases <- vapply(funs, to_filename_base, character(1))
  targets <- file.path(out_dir, paste0(file_bases, "_example.R"))
  # fail fast if any exist
  existing <- targets[file.exists(targets)]
  if (length(existing) > 0) {
    stop(
      "Example file(s) already exist; no files were created. Existing:\n  - ",
      paste(normalizePath(existing, winslash = "/"), collapse = "\n  - "),
      call. = FALSE
    )
  }
  # ensure directory exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- vapply(targets, function(p) {
    # create empty file
    file.create(p, showWarnings = FALSE)
  }, logical(1))
  if (!all(ok)) {
    # try to clean up any that were created if partial failure
    created <- targets[ok]
    unlink(created, recursive = FALSE, force = TRUE)
    stop("Failed to create one or more files in ", out_dir, call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    df <- data.frame(
      function_name = unname(funs),
      path = normalizePath(targets, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
    return(df)
  }
  tibble::tibble(
    function_name = unname(funs),
    path = normalizePath(targets, winslash = "/", mustWork = FALSE)
  )
}
