#' @title Default heap size for Java Virtual Machine
#'
#' @description
#' This function retrieves the default heap size for the Java Virtual Machine.
#' The standard size is 2 gigabytes.
#'
#' @example inst/roxygen-examples/default_heap_example.R
#' @noRd
#' @keywords internal
default_heap <- function() {
  heap <- getOption(
    "java.heap.size",
    Sys.getenv("JAVA_HEAP_SIZE", unset = NA)
  )

  if (is.na(heap)) "2g" else heap
}

# nocov start

#' @title Read a line from the console
#'
#' @description
#' Only written so we can mock for testing
#'
#' @example inst/roxygen-examples/dot-read_line_example.R
#' @noRd
#' @keywords internal
.read_line <- function(prompt) {
  readline(prompt)
}

# nocov end

#' @title Ask for Java heap size
#'
#' @description
#' This function prompts the user to specify the Java heap size if the heap
#' size is not set in the options or environment variables. Is only used in
#' interactive sessions.
#'
#' @example inst/roxygen-examples/ask_heap_size_example.R
#' @noRd
#' @keywords internal
ask_heap_size <- function() {
  prompt <- paste(
    "How many GB should the Java heap use?",
    "Press <Return> for the default (2):"
  )
  repeat {
    answer <- .read_line(prompt)
    if (!nzchar(answer)) {
      return("2g")
    }
    if (grepl("^[1-9][0-9]*$", answer)) {
      return(paste0(answer, "g"))
    }
    message("Please enter a positive integer such as 4, 8, 16, etc.")
  }
}

#' @title Find Tetrad GUI Launcher JAR
#'
#' @description
#' `find_tetrad_jar()` searches for the Tetrad GUI launcher JAR file for a
#' specific version in a given directory. By default, it looks in the directory
#' set by the `tetrad.dir` option or the `TETRAD_DIR` environment variable.
#'
#' @param version Character string specifying the Tetrad version to search for.
#' Defaults to the package option `causalDisco.tetrad.version`.
#'   You can override this by setting, for example:
#'   `options(causalDisco.tetrad.version = "7.6.8")` **before loading the package**.
#' @param dir Character string specifying the directory to search. Defaults to the value of
#'   `getOption("tetrad.dir", Sys.getenv("TETRAD_DIR", ""))`.
#'
#' @return A character vector of length 1 containing the path to the Tetrad GUI launcher JAR.
#'   If the file is not found, a warning is issued and an empty vector is returned.
#'
#' @examples
#' gui_jar <- find_tetrad_jar()
#' print(gui_jar)
#'
#' @noRd
#' @keywords internal
find_tetrad_jar <- function(version = getOption("causalDisco.tetrad.version"),
                            dir = getOption("tetrad.dir", Sys.getenv("TETRAD_DIR", ""))) {
  # Check that directory exists
  if (!nzchar(dir) || !dir.exists(dir)) {
    stop(paste0(
      "Tetrad directory not found. Please install Tetrad or set the ",
      "TETRAD_DIR environment variable or tetrad.dir option."
    ))
  }

  # Build expected filename
  jar_name <- paste0("tetrad-gui-", version, "-launch.jar")
  jar_path <- file.path(dir, jar_name)

  # Check if the file exists
  if (!file.exists(jar_path)) {
    warning("Tetrad GUI launcher JAR not found: ", jar_path)
    return(character(0))
  }

  return(jar_path)
}


#' @title Initialize Java Virtual Machine for causalDisco
#'
#' @description
#' This function initializes the Java Virtual Machine (JVM) for the causalDisco
#' package. It sets the heap size and classpath based on the Tetrad JAR files
#' found. If the JVM is already initialized, it adds the JARs to the classpath.
#' @param heap A string specifying the heap size for the JVM. "2g" for 2
#'  gigabytes.
#'
#' @example inst/roxygen-examples/init_java_example.R
#' @noRd
#' @keywords internal
init_java <- function(heap = default_heap()) {
  .check_if_pkgs_are_installed(
    pkgs = "rJava",
    function_name = "init_java"
  )

  jar <- find_tetrad_jar()
  if (!length(jar)) {
    stop(
      "No Tetrad JAR found for version ",
      getOption("causalDisco.tetrad.version"),
      " in ", system.file("java", package = "causalDisco")
    )
  }

  if (rJava::.jniInitialized) {
    if (!(jar %in% rJava::.jclassPath())) {
      rJava::.jaddClassPath(jar)
    }
  } else {
    rJava::.jinit(parameters = paste0("-Xmx", heap), classpath = jar)
  }
}

#' @title Parse heap size string
#'
#' @description
#' This function parses a heap size string and returns the size in gigabytes.
#'
#' @param x A string representing the heap size, e.g., "4g" or "4096m".
#' @returns A numeric value representing the heap size in gigabytes.
#'
#' @example inst/roxygen-examples/parse_heap_gb_example.R
#' @noRd
#' @keywords internal
parse_heap_gb <- function(x) {
  stopifnot(length(x) == 1)

  x <- tolower(trimws(as.character(x)))

  if (grepl("^\\d+g(b)?$", x)) {
    return(as.double(sub("g(b)?$", "", x)))
  }

  if (grepl("^\\d+m(b)?$", x)) {
    return(as.double(sub("m(b)?$", "", x)) / 1024)
  }

  stop(
    "Heap string \"", x, "\" not recognised. ",
    "Specify a whole number followed by 'g' (gigabytes) ",
    "or 'm' (megabytes), e.g. \"4g\" or \"4096m\"."
  )
}

#' @title Get current Java heap size in gigabytes
#'
#' @description
#' This function retrieves the current Java heap size in gigabytes (based on
#' rough calculation).
#'
#' @returns A numeric value representing the current heap size in gigabytes.
#'
#' @example inst/roxygen-examples/current_heap_gb_example.R
#' @noRd
#' @keywords internal
current_heap_gb <- function() {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava"
    ),
    function_name = "current_heap_gb"
  )
  rt <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
  (rJava::.jcall(rt, "J", "maxMemory") / 1e9) |> round()
}

#' @title Check if the session is interactive
#'
#' @description
#' This function checks if the current R session is interactive. Only written
#' for mocking.
#'
#' @returns A logical value indicating whether the session is interactive.
#' @example inst/roxygen-examples/is_interactive_example.R
#' @noRd
#' @keywords internal
is_interactive <- function() {
  interactive()
}

#' @title Check if required packages are installed
#'
#' @description
#' Used in functions and classes in causalDisco to check if required packages
#' are installed with a more informative error message.
#'
#' @param pkgs Character vector of package names to check.
#' @param function_name Name of the function requiring the packages (string).
#' @param class_name Name of the R6 class requiring the packages (string).
#'
#' @returns Invisibly returns TRUE if all packages are installed,
#'   otherwise stops or warns.
#'
#' @example inst/roxygen-examples/dot-check_if_pkgs_are_installed_example.R
#' @keywords internal
.check_if_pkgs_are_installed <- function(pkgs,
                                         function_name = NULL,
                                         class_name = NULL) {
  stopifnot(is.character(pkgs), length(pkgs) > 0)
  if (is.null(function_name) && is.null(class_name)) {
    stop("Either function_name or class_name must be provided for ",
      ".check_if_pkgs_are_installed()",
      call. = FALSE
    )
  }
  not_installed <- pkgs[!vapply(
    pkgs,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )]

  if (!is.null(function_name)) {
    if (length(not_installed) > 0) {
      msg <- paste0(
        "The following packages are required for `", function_name,
        "()` but are not installed: \n       [",
        paste(not_installed, collapse = ", "),
        "].\n       Please install them with install.packages()."
      )
      stop(msg, call. = FALSE)
    }
  } else if (!is.null(class_name)) {
    if (length(not_installed) > 0) {
      msg <- paste0(
        "The following packages are required for the R6 class `", class_name,
        "` but are not installed: \n       [",
        paste(not_installed, collapse = ", "),
        "].\n       Please install them with install.packages()."
      )
      stop(msg, call. = FALSE)
    }
  }
  invisible(TRUE)
}
