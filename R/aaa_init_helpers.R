#' @title Default heap size for Java Virtual Machine
#'
#' @description
#' This function retrieves the default heap size for the Java Virtual Machine.
#' The standard size is 2 gigabytes.
#'
#' @example inst/roxygen-examples/default_heap_example.R
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

#' @title Find Tetrad .jar files
#'
#' @description
#' This function searches for Tetrad JAR files in the package's Java directory.
#'
#' @returns A character vector of file paths to the JAR files.
#'
#' @example inst/roxygen-examples/find_tetrad_jars_example.R
#' @keywords internal
find_tetrad_jars <- function() {
  jar_dir <- system.file("java", package = "causalDisco")
  list.files(jar_dir, pattern = "\\.jar$", full.names = TRUE)
}

#' @title Initialize Java Virtual Machine for causalDisco
#'
#' @description
#' This function initializes the Java Virtual Machine (JVM) for the causalDisco
#' package. It sets the heap size and classpath based on the Tetrad JAR files
#' found. If the JVM is already initialized, it adds the JARs to the classpath.
#'
#' @param heap A string specifying the heap size for the JVM. "2g" for 2
#'  gigabytes.
#'
#' @example inst/roxygen-examples/init_java_example.R
#' @keywords internal
init_java <- function(heap = default_heap()) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rJava"
    ),
    function_name = "init_java"
  )
  jars <- find_tetrad_jars()
  if (!length(jars)) {
    stop(
      "No Tetrad JARs found in ",
      system.file("java", package = "causalDisco")
    )
  }

  if (rJava::.jniInitialized) {
    rJava::.jaddClassPath(setdiff(jars, rJava::.jclassPath()))
  } else {
    rJava::.jinit(parameters = paste0("-Xmx", heap), classpath = jars)
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
