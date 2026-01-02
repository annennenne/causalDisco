#' Default Tetrad version used in the package
#' @keywords internal
#' @noRd
.default_tetrad_version <- "7.6.8"

.onLoad <- function(...) {
  # Only set heap options if rJava is installed
  if (requireNamespace("rJava", quietly = TRUE)) {
    if (is.null(getOption("java.heap.size"))) {
      options(java.heap.size = default_heap())
    }
  }

  if (is.null(getOption("causalDisco.tetrad.version"))) {
    options(causalDisco.tetrad.version = .default_tetrad_version)
  }

  S7::methods_register()
}

.onAttach <- function(...) {
  has_rJava <- requireNamespace("rJava", quietly = TRUE) # nolint: object_name_linter.

  # If rJava is NOT installed, give a simple message and exit
  if (!has_rJava) {
    packageStartupMessage(
      "causalDisco: rJava is not installed. Java-based features are disabled.\n",
      "Install rJava and a Java JDK if you want to use Tetrad functionality."
    )
    return()
  }

  if (
    is_interactive() &&
      is.null(getOption("java.heap.size")) &&
      !nzchar(Sys.getenv("JAVA_HEAP_SIZE", unset = ""))
  ) {
    options(java.heap.size = ask_heap_size())
  }

  raw_heap <- getOption("java.heap.size", default_heap())
  heap_gb <- parse_heap_gb(raw_heap)
  options(java.heap.size = paste0(heap_gb, "g"))

  # Check Tetrad installation
  tetrad_status <- check_tetrad_install()
  tetrad_version <- ifelse(
    tetrad_status$installed,
    tetrad_status$version,
    "not installed"
  )

  current_heap_size <- NA
  java_initialized <- FALSE

  if (tetrad_status$installed) {
    try(
      {
        init_java(heap = paste0(heap_gb, "g"))
        current_heap_size <- current_heap_gb()
        java_initialized <- TRUE
      },
      silent = TRUE
    )
  }

  # Build startup message
  msg <- paste0(
    "causalDisco startup:\n",
    "  Java heap size requested: ",
    round(heap_gb, 2),
    " GB\n",
    "  Tetrad version: ",
    tetrad_version,
    "\n"
  )

  if (java_initialized) {
    if (abs(current_heap_size - heap_gb) > 0.1) {
      msg <- paste0(
        msg,
        "  WARNING: Java heap is ",
        current_heap_size,
        " GB but you requested ",
        heap_gb,
        " GB. Restart R to change the heap.\n"
      )
    } else {
      msg <- paste0(
        msg,
        "  Java successfully initialized with ",
        current_heap_size,
        " GB.\n"
      )
    }
  } else if (tetrad_status$installed) {
    msg <- paste0(
      msg,
      "  WARNING: Java initialization failed. Check your Java setup.\n"
    )
  } else {
    msg <- paste0(
      msg,
      "  Tetrad is not installed. Run install_tetrad() to install it.\n"
    )
  }

  msg <- paste0(
    msg,
    "  To change heap size, set options(java.heap.size = 'Ng') or ",
    "Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading.\n",
    "  Restart R to apply changes.\n"
  )

  packageStartupMessage(msg)
}
