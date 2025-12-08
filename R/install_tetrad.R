# -------------------------------
# Get Tetrad directory
# -------------------------------
get_tetrad_dir <- function() {
  # Check R option
  path <- getOption("tetrad.dir")
  if (!is.null(path)) {
    return(path)
  }

  # Check environment variable
  path <- Sys.getenv("TETRAD_DIR")
  if (nzchar(path) && dir.exists(path)) {
    options(tetrad.dir = path)
    return(path)
  }

  NULL
}

# -------------------------------
# Create output object with custom print
# -------------------------------
create_output <- function(installed, version = NULL, message) {
  obj <- list(
    installed = installed,
    version = version,
    message = message
  )
  class(obj) <- "tetrad_check"
  obj
}

#' @title Print Tetrad check result
#' @description Print method for tetrad_check objects.
#' @param x A tetrad_check object.
#' @param ... Additional arguments (not used).
#' @examples
#' causalDisco:::create_output(TRUE, "7.6.8", "Tetrad is installed.") |> print()
#' @export
print.tetrad_check <- function(x, ...) {
  cat("Installed:", x$installed, "\n")
  cat(paste0("Version: ", ifelse(is.null(x$version), "NULL", x$version)), "\n")
  cat("Message:", x$message, "\n")
}


#' @title Get Java Version
#' @description Retrieves the installed Java version.
#' @return A character string representing the Java version, or NA if Java is not found.
#' @keywords internal
#' @noRd
get_java_version <- function() {
  java <- Sys.which("java")
  if (!nzchar(java)) {
    return(NA_character_)
  }

  ver_lines <- tryCatch(
    system2(java, "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) NA_character_
  )

  if (is.na(ver_lines[1])) {
    return(NA_character_)
  }

  ver_line <- ver_lines[1]
  m <- regmatches(ver_line, regexpr('"[0-9._]+"', ver_line))
  if (length(m) == 0) {
    return(NA_character_)
  }

  gsub('"', "", m)
}


#' Check Tetrad Installation
#' @param version Character. The version of Tetrad to check.
#'  Default is the value of `getOption("causalDisco.tetrad.version")`.
#' @return A list with elements:
#'  - `installed`: Logical, whether Tetrad is installed.
#'  - `version`: Character or NULL, the installed version if found.
#'  - `java_ok`: Logical, whether Java >= 21.
#'  - `java_version`: Character, the installed Java version.
#'  - `message`: Character, a message describing the status.
#' @examples
#' check_tetrad_install()
#' @export
check_tetrad_install <- function(version = getOption("causalDisco.tetrad.version")) {
  tetrad_dir <- get_tetrad_dir()

  # Default output helper
  create_output <- function(installed, version = NULL, java_ok = NA, java_version = NULL, message) {
    list(
      installed = installed,
      version = version,
      java_ok = java_ok,
      java_version = java_version,
      message = message
    )
  }

  # Check Tetrad directory / JAR
  if (is.null(tetrad_dir)) {
    return(create_output(
      installed = FALSE,
      version = NULL,
      java_ok = NA,
      java_version = NULL,
      message = "Tetrad directory not configured. Call install_tetrad() to install it."
    ))
  }

  gui_jar <- file.path(tetrad_dir, paste0("tetrad-gui-", version, "-launch.jar"))

  if (!file.exists(gui_jar)) {
    return(create_output(
      installed = FALSE,
      version = NULL,
      java_ok = NA,
      java_version = NULL,
      message = paste0(
        "Tetrad version ", version, " not found. Please install it using install_tetrad()."
      )
    ))
  }

  java_version <- get_java_version()
  java_major <- as.integer(sub("\\..*", "", java_version))
  java_ok <- !is.na(java_major) && java_major >= 21

  msg <- paste0(
    "Tetrad found (version ", version, "). ",
    if (!java_ok) {
      paste0("Java >= 21 required but found ", java_version, ".")
    } else {
      paste0("Java version ", java_version, " is OK.")
    }
  )

  create_output(
    installed = TRUE,
    version = version,
    java_ok = java_ok,
    java_version = java_version,
    message = msg
  )
}

#' Install Tetrad GUI
#'
#' This function downloads and installs the Tetrad GUI JAR file to a specified directory.
#' It also sets the TETRAD_DIR environment variable for future R sessions.
#'
#' @template tetrad-version
#' @param dir Character. The directory to install Tetrad into. Default is "~/tetrad".
#' @param set_renviron Logical. Whether to set the TETRAD_DIR in .Renviron. Default is TRUE.
#' @param force Logical. Whether to force re-download if the file already exists. Default is FALSE.
#'
#' @return Invisible character string of the path to the downloaded JAR file.
#' @examples
#' \dontrun{
#' install_tetrad()
#' }
#' @export
install_tetrad <- function(
  version = getOption("causalDisco.tetrad.version"),
  dir = NULL,
  set_renviron = TRUE,
  force = FALSE
) {
  safe_download <- function(url, dest_file) {
    old_timeout <- getOption("timeout")
    options(timeout = max(300, old_timeout))
    on.exit(options(timeout = old_timeout), add = TRUE)

    utils::download.file(url, destfile = dest_file, mode = "wb")
  }

  # ------------------------
  # Determine target dir
  # ------------------------
  if (is.null(dir)) {
    dir <- file.path(path.expand("~"), "tetrad")
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  # Normalize path (works on macOS/Linux/Windows)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  # ------------------------
  # URL + destination
  # ------------------------
  base_url <- "https://repo1.maven.org/maven2/io/github/cmu-phil/tetrad-gui"
  jar_name <- paste0("tetrad-gui-", version, "-launch.jar")
  url <- paste0(base_url, "/", version, "/", jar_name)

  dest_file <- file.path(dir, jar_name)

  need_download <- force || !file.exists(dest_file)

  if (need_download) {
    msg_prefix <- if (force) "Re-downloading" else "Downloading"
    message(msg_prefix, " Tetrad ", version, "...")

    tryCatch(
      safe_download(url, dest_file),
      error = function(e) stop("Failed to download Tetrad: ", e$message)
    )

    message("Downloaded to: ", dest_file)
  } else {
    message("Tetrad already exists at: ", dest_file)
  }

  # ------------------------
  # Set session option
  # ------------------------
  options(tetrad.dir = dir)
  message("Tetrad directory set for this session: ", getOption("tetrad.dir"))

  # ------------------------
  # Persist in .Renviron
  # ------------------------
  if (set_renviron) {
    renviron <- file.path(path.expand("~"), ".Renviron")
    line <- paste0('TETRAD_DIR="', dir, '"')

    if (file.exists(renviron)) {
      content <- readLines(renviron, warn = FALSE)
      if (!any(grepl("^TETRAD_DIR=", content))) {
        write(line, file = renviron, append = TRUE)
        message("Added TETRAD_DIR to .Renviron")
      } else {
        message("TETRAD_DIR already exists in .Renviron, update manually if needed")
      }
    } else {
      write(line, file = renviron)
      message("Created .Renviron and added TETRAD_DIR")
    }
  }

  invisible(dest_file)
}
