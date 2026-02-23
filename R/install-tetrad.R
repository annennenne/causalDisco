get_tetrad_dir <- function() {
  # user override
  env <- Sys.getenv("TETRAD_DIR", "")
  if (nzchar(env) && dir.exists(env)) {
    return(env)
  }

  # package cache (default)
  cache <- getOption("causalDisco.tetrad_cache")
  if (!is.null(cache) && dir.exists(cache)) {
    return(cache)
  }

  NULL
}

#' @title Get Java Version
#' @description Retrieves the installed Java version.
#' @return A character string representing the Java version, or NA if Java is not found.
#' @keywords internal
#' @noRd
get_java_version <- function() {
  java <- Sys.which("java")

  # If java not found, try JAVA_HOME/bin
  if (!nzchar(java)) {
    java_home <- Sys.getenv("JAVA_HOME", "")
    java_bin <- file.path(java_home, "bin", "java.exe")
    if (file.exists(java_bin)) {
      java <- normalizePath(java_bin, winslash = "/")
    }
  }

  if (!nzchar(java) || !file.exists(java)) {
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
#' verify_tetrad()
#' @export
verify_tetrad <- function(
  version = getOption("causalDisco.tetrad.version")
) {
  tetrad_dir <- get_tetrad_dir()

  out <- function(installed, java_ok, java_version, message) {
    list(
      installed = installed,
      version = if (installed) version else NULL,
      java_ok = java_ok,
      java_version = java_version,
      message = message
    )
  }

  # ---- Java check ----
  java_version <- get_java_version()

  if (is.na(java_version)) {
    return(out(FALSE, FALSE, NULL, "Java not found. Install Java >= 21."))
  }

  java_major <- as.integer(sub("\\..*", "", java_version))
  java_ok <- !is.na(java_major) && java_major >= 21

  if (!java_ok) {
    return(out(
      FALSE,
      FALSE,
      java_version,
      paste0("Java >= 21 required (found ", java_version, ").")
    ))
  }

  # ---- Tetrad check ----
  if (is.null(tetrad_dir)) {
    return(out(
      FALSE,
      TRUE,
      java_version,
      "Tetrad not installed. Run install_tetrad()."
    ))
  }

  jar <- file.path(
    tetrad_dir,
    paste0("tetrad-gui-", version, "-launch.jar")
  )

  if (!file.exists(jar)) {
    return(out(
      FALSE,
      TRUE,
      java_version,
      "Tetrad not installed. Run install_tetrad()."
    ))
  }

  out(
    TRUE,
    TRUE,
    java_version,
    paste0("Tetrad version ", version, " is installed and ready to use.")
  )
}

#' Install Tetrad GUI
#'
#' @description
#' Downloads and installs the Tetrad GUI JAR file for a specified version
#' into a user-specified or default cache directory. The function ensures the
#' directory exists, downloads the JAR only if it is missing or if `force = TRUE`,
#' and verifies its checksum to ensure integrity.
#'
#' @param version Character; the Tetrad version to install. Default is
#'   `getOption("causalDisco.tetrad.version")`.
#' @param dir Character; the directory where the JAR should be installed. If
#'   `NULL` (default), the function uses the cache directory defined by
#'   `getOption("causalDisco.tetrad_cache")`. The directory will be created
#'   if it does not exist.
#' @param force Logical; if `TRUE`, forces re-download even if the JAR already
#'   exists. Default is `FALSE`.
#' @param quiet Logical; if `FALSE`, shows progress and messages about
#'   downloading and checksum verification. Default is `FALSE`.
#' @param temp_dir Logical; if `TRUE`, installs the JAR in a temporary directory
#'   instead of the cache. Default is `FALSE`.
#'
#' @return Invisibly returns the full path to the installed Tetrad JAR.
#'
#' @examples
#' \dontrun{
#' # Install default version in cache directory
#' install_tetrad()
#'
#' # Install a specific version and force re-download
#' install_tetrad(version = "7.6.10", force = TRUE)
#'
#' # Install in a temporary directory
#' install_tetrad(temp_dir = TRUE)
#'
#' # Install quietly (suppress messages)
#' install_tetrad(quiet = TRUE)
#' }
#'
#' @export
install_tetrad <- function(
  version = getOption("causalDisco.tetrad.version"),
  dir = NULL,
  force = FALSE,
  quiet = FALSE,
  temp_dir = FALSE
) {
  checkmate::assert_logical(force)
  checkmate::assert_logical(quiet)
  checkmate::assert_logical(temp_dir)

  old_options <- options()
  on.exit(options(old_options), add = TRUE)
  options(timeout = max(300, getOption("timeout")))

  # ------------------------
  # Determine install dir
  # ------------------------
  if (temp_dir) {
    dir <- tempdir()
  } else if (is.null(dir)) {
    dir <- getOption("causalDisco.tetrad_cache")
  }

  if (is.null(dir)) {
    if (!quiet) {
      message("Cache directory not initialized. Skipping Tetrad installation.")
    }
    return(NULL)
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  # ------------------------
  # Build URLs
  # ------------------------
  base <- "https://repo1.maven.org/maven2/io/github/cmu-phil/tetrad-gui"
  jar_name <- paste0("tetrad-gui-", version, "-launch.jar")
  jar_url <- paste0(base, "/", version, "/", jar_name)
  jar_path <- file.path(dir, jar_name)

  checksum_name <- paste0(jar_name, ".sha256")
  checksum_url <- paste0(base, "/", version, "/", checksum_name)
  checksum_path <- file.path(dir, checksum_name)

  # ------------------------
  # Download JAR if missing or forced
  # ------------------------
  download_ok <- TRUE
  if (!file.exists(jar_path) || force) {
    if (!quiet) {
      message("Downloading Tetrad ", version, " ...")
    }

    download_ok <- tryCatch(
      {
        utils::download.file(jar_url, jar_path, mode = "wb", quiet = quiet)
        utils::download.file(
          checksum_url,
          checksum_path,
          mode = "wb",
          quiet = quiet
        )
        TRUE
      },
      error = function(e) {
        if (!quiet) {
          message(
            "Unable to download Tetrad from ",
            jar_url,
            ". Check your internet connection or try later."
          )
        }
        FALSE
      },
      warning = function(w) {
        if (!quiet) {
          message(
            "Download warning: ",
            conditionMessage(w),
            ". Proceeding gracefully."
          )
        }
        TRUE
      }
    )

    if (!download_ok) return(NULL)
  } else if (!quiet) {
    message("Using cached Tetrad.")
  }

  # ------------------------
  # Verify checksum
  # ------------------------
  if (!file.exists(checksum_path) || !file.exists(jar_path)) {
    if (!quiet) {
      message("Tetrad files missing, skipping checksum verification.")
    }
    return(NULL)
  }

  if (!quiet) {
    message("Verifying checksum...")
  }

  expected <- tryCatch(
    trimws(readLines(checksum_path, warn = FALSE)[1]),
    error = function(e) NA_character_
  )
  actual <- tryCatch(
    digest::digest(file = jar_path, algo = "sha256"),
    error = function(e) NA_character_
  )

  if (
    is.na(expected) || is.na(actual) || tolower(actual) != tolower(expected)
  ) {
    if (!quiet) {
      message("Checksum verification failed. The file may be corrupted.")
    }
    return(NULL)
  }

  if (!quiet) {
    message("Checksum confirmed. Cached at: ", jar_path)
  }

  invisible(jar_path)
}
