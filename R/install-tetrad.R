# -------------------------------
# Get Tetrad directory
# -------------------------------
get_TETRAD_DIR <- function() {
  # Check R option
  path <- getOption("TETRAD_DIR")
  if (!is.null(path)) {
    return(path)
  }

  # Check environment variable
  path <- Sys.getenv("TETRAD_DIR")
  if (nzchar(path) && dir.exists(path)) {
    options(TETRAD_DIR = path)
    return(path)
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
  TETRAD_DIR <- get_TETRAD_DIR()

  # Default output helper
  create_output <- function(
    installed,
    version = NULL,
    java_ok = NA,
    java_version = NULL,
    message
  ) {
    list(
      installed = installed,
      version = version,
      java_ok = java_ok,
      java_version = java_version,
      message = message
    )
  }

  # ---- Step 1: Check Java first ----
  java_version <- get_java_version()

  if (is.null(java_version) || is.na(java_version)) {
    return(create_output(
      installed = FALSE,
      version = NULL,
      java_ok = FALSE,
      java_version = NULL,
      message = "Java was not found. Please install Java >= 21 using install_java()."
    ))
  }

  java_major <- as.integer(sub("\\..*", "", java_version))
  java_ok <- !is.na(java_major) && java_major >= 21

  if (!java_ok) {
    return(create_output(
      installed = FALSE,
      version = NULL,
      java_ok = FALSE,
      java_version = java_version,
      message = paste0(
        "Java >= 21 is required but found version ",
        java_version,
        ". Please update Java or run install_java()."
      )
    ))
  }

  # ---- Step 2: Check Tetrad directory / JAR ----
  if (is.null(TETRAD_DIR)) {
    return(create_output(
      installed = FALSE,
      version = NULL,
      java_ok = TRUE,
      java_version = java_version,
      message = "Tetrad not found. Call `install_tetrad()` to install."
    ))
  }

  gui_jar <- file.path(
    TETRAD_DIR,
    paste0("tetrad-gui-", version, "-launch.jar")
  )

  if (!file.exists(gui_jar)) {
    return(create_output(
      installed = FALSE,
      version = NULL,
      java_ok = TRUE,
      java_version = java_version,
      message = paste0(
        "Tetrad version ",
        version,
        " not found. Please install it using `install_tetrad()`."
      )
    ))
  }

  # ---- All OK ----
  msg <- paste0(
    "Tetrad found (version ",
    version,
    "). ",
    "Java version ",
    java_version,
    " is OK."
  )

  create_output(
    installed = TRUE,
    version = version,
    java_ok = TRUE,
    java_version = java_version,
    message = msg
  )
}

#' Install Tetrad GUI
#'
#' @description
#' Downloads and installs the Tetrad GUI JAR file for a specified version
#' into a user-specified directory. Configures the R session to know the
#' installation location via the `TETRAD_DIR` option.
#'
#' This function asks the user to confirm the installation directory
#' interactively, ensures the directory exists, and downloads the JAR
#' only if it’s missing or `force = TRUE`.
#'
#' @param version Character; the Tetrad version to install. Default is
#'   `getOption("causalDisco.tetrad.version")`.
#' @param dir Character; the directory where the JAR should be installed.
#'   Default is `"~/tetrad"`. The function will create this directory if it
#'   does not exist. The user will be prompted to confirm the location.
#' @param force Logical; if `TRUE`, forces re-download even if the JAR already
#'   exists. Default is `FALSE`.
#' @param verbose Logical; if `TRUE`, shows download progress. Default is `FALSE`.
#'
#' @return Invisibly returns the full path to the installed Tetrad JAR.
#'
#' @examples
#' \dontrun{
#' # Install default version in default directory
#' install_tetrad()
#'
#' # Install a specific version and force re-download
#' install_tetrad(version = "7.2.0", force = TRUE)
#'
#' # Install with verbose messages
#' install_tetrad(verbose = TRUE)
#' }
#'
#' @export
install_tetrad <- function(
  version = getOption("causalDisco.tetrad.version"),
  dir = NULL,
  force = FALSE,
  verbose = FALSE
) {
  quiet <- !verbose

  # ------------------------
  # Determine installation directory
  # ------------------------
  if (is.null(dir)) {
    dir <- "~/tetrad"
  }

  # Confirm installation interactively
  if (!interactive()) {
    stop(
      "Tetrad installation requires an interactive session.\n",
      "Either download Tetrad manually, or run this function in an interactive R session.",
      call. = FALSE
    )
  }

  dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
  message("\nTetrad will be installed to:\n  ", dir, "\n")
  ok <- utils::askYesNo("Do you want to continue?")
  if (!isTRUE(ok)) {
    stop("Installation cancelled.", call. = FALSE)
  }

  # Create directory if missing
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  # ------------------------
  # Construct URL + destination
  # ------------------------
  base_url <- "https://repo1.maven.org/maven2/io/github/cmu-phil/tetrad-gui"
  jar_name <- paste0("tetrad-gui-", version, "-launch.jar")
  url <- paste0(base_url, "/", version, "/", jar_name)
  dest_file <- file.path(dir, jar_name)

  need_download <- force || !file.exists(dest_file)

  # ------------------------
  # Download JAR if needed
  # ------------------------
  if (need_download) {
    msg_prefix <- if (force) "Re-downloading" else "Downloading"
    message(msg_prefix, " Tetrad ", version, "...")

    archive <- tempfile(fileext = ".jar")
    on.exit(unlink(archive), add = TRUE)

    tryCatch(
      {
        utils::download.file(
          url,
          destfile = archive,
          mode = "wb",
          quiet = quiet
        )
        file.copy(archive, dest_file, overwrite = TRUE)
      },
      error = function(e) {
        stop("Failed to download Tetrad: ", e$message)
      }
    )

    message("Downloaded to: ", dest_file)
  } else {
    message("Tetrad already exists at: ", dest_file)
  }

  options(TETRAD_DIR = dir)

  tetrad_line <- paste0("TETRAD_DIR='", dir, "'")

  cli::cli_bullets(c(
    "i" = "Setting {cli::col_green('TETRAD_DIR')} for current session to:",
    " " = "{cli::col_blue(dir)}",
    "i" = "To make this change permanent, set {cli::col_green('TETRAD_DIR')} in your .Renviron file.",
    " " = "Run  {.code usethis::edit_r_environ()} to open your .Renviron file, and then add the following line:",
    " " = "{cli::col_blue(tetrad_line)}"
  ))

  invisible(dest_file)
}
