# ============================
#  Shared Helpers
# ============================

# Detect OS in normalized form
get_os <- function() {
  tolower(Sys.info()[["sysname"]])
}

# Detect CPU architecture and map to Adoptium API names
detect_arch <- function() {
  m <- tolower(Sys.info()[["machine"]])

  if (m %in% c("x86_64", "x86-64", "amd64")) {
    return("x64")
  }
  if (m %in% c("arm64", "aarch64")) {
    return("aarch64")
  }

  stop("Unsupported architecture: ", m)
}

# Build download URL for Temurin JDK 25
jdk_download_url <- function(arch, os) {
  paste0(
    "https://api.adoptium.net/v3/binary/latest/25/ga/",
    os,
    "/",
    arch,
    "/jdk/hotspot/normal/eclipse"
  )
}

# Detect extracted JDK folder (prefer longest)
detect_jdk_folder <- function(dir) {
  dirs <- list.dirs(dir, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0) {
    return(NULL)
  }
  dirs[order(nchar(dirs), decreasing = TRUE)][1]
}

get_java_home <- function() {
  # 1. Package option
  opt <- getOption("temurin.java_home", NULL)

  if (!is.null(opt) && dir.exists(opt)) {
    return(normalizePath(opt, winslash = "/", mustWork = TRUE))
  }

  # 2. System JAVA_HOME
  env <- Sys.getenv("JAVA_HOME", "")

  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = TRUE))
  }

  # 3. Default install location
  default <- path.expand("~/temurin25")

  if (dir.exists(default)) {
    jdk <- detect_jdk_folder(default)

    if (!is.null(jdk)) {
      if (get_os() == "darwin") {
        jdk <- file.path(jdk, "Contents/Home")
      }

      return(normalizePath(jdk, winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    "Java not found.\n\n",
    "Run temurin::install_java() or set manually:\n",
    "options(temurin.java_home = '/path/to/java')",
    call. = FALSE
  )
}

confirm_installation <- function(install_dir) {
  if (!interactive()) {
    stop(
      "Java installation requires an interactive session.\n",
      "Either download Java manually (e.g. Temurin from https://adoptium.net),\n",
      "or run this function in an interactive R session.",
      call. = FALSE
    )
  }

  install_dir <- normalizePath(
    path.expand(install_dir),
    winslash = "/",
    mustWork = FALSE
  )

  message("\nJava will be installed to:\n  ", install_dir, "\n")

  ok <- utils::askYesNo("Do you want to continue?")

  if (!isTRUE(ok)) {
    stop("Installation cancelled.", call. = FALSE)
  }

  invisible(install_dir)
}

#' Install Eclipse Temurin JDK
#'
#' @description
#' Installs the Eclipse Temurin JDK in the user's home directory (or a specified
#' location) and configures the environment so the JDK is immediately available
#' to the current R session.
#'
#' This function sets the `JAVA_HOME` environment variable, ensuring that packages
#' such as **rJava** can find and use the JDK without additional configuration.
#'
#' The function automatically handles downloads and extraction for **Windows** and
#' **macOS**, using the appropriate archive format. Linux is not supported, as
#' Java is typically installed via the system package manager.
#'
#' @details
#' The function requires an **interactive R session**. It will prompt the user to
#' confirm the installation directory using [utils::askYesNo()]. If the session
#' is non-interactive, the function will
#' stop with an error.
#'
#' @param install_dir Character; the directory where the JDK should be installed.
#'   Default is `"~/temurin"`. The function will create this directory if it does
#'   not exist. If a JDK is already present, it will be used unless `force = TRUE`,
#'   in which case it will be reinstalled.
#'
#' @param force Logical; if `TRUE`, forces reinstallation even if a JDK already
#'   exists in the specified directory. Default is `FALSE`.
#'
#' @param verbose Logical; if `TRUE`, shows download progress and messages.
#'   Default is `FALSE`.
#'
#' @return Invisibly returns the path to `JAVA_HOME` for the installed or detected JDK.
#'
#' @examples
#' \dontrun{
#' # Install with default directory
#' install_java()
#'
#' # Install in a custom directory and force reinstall
#' install_java(install_dir = "C:/Java/temurin", force = TRUE)
#'
#' # Install with verbose messages
#' install_java(verbose = TRUE)
#' }
#'
#' @export
install_java <- function(
  force = FALSE,
  install_dir = "~/temurin",
  verbose = FALSE
) {
  quiet <- !verbose
  platform <- get_os()

  # Expand ~ to full user path
  install_dir <- normalizePath(
    path.expand(install_dir),
    winslash = "/",
    mustWork = FALSE
  )

  install_dir <- confirm_installation(install_dir)
  arch <- detect_arch()
  jdk_existing <- detect_jdk_folder(install_dir)

  # Determine JAVA_HOME for mac vs windows
  compute_java_home <- function(jdk_path) {
    if (platform == "mac") {
      normalizePath(file.path(jdk_path, "Contents/Home"), winslash = "/")
    } else {
      normalizePath(jdk_path, winslash = "/")
    }
  }

  if (!is.null(jdk_existing) && !force) {
    java_home <- compute_java_home(jdk_existing)

    message(
      "Found existing Java installation:\n  ",
      java_home,
      "\n  Use force = TRUE to reinstall.\n"
    )

    options(temurin.java_home = java_home)
    Sys.setenv(JAVA_HOME = java_home)

    java_home_line <- paste0("JAVA_HOME='", java_home, "'")

    cli::cli_bullets(c(
      "i" = "Setting {cli::col_green('JAVA_HOME')} for current session to:",
      " " = "{cli::col_blue(java_home)}",
      "i" = "To make this change permanent, set {cli::col_green('JAVA_HOME')} in your .Renviron file.",
      " " = "Run  {.code usethis::edit_r_environ()} to open your .Renviron file, and then add the following line:",
      " " = "{cli::col_blue(java_home_line)}"
    ))

    return(invisible(java_home))
  }

  # Remove old installation if force = TRUE
  if (force && dir.exists(install_dir)) {
    unlink(install_dir, recursive = TRUE, force = TRUE)
  }

  dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)

  url <- jdk_download_url(arch, platform)
  message("Downloading Java...")

  # Download and extract based on platform
  if (platform == "windows") {
    archive <- tempfile(fileext = ".zip")
    on.exit(unlink(archive), add = TRUE)
    utils::download.file(url, archive, mode = "wb", quiet = quiet)
    utils::unzip(archive, exdir = install_dir)
  } else {
    archive <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(archive), add = TRUE)
    utils::download.file(url, archive, mode = "wb", quiet = quiet)
    utils::untar(archive, exdir = install_dir)
  }

  jdk_installed <- detect_jdk_folder(install_dir)
  if (is.null(jdk_installed)) {
    stop("Extraction failed.", call. = FALSE)
  }

  java_home <- compute_java_home(jdk_installed)

  options(temurin.java_home = java_home)
  Sys.setenv(JAVA_HOME = java_home)

  cli::cli_alert_success("Java installed successfully!")

  java_home_line <- paste0("JAVA_HOME='", java_home, "'")

  cli::cli_bullets(c(
    "i" = "Setting {cli::col_green('JAVA_HOME')} for current session to:",
    " " = "{cli::col_blue(java_home)}",
    "i" = "To make this change permanent, set {cli::col_green('JAVA_HOME')} in your .Renviron file.",
    " " = "Run  {.code usethis::edit_r_environ()} to open your .Renviron file, and then add the following line:",
    " " = "{cli::col_blue(java_home_line)}"
  ))

  invisible(java_home)
}
