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
    os, "/", arch, "/jdk/hotspot/normal/eclipse"
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


# ============================
#  Windows Installer
# ============================

install_java_windows <- function(force = FALSE) {
  arch <- detect_arch()
  install_dir <- path.expand("~/temurin25")

  # Read user PATH (registry)
  get_user_path <- function() {
    tryCatch(
      paste(system2(
        "powershell",
        args = c(
          "-NoProfile", "-Command",
          "[Environment]::GetEnvironmentVariable('Path','User')"
        ),
        stdout = TRUE
      ), collapse = ";"),
      error = function(e) Sys.getenv("PATH")
    )
  }

  # Read user JAVA_HOME (registry)
  get_user_java_home <- function() {
    tryCatch(
      system2(
        "powershell",
        args = c(
          "-NoProfile", "-Command",
          "[Environment]::GetEnvironmentVariable('JAVA_HOME','User')"
        ),
        stdout = TRUE
      ),
      error = function(e) Sys.getenv("JAVA_HOME")
    )
  }

  jdk_existing <- detect_jdk_folder(install_dir)

  # -------- Already installed --------
  if (!is.null(jdk_existing) && !force) {
    jdk_bin <- file.path(jdk_existing, "bin")

    # Set for this session
    Sys.setenv(JAVA_HOME = jdk_existing)
    Sys.setenv(PATH = paste0(jdk_bin, ";", Sys.getenv("PATH")))

    message("JDK already installed at: ", jdk_existing)
    message("JAVA_HOME set for this session: ", jdk_existing)
    return(invisible(jdk_existing))
  }

  # -------- Reinstall if forced --------
  if (force && dir.exists(install_dir)) {
    unlink(install_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)

  # -------- Download JDK --------
  url <- jdk_download_url(arch, "windows")
  message("Downloading JDK from:\n  ", url)

  zipfile <- tempfile(fileext = ".zip")
  utils::download.file(url, zipfile, mode = "wb")
  utils::unzip(zipfile, exdir = install_dir)

  jdk_installed <- detect_jdk_folder(install_dir)
  if (is.null(jdk_installed)) stop("Extraction failed: no JDK folder found")

  jdk_bin <- file.path(jdk_installed, "bin")

  # --- CURRENT SESSION ---
  Sys.setenv(JAVA_HOME = jdk_installed)
  Sys.setenv(PATH = paste0(jdk_bin, ";", Sys.getenv("PATH")))

  # --- Persistent PATH ---
  user_path <- get_user_path()
  if (!grepl(jdk_bin, user_path, fixed = TRUE)) {
    system2("setx", c("PATH", shQuote(paste(user_path, jdk_bin, sep = ";"))))
    message("Added JDK bin directory to the user PATH.")
  }

  # --- Persistent JAVA_HOME ---
  user_java_home <- get_user_java_home()
  if (!identical(
    normalizePath(user_java_home, winslash = "/"),
    normalizePath(jdk_installed, winslash = "/")
  )) {
    system2("setx", c("JAVA_HOME", shQuote(jdk_installed)))
    message("Set persistent JAVA_HOME.")
  }

  message("\nJava version check (current session):")
  message(system2("java", "-version", stdout = TRUE, stderr = TRUE))

  invisible(jdk_installed)
}


# ============================
#  macOS Installer
# ============================

install_java_mac <- function(force = FALSE) {
  arch <- detect_arch()
  install_dir <- path.expand("~/temurin25")

  jdk_existing <- detect_jdk_folder(install_dir)

  # --- Already installed ---
  if (!is.null(jdk_existing) && !force) {
    java_home <- file.path(jdk_existing, "Contents/Home")
    jdk_bin <- file.path(java_home, "bin")

    Sys.setenv(PATH = paste0(jdk_bin, ":", Sys.getenv("PATH")))

    message("JDK already installed at: ", jdk_existing)
    message("Java is ready to use in this R session.")
    return(invisible(jdk_existing))
  }

  # --- Force reinstall ---
  if (force && dir.exists(install_dir)) {
    unlink(install_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Download + extract ---
  url <- jdk_download_url(arch, "mac")
  message("Downloading JDK from:\n  ", url)

  tarfile <- tempfile(fileext = ".tar.gz")
  utils::download.file(url, tarfile, mode = "wb")

  utils::untar(tarfile, exdir = install_dir)

  jdk_installed <- detect_jdk_folder(install_dir)
  if (is.null(jdk_installed)) stop("Extraction failed: no JDK folder found")

  java_home <- file.path(jdk_installed, "Contents/Home")
  jdk_bin <- file.path(java_home, "bin")

  Sys.setenv(PATH = paste0(jdk_bin, ":", Sys.getenv("PATH")))

  message("\nJava in this session:")
  message(system2("java", "-version", stdout = TRUE, stderr = TRUE))

  invisible(jdk_installed)
}


# ============================
#  Unified Entry Point
# ============================

#' Install Eclipse Temurin JDK 25 (with JAVA_HOME configuration)
#'
#' @description
#' Installs the Eclipse Temurin JDK 25 in the user's home directory and configures
#' the environment so the JDK is immediately available to the current R session.
#'
#' On **Windows**, this function also sets the `JAVA_HOME` environment variable
#' (both for the current session and persistently using `setx`) to ensure that
#' packages such as **rJava** work without additional configuration.
#'
#' On **macOS**, the JDK is installed under `~/temurin25` and the current R
#' session's `JAVA_HOME` is updated automatically. macOS users may need to restart
#' their terminal or R session for system-wide detection.
#'
#' This helper function is intended for users who prefer an automated installation
#' or who find it inconvenient to manually download and install Java from the
#' Adoptium website (<https://adoptium.net/temurin/releases>). It ensures the JDK
#' is installed in a predictable location and that environment variables such as
#' `JAVA_HOME` are configured correctly for use in R, including with packages like
#' **rJava**.
#'
#' Linux is not supported by this helper, as Java is typically installed via the
#' system package manager.
#'
#' @param force Logical; if `TRUE`, forces reinstallation even if the JDK is
#'        already present.
#'
#' @examples
#' \dontrun{
#' install_java()
#' }
#'
#' @export
install_java <- function(force = FALSE) {
  os <- get_os()

  if (os == "windows") {
    install_java_windows(force = force)
  } else if (os == "darwin") {
    install_java_mac(force = force)
  } else {
    stop("Unsupported OS: ", os, call. = FALSE)
  }
}
