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

  jdk_existing <- detect_jdk_folder(install_dir)

  # --- Already installed ---
  if (!is.null(jdk_existing) && !force) {
    jdk_bin <- file.path(jdk_existing, "bin")

    # Make Java available immediately in this R session
    Sys.setenv(PATH = paste0(jdk_bin, ";", Sys.getenv("PATH")))

    message("JDK already installed at: ", jdk_existing)
    message("Java is ready to use in this R session.")
    message("If you want Java available globally, restart R or your terminal.")

    return(invisible(jdk_existing))
  }

  # --- Force reinstall ---
  if (force && dir.exists(install_dir)) {
    unlink(install_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Download archive ---
  url <- jdk_download_url(arch, "windows")
  message("Downloading JDK from:\n  ", url)

  zipfile <- tempfile(fileext = ".zip")
  utils::download.file(url, zipfile, mode = "wb")

  utils::unzip(zipfile, exdir = install_dir)

  jdk_installed <- detect_jdk_folder(install_dir)
  if (is.null(jdk_installed)) stop("Extraction failed: no JDK folder found")

  jdk_bin <- file.path(jdk_installed, "bin")

  # Add to current R session's PATH
  Sys.setenv(PATH = paste0(jdk_bin, ";", Sys.getenv("PATH")))

  # --- Persistent PATH (user scope) ---
  user_path <- get_user_path()
  if (!grepl(jdk_bin, user_path, fixed = TRUE)) {
    system2("setx", c("PATH", shQuote(paste(user_path, jdk_bin, sep = ";"))))
    message("Added JDK to the user PATH.")
    message("Restart R or your terminal to make the change global.")
  }

  message("\nJava in this session:")
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

#' Install Temurin JDK 25
#'
#' @description
#' This function installs the Eclipse Temurin JDK 25 on the user's system. It works
#' on Windows and macOS operating systems. For Linux we assume the user can install
#' Java via their package manager.
#' The JDK is installed in the user's home directory.
#' @param force Logical; if TRUE, forces re-installation even if JDK is already installed.
#' @examples
#' \dontrun{
#' install_java()
#' }
#' @export
install_java <- function(force = FALSE) {
  os <- get_os()

  if (os == "windows") {
    install_java_windows(force = force)
  } else if (os == "darwin") {
    install_java_mac(force = force)
  } else {
    stop("Unsupported OS: ", os)
  }
}
