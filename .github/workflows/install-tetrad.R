# Ensure Tetrad is installed for workflows.
# - Not done in setup.R, since rude to download on users' machines without asking. Instead, we check for Tetrad
# and print instructions if not found.

# Old rude version which doesn't ask for confirmation. We still use this for GitHub Actions.
install_tetrad_rude <- function(
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
  options(TETRAD_DIR = dir)
  message("Tetrad directory set for this session: ", getOption("TETRAD_DIR"))

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
        message(
          "TETRAD_DIR already exists in .Renviron, update manually if needed"
        )
      }
    } else {
      write(line, file = renviron)
      message("Created .Renviron and added TETRAD_DIR")
    }
  }

  invisible(dest_file)
}

source("R/zzz.R")
source("R/install-tetrad.R")
install_tetrad_rude(version = .default_tetrad_version)
verify_tetrad(version = .default_tetrad_version)
