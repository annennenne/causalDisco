# Ensure Tetrad is installed for local testing.
# - Skip Tetrad tests on CRAN (avoid installing Java and Tetrad on CRAN).
# This also avoids rare memory issues on Tetrad (only observed on 7.6.8 and not on current version though).
# rare memory issues (don't think the current version can though?)
# - GitHub Actions workflow uses .github/workflows/install-tetrad.R.

on_cran <- identical(Sys.getenv("NOT_CRAN"), "false")

if (!on_cran && FALSE) {
  status <- verify_tetrad()
  java_ok <- status$java_ok
  tetrad_installed <- status$installed
  if (!java_ok) {
    os <- get_os()
    if (os == "windows" || os == "darwin") {
      install_java()
    } else {
      message("Unsupported OS for automatic Java installation: ", os)
    }
  }
  if (!tetrad_installed) {
    install_tetrad()
  }
} else {
  message("Skipping Tetrad installation on CRAN")
}
