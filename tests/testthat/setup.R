# Ensure Tetrad is installed for local testing.
# - Skip Tetrad tests on CRAN to avoid rare memory issues that could cause false negatives.
# - GitHub Actions workflow uses .github/workflows/install-tetrad.R.
skip_on_cran()
tetrad_installed <- check_tetrad_install()$installed
if (!tetrad_installed) {
  install_tetrad()
}
