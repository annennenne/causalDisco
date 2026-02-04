# Ensures Tetrad is installed for when running it on GitHub Actions
# Move this to GitHub Actions workflow? Sometimes Tetrad runs out of memory (rare though) which
# would break tests ran on CRAN.
# Could keep this with skip_on_cran() so it auto downloads when a user runs tests locally?
# (Note GA runs as if on CRAN regarding skip_on_cran())
skip_on_cran()
tetrad_installed <- check_tetrad_install()$installed
if (!tetrad_installed) {
  install_tetrad()
}
