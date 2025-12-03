# Ensures Tetrad is installed for when running it on GitHub Actions
# Look into modifying the workflows to cache the Tetrad installation
tetrad_installed <- check_tetrad_install()$installed
if (!tetrad_installed) {
  install_tetrad()
}
