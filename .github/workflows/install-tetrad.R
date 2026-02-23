source("R/zzz.R")
source("R/install-tetrad.R")

# Install Tetrad to package cache
install_tetrad(
  version = .default_tetrad_version,
  force = FALSE,
  quiet = FALSE
)

verify_tetrad(version = .default_tetrad_version)
