# Ensure Tetrad is installed for workflows.
# - Not done in setup.R, since that would install it on CRAN too (workflows runs as CRAN)
# to avoid rare memory issues that could cause false negatives.
source("R/zzz.R")
source("R/install-tetrad.R")
install_tetrad(version = .default_tetrad_version)
verify_tetrad(version = .default_tetrad_version)
