source("R/zzz.R")
source("R/install-tetrad.R")

# Copy pasted from .onLoad in R/zzz.R.
# ---------------------------------
# Default Tetrad version
# ---------------------------------
if (is.null(getOption("causalDisco.tetrad.version"))) {
  options(causalDisco.tetrad.version = .default_tetrad_version)
}

version <- getOption("causalDisco.tetrad.version")

# ---------------------------------
# Versioned cache directory
# ---------------------------------
cache_subdir <- paste0("causalDisco/tetrad_v", version)

causalDisco$cache_dir <- tools::R_user_dir(cache_subdir, which = 'cache')
if (!dir.exists(causalDisco$cache_dir)) {
  dir.create(causalDisco$cache_dir, recursive = TRUE)
}

options(causalDisco.tetrad_cache = causalDisco$cache_dir)

install_tetrad()
verify_tetrad()
