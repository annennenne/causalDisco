# Install Tetrad GUI

Downloads and installs the Tetrad GUI JAR file for a specified version
into a user-specified or default cache directory. The function ensures
the directory exists, downloads the JAR only if it is missing or if
`force = TRUE`, and verifies its checksum to ensure integrity.

## Usage

``` r
install_tetrad(
  version = getOption("causalDisco.tetrad.version"),
  dir = NULL,
  force = FALSE,
  quiet = FALSE,
  temp_dir = FALSE
)
```

## Arguments

- version:

  Character; the Tetrad version to install. Default is
  `getOption("causalDisco.tetrad.version")`.

- dir:

  Character; the directory where the JAR should be installed. If `NULL`
  (default), the function uses the cache directory defined by
  `getOption("causalDisco.tetrad_cache")`. The directory will be created
  if it does not exist.

- force:

  Logical; if `TRUE`, forces re-download even if the JAR already exists.
  Default is `FALSE`.

- quiet:

  Logical; if `FALSE`, shows progress and messages about downloading and
  checksum verification. Default is `FALSE`.

- temp_dir:

  Logical; if `TRUE`, installs the JAR in a temporary directory instead
  of the cache. Default is `FALSE`.

## Value

Invisibly returns the full path to the installed Tetrad JAR.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install default version in cache directory
install_tetrad()

# Install a specific version and force re-download
install_tetrad(version = "7.6.10", force = TRUE)

# Install in a temporary directory
install_tetrad(temp_dir = TRUE)

# Install quietly (suppress messages)
install_tetrad(quiet = TRUE)
} # }
```
