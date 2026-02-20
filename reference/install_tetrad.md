# Install Tetrad GUI

Downloads and installs the Tetrad GUI JAR file for a specified version
into a user-specified directory. Configures the R session to know the
installation location via the `TETRAD_DIR` option.

This function asks the user to confirm the installation directory
interactively, ensures the directory exists, and downloads the JAR only
if it’s missing or `force = TRUE`.

## Usage

``` r
install_tetrad(
  version = getOption("causalDisco.tetrad.version"),
  dir = NULL,
  force = FALSE,
  verbose = FALSE
)
```

## Arguments

- version:

  Character; the Tetrad version to install. Default is
  `getOption("causalDisco.tetrad.version")`.

- dir:

  Character; the directory where the JAR should be installed. Default is
  `"~/tetrad"`. The function will create this directory if it does not
  exist. The user will be prompted to confirm the location.

- force:

  Logical; if `TRUE`, forces re-download even if the JAR already exists.
  Default is `FALSE`.

- verbose:

  Logical; if `TRUE`, shows download progress. Default is `FALSE`.

## Value

Invisibly returns the full path to the installed Tetrad JAR.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install default version in default directory
install_tetrad()

# Install a specific version and force re-download
install_tetrad(version = "7.2.0", force = TRUE)

# Install with verbose messages
install_tetrad(verbose = TRUE)
} # }
```
