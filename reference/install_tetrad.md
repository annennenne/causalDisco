# Install Tetrad GUI

This function downloads and installs the Tetrad GUI JAR file to a
specified directory. It also sets the TETRAD_DIR environment variable
for future R sessions.

## Usage

``` r
install_tetrad(
  version = getOption("causalDisco.tetrad.version"),
  dir = NULL,
  set_renviron = TRUE,
  force = FALSE
)
```

## Arguments

- version:

  Character. The version of Tetrad to install. Default is "7.6.10". Use
  `options(causalDisco.tetrad.version = "...")` to change the default
  version.

- dir:

  Character. The directory to install Tetrad into. Default is
  "~/tetrad".

- set_renviron:

  Logical. Whether to set the TETRAD_DIR in .Renviron. Default is TRUE.

- force:

  Logical. Whether to force re-download if the file already exists.
  Default is FALSE.

## Value

Invisible character string of the path to the downloaded JAR file.

## Examples

``` r
if (FALSE) { # \dontrun{
install_tetrad()
} # }
```
