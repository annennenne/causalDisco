# Install Eclipse Temurin JDK 25

Installs the Eclipse Temurin JDK 25 in the user's home directory and
configures the environment so the JDK is immediately available to the
current R session.

This function also sets the `JAVA_HOME` environment variable to ensure
that packages such as **rJava** work without additional configuration.

This helper function is intended for users who prefer an automated
installation or who find it inconvenient to manually download and
install Java from the Adoptium website
(<https://adoptium.net/temurin/releases>).

Linux is not supported by this helper, as Java is typically installed
via the system package manager.

## Usage

``` r
install_java(install_dir = "~/temurin25", force = FALSE)
```

## Arguments

- install_dir:

  Character; the directory where the JDK should be installed. Default is
  `"~/temurin25"`. The function will create this directory if it does
  not exist. If a JDK is already present in this directory, it will be
  used unless `force = TRUE` is specified, in which case it will be
  reinstalled.

- force:

  Logical; if `TRUE`, forces reinstallation even if the JDK is already
  present. Default is `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install with default directory
install_java()

#' Install in a custom directory and force reinstall
install_java(install_dir = "C:/Java/temurin25", force = TRUE)
} # }
```
