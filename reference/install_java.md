# Install Eclipse Temurin JDK 25 (with JAVA_HOME configuration)

Installs the Eclipse Temurin JDK 25 in the user's home directory and
configures the environment so the JDK is immediately available to the
current R session.

On **Windows**, this function also sets the `JAVA_HOME` environment
variable (both for the current session and persistently using `setx`) to
ensure that packages such as **rJava** work without additional
configuration.

On **macOS**, the JDK is installed under `~/temurin25` and the current R
session's `JAVA_HOME` is updated automatically. macOS users may need to
restart their terminal or R session for system-wide detection.

This helper function is intended for users who prefer an automated
installation or who find it inconvenient to manually download and
install Java from the Adoptium website
(<https://adoptium.net/temurin/releases>).

Linux is not supported by this helper, as Java is typically installed
via the system package manager.

## Usage

``` r
install_java(force = FALSE)
```

## Arguments

- force:

  Logical; if `TRUE`, forces reinstallation even if the JDK is already
  present. Default is `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
install_java()
} # }
```
