# Install Eclipse Temurin JDK

Installs the Eclipse Temurin JDK in the user's home directory (or a
specified location) and configures the environment so the JDK is
immediately available to the current R session.

This function sets the `JAVA_HOME` environment variable, ensuring that
packages such as **rJava** can find and use the JDK without additional
configuration.

The function automatically handles downloads and extraction for
**Windows** and **macOS**, using the appropriate archive format. Linux
is not supported, as Java is typically installed via the system package
manager.

## Usage

``` r
install_java(force = FALSE, install_dir = "~/temurin", verbose = FALSE)
```

## Arguments

- force:

  Logical; if `TRUE`, forces reinstallation even if a JDK already exists
  in the specified directory. Default is `FALSE`.

- install_dir:

  Character; the directory where the JDK should be installed. Default is
  `"~/temurin"`. The function will create this directory if it does not
  exist. If a JDK is already present, it will be used unless
  `force = TRUE`, in which case it will be reinstalled.

- verbose:

  Logical; if `TRUE`, shows download progress and messages. Default is
  `FALSE`.

## Value

Invisibly returns the path to `JAVA_HOME` for the installed or detected
JDK.

## Details

The function requires an **interactive R session**. It will prompt the
user to confirm the installation directory using
[`utils::askYesNo()`](https://rdrr.io/r/utils/askYesNo.html). If the
session is non-interactive, the function will stop with an error.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install with default directory
install_java()

# Install in a custom directory and force reinstall
install_java(install_dir = "C:/Java/temurin", force = TRUE)

# Install with verbose messages
install_java(verbose = TRUE)
} # }
```
