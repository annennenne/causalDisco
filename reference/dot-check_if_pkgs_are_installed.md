# Check if required packages are installed

Used in functions and classes in causalDisco to check if required
packages are installed with a more informative error message.

## Usage

``` r
.check_if_pkgs_are_installed(pkgs, function_name = NULL, class_name = NULL)
```

## Arguments

- pkgs:

  Character vector of package names to check.

- function_name:

  Name of the function requiring the packages (string).

- class_name:

  Name of the R6 class requiring the packages (string).

## Value

Invisibly returns TRUE if all packages are installed, otherwise stops or
warns.

## Examples

``` r
# Succeeds silently (base packages are always available)
causalDisco:::.check_if_pkgs_are_installed(
  pkgs = c("stats"),
  function_name = "my_fun"
)

# Same check but for an R6 class context
causalDisco:::.check_if_pkgs_are_installed(
  pkgs = c("utils"),
  class_name = "MyClass"
)

# This will error with an informative message
try(
  causalDisco:::.check_if_pkgs_are_installed(
    pkgs = c("definitelyNotAPackage123", "alsoNotAPackage456"),
    function_name = "my_fun"
  )
)
#> Error : The following packages are required for `my_fun()` but are not installed: 
#>        [definitelyNotAPackage123, alsoNotAPackage456].
#>        Please install them with install.packages().
try(
  causalDisco:::.check_if_pkgs_are_installed(
    pkgs = c("definitelyNotAPackage123", "alsoNotAPackage456"),
    class_name = "MyClass"
  )
)
#> Error : The following packages are required for the R6 class `MyClass` but are not installed: 
#>        [definitelyNotAPackage123, alsoNotAPackage456].
#>        Please install them with install.packages().
```
