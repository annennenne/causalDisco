# Succeeds silently (base packages are always available)
.check_if_pkgs_are_installed(
  pkgs = c("stats"),
  function_name = "my_fun"
)

# Same check but for an R6 class context
.check_if_pkgs_are_installed(
  pkgs = c("utils"),
  class_name = "MyClass"
)

# This will error with an informative message
try(
  .check_if_pkgs_are_installed(
    pkgs = c("definitelyNotAPackage123", "alsoNotAPackage456"),
    function_name = "my_fun"
  )
)
try(
  .check_if_pkgs_are_installed(
    pkgs = c("definitelyNotAPackage123", "alsoNotAPackage456"),
    class_name = "MyClass"
  )
)
