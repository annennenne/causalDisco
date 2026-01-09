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
try(
  causalDisco:::.check_if_pkgs_are_installed(
    pkgs = c("definitelyNotAPackage123", "alsoNotAPackage456"),
    class_name = "MyClass"
  )
)
