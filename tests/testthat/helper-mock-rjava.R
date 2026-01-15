# fake JVM state ----------------------------------------------------
.j_state <- new.env(parent = emptyenv())
.j_state$started <- FALSE
.j_state$class_path <- character()

fake_jinit <- function(parameters, classpath) {
  .j_state$started <- TRUE
  .j_state$class_path <- unique(c(.j_state$class_path, classpath))

  # reflect JVM-started in rJava namespace
  ns <- asNamespace("rJava")
  unlockBinding(".jniInitialized", ns)
  assign(".jniInitialized", TRUE, ns) # nolint: object_name_linter.
  lockBinding(".jniInitialized", ns)
  invisible(NULL)
}
fake_jadd <- function(cp) {
  .j_state$class_path <- unique(c(.j_state$class_path, cp))
  invisible(NULL)
}
fake_jcp <- function() .j_state$class_path

with_mock_rjava <- function(code) {
  ns <- asNamespace("rJava")

  # save originals
  originals <- mget(
    c(".jinit", ".jaddClassPath", ".jclassPath", ".jniInitialized"),
    envir = ns
  )

  # install fakes
  unlockBinding(".jinit", ns)
  assign(".jinit", fake_jinit, ns)
  unlockBinding(".jaddClassPath", ns)
  assign(".jaddClassPath", fake_jadd, ns) # nolint: object_name_linter.
  unlockBinding(".jclassPath", ns)
  assign(".jclassPath", fake_jcp, ns) # nolint: object_name_linter.
  unlockBinding(".jniInitialized", ns)
  assign(".jniInitialized", FALSE, ns) # nolint: object_name_linter.
  lockBinding(".jinit", ns)
  lockBinding(".jaddClassPath", ns)
  lockBinding(".jclassPath", ns)
  lockBinding(".jniInitialized", ns)

  # reset state
  .j_state$started <- FALSE
  .j_state$class_path <- character()

  on.exit(
    {
      # restore originals
      for (nm in names(originals)) {
        unlockBinding(nm, ns)
        assign(nm, originals[[nm]], ns)
        lockBinding(nm, ns)
      }
    },
    add = TRUE
  )

  force(code)
}
