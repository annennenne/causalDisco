.onLoad <- function(libname, pkgname) {
  # Initialize the JVM if it isn’t already running.
  # todo : how many gb?
  if (!.jniInitialized) {
    .jinit(
      parameters = "-Xmx2g",
      classpath = "/tetrad/tetrad-current.jar"
    )
  }
}
