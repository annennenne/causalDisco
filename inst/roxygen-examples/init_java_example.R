### init_java() example ###

# Only run in an interactive session and before rJava is already initialized
if (requireNamespace("rJava", quietly = TRUE) && !rJava::.jniInitialized) {
  init_java(heap = default_heap())
}
