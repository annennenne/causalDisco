# Only run in an interactive session and before rJava is already initialized
if (requireNamespace("rJava", quietly = TRUE) && !rJava::.jniInitialized) {
  try(causalDisco:::init_java(heap = default_heap()))
}
