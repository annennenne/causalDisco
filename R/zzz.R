.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("java.heap.size"))) {
    options(java.heap.size = default_heap())
  }
}

.onAttach <- function(libname, pkgname) {
  if (is_interactive() &&
    is.null(getOption("java.heap.size")) &&
    !nzchar(Sys.getenv("JAVA_HEAP_SIZE", unset = ""))) {
    options(java.heap.size = ask_heap_size())
  }

  raw_heap <- getOption("java.heap.size", default_heap())
  heap_gb <- parse_heap_gb(raw_heap)
  options(java.heap.size = paste0(heap_gb, "g"))

  init_java(heap = paste0(heap_gb, "g"))

  current_heap_size <- current_heap_gb()
  if (abs(current_heap_size - heap_gb) > 0.1) {
    warning(
      "Java heap is ", current_heap_size,
      " GB but you requested ", heap_gb,
      " GB. Restart R to change the heap.",
      call. = FALSE
    )
  } else {
    packageStartupMessage(
      "causalDisco is inintalized with Java heap size ",
      current_heap_size, "gb for Tetrad.\n",
      "To change it to Ngb, set options(java.heap.size = 'Ng') or ",
      "Sys.setenv(JAVA_HEAP_SIZE = 'Ng') *before* loading. \n\n",
      "If you need to change the heap size, you need to restart R. \n\n",
      "For example, to set it to 4gb, use options(java.heap.size = '4g') ",
      "or Sys.setenv(JAVA_HEAP_SIZE = '4g')."
    )
  }
}
