default_heap <- function() {
  getOption(
    "java.heap.size",
    Sys.getenv("JAVA_HEAP_SIZE", unset = NA)
  ) |>
    {
      \(x) if (is.na(x)) "2g" else x
    }()
}

ask_heap_size <- function() {
  prompt <- paste(
    "How many GB should the Java heap use?",
    "Press <Return> for the default (2):"
  )
  repeat {
    answer <- readline(prompt)
    if (!nzchar(answer)) {
      return("2g")
    }
    if (grepl("^[1-9][0-9]*$", answer)) {
      return(paste0(answer, "g"))
    }
    message("Please enter a positive integer such as 4, 8, 16 …")
  }
}

find_tetrad_jars <- function() {
  jar_dir <- system.file("java", package = "causalDisco")
  list.files(jar_dir, pattern = "\\.jar$", full.names = TRUE)
}

init_java <- function(heap = default_heap()) {
  jars <- find_tetrad_jars()
  if (!length(jars)) {
    stop("No Tetrad JARs found in ", system.file("java", package = "causalDisco"))
  }

  if (rJava::.jniInitialized) {
    rJava::.jaddClassPath(setdiff(jars, rJava::.jclassPath()))
  } else {
    rJava::.jinit(parameters = paste0("-Xmx", heap), classpath = jars)
  }
}

# R/aaa_java_init_helpers.R
parse_heap_gb <- function(x) {
  stopifnot(length(x) == 1)

  x <- tolower(trimws(as.character(x)))

  if (grepl("^\\d+g(b)?$", x)) {
    return(as.double(sub("g(b)?$", "", x)))
  }

  if (grepl("^\\d+m(b)?$", x)) {
    return(as.double(sub("m(b)?$", "", x)) / 1024)
  }

  stop(
    "Heap string \"", x, "\" not recognised. ",
    "Specify a whole number followed by 'g' (gigabytes) ",
    "or 'm' (megabytes), e.g. \"4g\" or \"4096m\"."
  )
}
