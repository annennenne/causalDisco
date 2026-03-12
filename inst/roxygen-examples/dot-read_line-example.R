# Reads a line from the console
# (only run interactively)
if (interactive()) {
  name <- .read_line("What's your name? ")
  cat("Hello,", name, "\n")
}
