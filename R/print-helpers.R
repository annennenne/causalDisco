#' @title Print section helper
#' @description Print a titled section with a formatted tibble.
#' @param title Section title.
#' @param tbl Data frame or tibble to print.
#' @param header_fmt Optional function to format the header line.
#' @param n_max Maximum number of rows to print in compact mode.
#' @keywords internal
#' @noRd
print_section <- function(
  title,
  tbl,
  header_fmt = NULL,
  n_max = 20
) {
  if (!nrow(tbl)) {
    return(invisible())
  }

  cli::cli_h2(title)

  # format as tibble text lines
  lines <- format(tibble::as_tibble(tbl))
  lines <- lines[-1L] # drop "# A tibble: ..."

  if (!is.null(header_fmt)) {
    lines[1] <- header_fmt(lines[1])
  }

  if (length(lines) > n_max) {
    cat(lines[1:n_max], sep = "\n")
    cli::cli_text("... and {length(lines) - n_max} more rows")
  } else {
    cat(lines, sep = "\n")
  }
}
