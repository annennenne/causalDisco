# Helper to reduce repetition
expect_output_list <- function(x) {
  expect_type(x, "list")
  expect_named(
    x,
    c("installed", "version", "java_ok", "java_version", "message")
  )
}
