# Requires tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  set.seed(1405)
  df <- data.frame(
    cont = rnorm(6),
    disc = as.integer(sample(0:2, 6, replace = TRUE))
  )

  # R -> Tetrad (DataSet)
  jds <- rdata_to_tetrad(df, int_cols_as_cont = FALSE)
  rJava::.jinstanceof(jds, "edu/cmu/tetrad/data/DataSet") # should be TRUE

  # Tetrad (DataSet) -> R
  df_roundtrip <- tetrad_data_to_rdata(jds)
  str(df_roundtrip)

  # Check types are preserved: cont is numeric (double), disc is integer
  stopifnot(is.numeric(df_roundtrip$cont), is.integer(df_roundtrip$disc))

  # Values should match (up to numeric tolerance)
  stopifnot(all.equal(df$cont, df_roundtrip$cont))
  stopifnot(identical(df$disc, df_roundtrip$disc))
}
