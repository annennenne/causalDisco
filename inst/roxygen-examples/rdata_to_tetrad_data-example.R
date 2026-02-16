# Requires tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  set.seed(1405)
  my_df <- data.frame(
    cont = rnorm(6),
    disc = factor(as.integer(sample(0:2, 6, replace = TRUE)))
  )

  # R -> Tetrad (DataSet)
  jds <- rdata_to_tetrad(my_df, int_cols_as_cont = FALSE)
  rJava::.jinstanceof(jds, "edu/cmu/tetrad/data/DataSet") # should be TRUE

  # Tetrad (DataSet) -> R
  df_roundtrip <- tetrad_data_to_rdata(jds)
  str(df_roundtrip)

  # Check types are preserved: cont is numeric (double), disc is integer
  checkmate::assert_numeric(df_roundtrip$cont)
  checkmate::assert_factor(df_roundtrip$disc)

  # Numeric columns should match (up to tolerance)
  checkmate::assert_true(isTRUE(all.equal(my_df$cont, df_roundtrip$cont)))

  # Factor columns should match exactly when coerced to integers
  checkmate::assert_true(identical(
    as.integer(my_df$disc),
    as.integer(df_roundtrip$disc)
  ))
}
