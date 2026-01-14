# ──────────────────────────────────────────────────────────────────────────────
# rdata_to_tetrad() and tetrad_data_to_rdata()
# ──────────────────────────────────────────────────────────────────────────────

test_that("rdata_to_tetrad() validates input", {
  skip_if_no_tetrad()
  expect_error(rdata_to_tetrad(1), "Input must be a data frame.")

  df_bad <- data.frame(a = 1:3, b = c("x", "y", "z"))
  expect_error(
    expect_warning(rdata_to_tetrad(df_bad)),
    "Data frame contains non-numeric or non-factor columns."
  )
})

test_that("tetrad_data_to_rdata() and rdata_to_tetrad() round-trip mixed data with NAs", {
  skip_if_no_tetrad()

  # small mixed data frame
  my_df <- data.frame(
    x = c(1, 2.5, NA_real_), # continuous (double)
    y = factor(c(1L, NA_integer_, 3L)), # discrete (factor)
    check.names = FALSE
  )

  # R -> Java
  ds <- rdata_to_tetrad(my_df, FALSE)
  # quick sanity: Java reports same shape
  nrows <- rJava::.jcall(ds, "I", "getNumRows")
  ncols <- rJava::.jcall(ds, "I", "getNumColumns")
  expect_identical(nrows, nrow(my_df))
  expect_identical(ncols, ncol(my_df))

  # Java -> R
  back <- tetrad_data_to_rdata(ds)

  # names preserved
  expect_identical(names(back), names(my_df))

  # types preserved
  expect_identical(class(back$x), "numeric")
  expect_identical(class(back$y), "factor")

  # values preserved, including NAs
  expect_equal(back$x, my_df$x, tolerance = 1e-12)
  expect_identical(is.na(back$x), is.na(my_df$x))
  expect_equal(as.integer(back$y), as.integer(my_df$y))
})

test_that("rdata_to_tetrad() constructs expected variable kinds (smoke test)", {
  skip_if_no_tetrad()

  my_df <- data.frame(
    cont = c(0.1, NA_real_, 2.3),
    disc = c(1L, 2L, NA_integer_),
    check.names = FALSE
  )

  ds <- expect_warning(
    rdata_to_tetrad(my_df, FALSE),
    "The following integer columns are not factors: disc. They will be converted to numeric."
  )

  my_df <- data.frame(
    cont = c(0.1, NA_real_, 2.3),
    disc = factor(c(1L, 2L, NA_integer_)),
    check.names = FALSE
  )

  ds <- rdata_to_tetrad(my_df, FALSE)

  # column 0: ContinuousVariable, column 1: DiscreteVariable
  node0 <- rJava::.jcall(
    ds,
    "Ledu/cmu/tetrad/graph/Node;",
    "getVariable",
    as.integer(0)
  )
  node1 <- rJava::.jcall(
    ds,
    "Ledu/cmu/tetrad/graph/Node;",
    "getVariable",
    as.integer(1)
  )

  expect_true(rJava::.jinstanceof(
    node0,
    "edu/cmu/tetrad/data/ContinuousVariable"
  ))
  expect_true(rJava::.jinstanceof(
    node1,
    "edu/cmu/tetrad/data/DiscreteVariable"
  ))

  # back conversion keeps NA conventions
  back <- tetrad_data_to_rdata(ds)
  expect_true(is.na(back$cont[2]))
  expect_true(is.na(back$disc[3]))
})

test_that("rdata_to_tetrad() preserves factor labels and values", {
  skip_if_no_tetrad()
  my_df <- data.frame(
    fac = factor(c("c", "a", NA, "b", "a"), levels = c("a", "b", "c")),
    check.names = FALSE
  )

  ds <- rdata_to_tetrad(my_df)
  node <- rJava::.jcall(
    ds,
    "Ledu/cmu/tetrad/graph/Node;",
    "getVariable",
    as.integer(0)
  )
  expect_true(rJava::.jinstanceof(node, "edu/cmu/tetrad/data/DiscreteVariable"))

  back <- tetrad_data_to_rdata(ds)

  expect_equal(class(back$fac), "factor")
  expect_equal(length(levels(back$fac)), length(levels(my_df$fac)))
  expect_equal(as.integer(back$fac), as.integer(my_df$fac))
  expect_true(is.na(back$fac[3]))
})

test_that("tetrad_data_to_rdata() assigns correct NA types", {
  skip_if_no_tetrad()

  # continuous with NA -> NA_real_
  cvar <- rJava::.jnew("edu/cmu/tetrad/data/ContinuousVariable", "cont")
  vlist1 <- rJava::.jnew("java/util/ArrayList")
  rJava::.jcall(vlist1, "Z", "add", rJava::.jcast(cvar, "java/lang/Object"))

  c_inner <- rJava::.jarray(c(NA_real_), dispatch = TRUE) # double[]
  c_outer <- rJava::.jarray(list(c_inner), dispatch = TRUE) # double[][]
  i_outer <- rJava::.jarray(list(rJava::.jnull("[I")), dispatch = TRUE) # int[][]

  ds_cont <- rJava::.jcall(
    "edu/cmu/tetrad/util/DataSetHelper",
    "Ledu/cmu/tetrad/data/DataSet;",
    "fromR",
    rJava::.jcast(vlist1, "java.util.List"),
    as.integer(1L),
    rJava::.jcast(c_outer, "[[D"),
    rJava::.jcast(i_outer, "[[I")
  )
  out_c <- tetrad_data_to_rdata(ds_cont)
  expect_true(is.na(out_c$cont[1]))

  # discrete with NA -> NA_integer_
  dvar <- rJava::.jnew(
    "edu/cmu/tetrad/data/DiscreteVariable",
    "disc",
    as.integer(3)
  )
  vlist2 <- rJava::.jnew("java/util/ArrayList")
  rJava::.jcall(vlist2, "Z", "add", rJava::.jcast(dvar, "java/lang/Object"))

  d_inner <- rJava::.jarray(as.integer(NA_integer_), dispatch = TRUE) # int[]
  d_outer <- rJava::.jarray(list(d_inner), dispatch = TRUE) # int[][]
  c0_outer <- rJava::.jarray(list(rJava::.jnull("[D")), dispatch = TRUE) # double[][]

  ds_disc <- rJava::.jcall(
    "edu/cmu/tetrad/util/DataSetHelper",
    "Ledu/cmu/tetrad/data/DataSet;",
    "fromR",
    rJava::.jcast(vlist2, "java.util.List"),
    as.integer(1L),
    rJava::.jcast(c0_outer, "[[D"),
    rJava::.jcast(d_outer, "[[I")
  )
  out_d <- tetrad_data_to_rdata(ds_disc)
  expect_true(is.na(out_d$disc[1]))
})
