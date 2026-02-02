### tetrad_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Requires Tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  data(num_data)

  # Recommended:
  my_pc <- pc(engine = "tetrad", test = "fisher_z")
  my_pc(num_data)

  # or
  disco(data = num_data, method = my_pc)

  # Example with detailed settings:
  my_pc2 <- pc(
    engine = "tetrad",
    test = "sem_bic",
    penalty_discount = 1,
    structure_prior = 1,
    singularity_lambda = 0.1
  )
  disco(data = num_data, method = my_pc2)

  # Using R6 class:
  s <- TetradSearch$new()

  s$set_data(num_data)
  s$set_test(method = "fisher_z", alpha = 0.05)
  s$set_alg("pc")

  g <- s$run_search()

  print(g)
}
