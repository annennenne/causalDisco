data(tpc_example)

# Requires Tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  # Recommended path using disco()
  grasp_tetrad <- grasp(
    engine = "tetrad",
    test = "fisher_z",
    score = "sem_bic",
    alpha = 0.05
  )
  disco(tpc_example, grasp_tetrad)

  # or using grasp_tetrad directly
  grasp_tetrad(tpc_example)
}

#### With tier knowledge ####
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  kn <- knowledge(
    tpc_example,
    tier(
      child ~ tidyselect::starts_with("child"),
      youth ~ tidyselect::starts_with("youth"),
      oldage ~ tidyselect::starts_with("oldage")
    )
  )

  # Recommended path using disco()
  grasp_tetrad <- grasp(
    engine = "tetrad",
    test = "fisher_z",
    score = "sem_bic",
    alpha = 0.05
  )
  disco(tpc_example, grasp_tetrad, knowledge = kn)

  # or using grasp_tetrad directly
  grasp_tetrad <- grasp_tetrad |> set_knowledge(kn)
  grasp_tetrad(tpc_example)
}

# With all algorithm arguments specified
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  grasp_tetrad <- grasp_fci(
    engine = "tetrad",
    test = "poisson_prior",
    score = "rank_bic",
    alpha = 0.05,
    depth = 3,
    stable_fas = FALSE,
    max_disc_path_length = 5,
    covered_depth = 3,
    singular_depth = 2,
    nonsingular_depth = 2,
    ordered_alg = TRUE,
    raskutti_uhler = TRUE,
    use_data_order = FALSE,
    num_starts = 3
  )
  disco(tpc_example, grasp_tetrad)
}
