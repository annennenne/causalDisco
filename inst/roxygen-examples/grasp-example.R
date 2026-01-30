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
