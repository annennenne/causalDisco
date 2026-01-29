data(tpc_example)

# Requires Tetrad to be installed
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  # Recommended path using disco()
  boss_fci_tetrad <- boss_fci(engine = "tetrad", score = "sem_bic")
  disco(tpc_example, boss_fci_tetrad)

  # or using boss_fci_tetrad directly
  boss_fci_tetrad(tpc_example)
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
  boss_fci_tetrad <- boss_fci(engine = "tetrad", score = "sem_bic")
  disco(tpc_example, boss_fci_tetrad, knowledge = kn)

  # or using boss_fci_tetrad directly
  boss_fci_tetrad <- boss_fci_tetrad |> set_knowledge(kn)
  boss_fci_tetrad(tpc_example)
}
