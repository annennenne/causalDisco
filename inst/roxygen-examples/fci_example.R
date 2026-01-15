data("tpc_example")

# Recommended path using disco()
fci_pcalg <- fci(engine = "pcalg", test = "fisher_z", alpha = 0.05)
disco(tpc_example, fci_pcalg)

# or using fci_pcalg directly
fci_pcalg(tpc_example)

#### Using tetrad engine with tier knowledge ####
# Requires Tetrad to be installed
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
  fci_tetrad <- fci(engine = "tetrad", test = "fisher_z", alpha = 0.05)
  disco(tpc_example, fci_tetrad, knowledge = kn)

  # or using fci_tetrad directly
  fci_tetrad <- fci_tetrad |> set_knowledge(kn)
  fci_tetrad(tpc_example)
}
