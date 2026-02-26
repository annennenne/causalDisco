.tetrad_required_edge_bug_methods <- c(
  "fci",
  "gfci",
  "grasp_fci",
  "sp_fci"
)

.tetrad_required_edge_hang_methods <- c(
  "boss",
  "boss_fci"
)


check_method_knowledge_bug <- function(method, knowledge) {
  if (is.null(knowledge) || is.null(knowledge$edges)) {
    return(invisible(method))
  }

  if (!identical(attr(method, "engine"), "tetrad")) {
    return(invisible(method))
  }

  has_required <- any(
    knowledge$edges$status == "required",
    na.rm = TRUE
  )

  if (!has_required) {
    return(invisible(method))
  }

  method_classes <- class(method)

  # ---- incorrect result bug (fixed upstream, not yet released) ----
  if (any(method_classes %in% .tetrad_required_edge_bug_methods)) {
    warning(
      paste0(
        "The Tetrad FCI-family algorithms (fci, gfci, grasp_fci, sp_fci) ",
        "in the currently released Tetrad version do not correctly enforce ",
        "required edges from knowledge. The resulting PAG may violate ",
        "required constraints.\n\n",
        "This issue is fixed in the unreleased Tetrad version. ",
        "To use required edges safely, install the development version of ",
        "Tetrad or use a different algorithm/engine.\n\n",
        "E.g., one way to do use the development version is to replace the `.jar` file in ",
        "`tools::R_user_dir('causalDisco/tetrad_v7.6.10', which = 'cache')` ",
        "with the `.jar` file from:\n",
        "https://github.com/cmu-phil/py-tetrad/blob/main/pytetrad/resources/tetrad-current.jar\n",
        "and keep the old name of tetrad-gui-7.6.10-launch.jar"
      ),
      call. = FALSE
    )
  }

  # ---- hanging bug ----
  if (any(method_classes %in% .tetrad_required_edge_hang_methods)) {
    stop(
      paste0(
        "The Tetrad BOSS-family algorithms (boss, boss_fci) cannot safely run ",
        "with required edges in knowledge because they may hang indefinitely. ",
        "Remove required edges or use another method.\n\n",
        "See https://github.com/cmu-phil/tetrad/issues/1950 for more information."
      ),
      call. = FALSE
    )
  }

  invisible(method)
}
