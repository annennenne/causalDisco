# This is an example for developers interested in implementing their own methods
# for causalDisco.

# Overview:
# First, we initialize the data and the knowledge object.
# Second, we implement a simple graph builder as a runner builder function.
#   Here we use a super-simple heuristic:
#     - compute pairwise correlations
#     - if |cor| >= 0.2:
#       * if variables are in different tiers: direct edge earlier --> later
#       * else: undirected edge (---)
# Third, we wrap the builder as a disco method using disco_method().
# Finally, we use the method with and without knowledge
#          with and without using disco()

# 1) Initialize data and knowledge
data(tpc_example)

kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

# 2) Implement a simple graph builder
toy_graph_builder <- function(knowledge = NULL) {
  state <- new.env(parent = emptyenv())
  state$knowledge <- knowledge
  state$thr <- 0.2 # correlation threshold

  runner <- list(
    set_knowledge = function(kn) {
      state$knowledge <- kn
      invisible(NULL)
    },
    run = function(data) {
      checkmate::assert_data_frame(data)
      vnames <- names(data)

      # correlation matrix
      C <- stats::cor(data, use = "pairwise.complete.obs")

      # tier ranks (NA if variable has no tier)
      tier_rank <- rep(NA_integer_, length(vnames))
      names(tier_rank) <- vnames
      if (!is.null(state$knowledge)) {
        tier_levels <- state$knowledge$tiers$label
        rank_map <- stats::setNames(seq_along(tier_levels), tier_levels)
        var_tier <- stats::setNames(
          state$knowledge$vars$tier,
          state$knowledge$vars$var
        )
        hits <- intersect(names(var_tier), vnames)
        tier_rank[hits] <- unname(rank_map[var_tier[hits]])
      }

      # build edge list
      out <- list()
      k <- 0L
      for (i in seq_along(vnames)) {
        for (j in (i + 1L):length(vnames)) {
          if (j > length(vnames)) {
            next
          }
          cij <- C[i, j]
          if (!is.na(cij) && abs(cij) >= state$thr) {
            vi <- vnames[i]
            vj <- vnames[j]
            ri <- tier_rank[vi]
            rj <- tier_rank[vj]
            if (!is.na(ri) && !is.na(rj) && ri != rj) {
              # orient from earlier -> later
              if (ri < rj) {
                k <- k + 1L
                out[[k]] <- list(
                  from = vi,
                  to = vj,
                  edge = "-->"
                )
              } else {
                k <- k + 1L
                out[[k]] <- list(
                  from = vj,
                  to = vi,
                  edge = "-->"
                )
              }
            } else {
              # same/unknown tier: undirected
              k <- k + 1L
              out[[k]] <- list(
                from = vi,
                to = vj,
                edge = "---"
              )
            }
          }
        }
      }
      # return a data frame
      if (length(out) == 0L) {
        cg <- caugi::caugi(
          from = character(),
          to = character(),
          edge = character()
        )
        return(knowledgeable_caugi(cg))
      }
      do.call(rbind, lapply(out, as.data.frame))
      cg <- caugi::caugi(
        from = out$from,
        to = out$to,
        edge = out$edge
      )

      knowledgeable_caugi(cg)
    }
  )
  runner
}

# 3) Wrap the builder as a disco method
toy_method <- causalDisco:::disco_method(toy_graph_builder, "toy")

# 4) Use the method
# Without knowledge: mostly undirected edges
toy_method(tpc_example)

# Inject knowledge, then run
toy_with_kn <- set_knowledge(toy_method, kn)
toy_with_kn(tpc_example)

# Or use the disco()
disco(tpc_example, toy_method, knowledge = kn)
