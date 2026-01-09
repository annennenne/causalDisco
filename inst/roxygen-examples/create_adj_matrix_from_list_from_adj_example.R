# List
in_edges <- list(
  X = integer(0),
  Y = c(1L),
  Z = c(1L, 2L)
)

# Adjacency matrix
adj <- causalDisco:::create_adj_matrix_from_list(in_edges)

# Convert back
in_edges_back <- causalDisco:::create_list_from_adj_matrix(adj)
identical(in_edges_back, in_edges) # TRUE
