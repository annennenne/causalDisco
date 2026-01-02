### find_adjacencies() example ###

# Small toy adjacency (from-to)
amat <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
# A --> B, C --> B
amat["A", "B"] <- 1
amat["C", "B"] <- 1

# Adjacent to B (both incoming and outgoing considered)
causalDisco:::find_adjacencies(amat, 2L) # indices of A and C (1, 3)
