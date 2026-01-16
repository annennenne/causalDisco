### pcalg_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Load data
data("tpc_example")

# Recommended:
pc(engine = "pcalg", test = "fisher_z")(tpc_example)

# or
my_pc <- pc(engine = "pcalg", test = "fisher_z")
my_pc(tpc_example)

# or
disco(data = tpc_example, method = my_pc)

# Using R6 class:
s <- PcalgSearch$new()

s$set_test(method = "fisher_z", alpha = 0.05)
s$set_data(tpc_example)
s$set_alg("pc")

g <- s$run_search()

print(g)
