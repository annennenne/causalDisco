### pcalg_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Load data
data("tpcExample")

# Recommended:
pc(engine = "pcalg", test = "fisher_z")(tpcExample)

# or
my_pc <- pc(engine = "pcalg", test = "fisher_z")
my_pc(tpcExample)

# or
disco(data = tpcExample, method = my_pc)

# Using R6 class:
s <- pcalgSearch$new()

s$set_test(method = "fisher_z", alpha = 0.05)
s$set_data(tpcExample)
s$set_alg("pc")

g <- s$run_search()

print(g)
