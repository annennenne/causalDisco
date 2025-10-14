### tetrad_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Load data
data("tpcExample")

# Recommended:
my_pc <- pc(engine = "tetrad", test = "conditional_gaussian")
my_pc(tpcExample)

# or
disco(data = tpcExample, method = my_pc)

# Using R6 class:
s <- TetradSearch$new()

s$set_data(tpcExample)
s$set_test(method = "conditional_gaussian", alpha = 0.05)
s$set_alg("pc")

g <- s$run_search()

print(g)
