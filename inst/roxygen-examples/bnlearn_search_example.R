### bnlearn_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Load data
data("tpcExample")

# Recommended:
my_pc <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
result <- my_pc(tpcExample)

# or
result <- disco(data = tpcExample, method = my_pc)

plot(result)

# Using R6 class:
s <- bnlearnSearch$new()

s$set_data(tpcExample)
s$set_test(method = "fisher_z", alpha = 0.05)
s$set_alg("pc")

g <- s$run_search()

plot(g)
