# create a bare knowledge object using .new_knowledge()
kn1 <- try(causalDisco:::.new_knowledge(vars = c("A", "B", "C"), frozen = TRUE))

# create the knowledge object in the intended way
df <- data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10))
kn2 <- knowledge(df)

identical(kn1, kn2) # TRUE
