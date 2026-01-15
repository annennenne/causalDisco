# For reproducibility
set.seed(1405)

# Number of samples
n <- 1000

# Define probabilities for A
p_A <- c(0.4, 0.35, 0.25) # Probabilities for A = {1, 2, 3}

# Simulate A from a categorical distribution
A <- sample(1:3, n, replace = TRUE, prob = p_A)

# Define conditional probabilities for B given A
p_B_given_A <- list(
  c(0.7, 0.3), # P(B | A=1)
  c(0.4, 0.6), # P(B | A=2)
  c(0.2, 0.8) # P(B | A=3)
)

# Sample B based on A
B <- sapply(A, function(a) sample(1:2, 1, prob = p_B_given_A[[a]]))

# Define conditional probabilities for C given A and B
p_C_given_A_B <- list(
  "1_1" = c(0.6, 0.4), # P(C | A=1, B=1)
  "1_2" = c(0.3, 0.7), # P(C | A=1, B=2)
  "2_1" = c(0.5, 0.5), # P(C | A=2, B=1)
  "2_2" = c(0.2, 0.8), # P(C | A=2, B=2)
  "3_1" = c(0.7, 0.3), # P(C | A=3, B=1)
  "3_2" = c(0.4, 0.6) # P(C | A=3, B=2)
)

# Sample C based on A and B
C <- mapply(
  function(a, b) sample(1:2, 1, prob = p_C_given_A_B[[paste(a, b, sep = "_")]]),
  A,
  B
)

# Create dataset
simdata <- data.frame(as.factor(A), as.factor(B), as.factor(C))

# Define order in prefix way
colnames(simdata) <- c("child_A", "child_B", "adult_C")
prefix_order <- c("child", "adult")

# Define knowledge object
kn <- knowledge(
  simdata,
  tier(
    child ~ tidyselect::starts_with("child"),
    adult ~ tidyselect::starts_with("adult")
  )
)

# Define TemporalBDeu score
t_score <- new("TemporalBDeu", knowledge = kn, data = simdata)
# Run tges
tges_pre <- tges_run(t_score)

# Plot MPDAG
# plot(tges_pre)
