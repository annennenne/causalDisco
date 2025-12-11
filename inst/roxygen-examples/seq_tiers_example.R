### seq_tiers() example ###

# generate a bundle of tier formulas using a pattern with {i}
# here we create: 1 ~ matches("^child_x1$"), 2 ~ matches("^child_x2$")
formulas <- seq_tiers(1:2, matches("^child_x{i}$"))

# you can pass this bundle directly to tier() inside knowledge()
data(tpc_example)
kn <- knowledge(head(tpc_example), tier(formulas))
print(kn)
