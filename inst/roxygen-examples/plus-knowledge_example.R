# Create two knowledge objects
kn1 <- knowledge(
  tier(
    1 ~ V1,
    2 ~ V2
  ),
  V1 %-->% V2
)

kn2 <- knowledge(
  tier(3 ~ V3),
  V2 %!-->% V3
)

kn_merged <- kn1 + kn2

# Error paths
# Merging with conflicting tier information

kn1 <- knowledge(
  tier(
    1 ~ V1,
    2 ~ V2
  )
)

kn2 <- knowledge(
  tier(3 ~ V2)
)

try(kn1 + kn2)

kn2 <- knowledge(
  tier(1 ~ V1 + V2)
)

try(kn1 + kn2)

# Requried / forbidden violations

kn1 <- knowledge(
  V1 %!-->% V2
)

kn2 <- knowledge(
  V1 %-->% V2
)

try(kn1 + kn2)
