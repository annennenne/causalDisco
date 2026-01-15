kn <- knowledge()
causalDisco:::is_knowledge(kn) # TRUE
try(causalDisco:::is_knowledge(list())) # throws error
