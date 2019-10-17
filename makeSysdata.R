props <- read.csv("./internal_data/properties.csv")
propLabels <- read.csv("./internal_data/propertyLabels.csv")
descs <- read.csv("./internal_data/descriptions.csv")
descs <- descs[!is.na(descs$ID),]

#export data to /R/sysdata.rda
usethis::use_data(props, propLabels, descs, internal = TRUE, overwrite = TRUE)
