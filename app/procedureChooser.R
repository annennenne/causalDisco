#choose applicable procedures based on input from the 16 properties
props <- read.csv("../data/properties.csv")


procedureChooser <- function(inProps) {
  inProps[sapply(inProps, is.null)] <- 1
  inProps[sapply(inProps, is.na)] <- 1
  
  #props[, inProps]
}
