#read procedure classification
props <- read.csv("properties.csv")

#Length is ncol - 1 as there is an ID variable in props
nProps <- ncol(props) - 1

#All possible procedures
allProcedures <- props$ID

#choose applicable procedures based on input from the 16 properties
procedureChooser <- function(inProps) {
  procedures <- allProcedures
  i <- 1
  while (length(procedures) > 0 && i <= nProps) {
    #convert to numeric (otherwise read as int)
    thisProp <- as.numeric(inProps[[i]])
    if (!is.null(inProps)) {
      if (identical(thisProp,1)) {
        validProcedures <- props[sapply(props[,i+1],
                                        function(x) identical(as.numeric(x), 1)),
                                 "ID"]
        procedures <- intersect(procedures, validProcedures)

      }
    }
    i <- i + 1
  }

  procedures
}

