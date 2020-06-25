
disco <- function(data, properties) {
  procedures <- procedureChooser(properties)

  results <- list()

  #bnlearn::pcstable procedure ID 16
  if (16 %in% procedures) {
    results <- c(results, `bnlearn::pc.stable` = call_bnlearn("pc.stable", data))
  }

  results
}



#choose applicable procedures based on input from the 16 properties
procedureChooser <- function(inProps, propertyTable = props) {
 #Length is ncol - 1 as there is an ID variable in props
  nProps <- ncol(propertyTable) - 1

  #All possible procedures
  procedures <- propertyTable$ID

  inProps <- propLabels$assumption %in% inProps

  i <- 1
  while (length(procedures) > 0 && i <= nProps) {
    #convert to numeric (otherwise read as int)
    thisProp <- as.numeric(inProps[[i]])
    if (!is.null(inProps)) {
      if (identical(thisProp,1)) {
        validProcedures <- propertyTable[sapply(propertyTable[,i+1],
                                        function(x) identical(as.numeric(x), 1)),
                                 "ID"]
        procedures <- intersect(procedures, validProcedures)

      }
    }
    i <- i + 1
  }

  procedures
}

bn2scm <- function(bn) {
  arcs <- bn$arcs
  forms <- list()

  for (i in 1:length(nrow(arcs))) {
    forms <- c(forms, formulize(arcs[i, "from"], arcs[i, "to"]))
  }

  SCM$new(formulas = forms)
}

call_bnlearn <- function(procedure, data) {
  bnm <- eval(as.call(list(getFromNamespace(procedure, "bnlearn"), x = data)))
  bn2scm(bnm)
}

#procedureChooser(list("numData", "implemented"))



formulize = function(lhs, rhs) {
  if (length(lhs) > 1) {
    lhs <- paste("list(", paste(lhs, collapse = ","), ")")
  }
  rhs <- paste(rhs, collapse = "+")
  as.formula(paste(lhs, "~", rhs))
}



res <- disco(numData, properties = list("numData"))
