#Generate rmd pages (tiles) for each function available in descriptions.R
library(pander)
library(rmarkdown)


#Write line by line to file
writer <- function(outfile, x, ...) {
  cat(paste0(x, ...), file = outfile, append=TRUE, sep="\n")
}


#read description info. Only read actual rows (first 24)
descData <- read.csv("descriptions.csv", header = TRUE,
                     nrows = 24, stringsAsFactors = FALSE)


features <- c("package", "function_name", "input", "output", "link_docu", 
              "link_article", "link_develRepo")
featureNames <- c("Package:", "Function:", "Input:", "Output:", "Documentation:", 
                  "Article:", "Development repository:")


n <- nrow(descData)
for (i in 1:n) {
  thisInfo <- descData[i, , drop = FALSE]
  thisCodeExample <- readLines(paste("../app/examples/", 
                                     thisInfo["link_example"],
                                     sep = ""))
  
  #make feature table
  theseFeatures <- thisInfo[, features]
  areFilledOut <- !is.na(theseFeatures)
  thisTable <- matrix(NA, sum(areFilledOut), 1, 
                      dimnames = list(featureNames[areFilledOut], NULL))
  thisTable[,1] <- theseFeatures[areFilledOut]
  
  #open connection, write info
  filePath <- paste("../app/www/tile_", thisInfo["ID"], ".rmd", sep = "")
  conn <- file(filePath, "w")
  writer(conn, paste("#", thisInfo["name"], sep = ""))
  writer(conn, thisInfo["description"])
  writer(conn, pandoc.table.return(thisTable,  
                                             justify = "ll"))
  if (!is.na(thisInfo["comment1"]) | !is.na(thisInfo["comment2"])) {
    writer(conn, "**Note:**", "\n")
  }
  if (!is.na(thisInfo["comment1"])) {
    writer(conn, paste("-", thisInfo["comment1"]))
  }
  if (!is.na(thisInfo["comment2"])) {
    writer(conn, paste("-", thisInfo["comment2"]))
  }
  writer(conn, "\n")
  writer(conn, "*Minimal code example:*", "\n")
  writer(conn, "```` {r, eval = FALSE}")
  writer(conn, thisCodeExample)
  writer(conn, "```")
  close(conn)
  
  #render to html
  render(filePath, quiet = TRUE, output_format = "html_document")
}
