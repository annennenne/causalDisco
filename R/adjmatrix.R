#' @importFrom R6 R6Class
adjmatrix <- R6::R6Class("adjmatrix",
                     public = list(
                       initialize = function(m) {
                         if (is.matrix(m)) {
                           if (nrow(m) == ncol(m)) {
                             if (!is.null(rownames(m))) {
                               if (!is.null(colnames(m))) {
                                 if (rownames(m) != colnames(m)) {
                                   stop("Row and column names must match for a valid adjacency matrix")
                                 }
                               } else {
                                 colnames(m) <- rownames(m)
                               }
                             }
                           }
                         }
                         invisible(m)
                       }
                     )
)

