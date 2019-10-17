#' @imports R6
#' @export
Observations <- R6::R6Class("Observations",
                 public = list(
                   initialize = function(data = NULL, variables = NULL,
                                         properties = NULL) {
                     #data overrules variables
                     if (!is.null(data) & !is.null(variables)) {
                       message(paste("Only one of \'data\' and \'variables\' can be specified.",
                                     "Ignoring \'variables\' input."))
                       variables <- NULL
                     }

                     #create mock data from variable names
                     if (!is.null(variables)) {
                       if ("character" %in% class(variables)) {
                         data <- as.data.frame(matrix(NA, 1, length(variables),
                                                      dimnames = list(NULL, variables)))
                       } else stop("\'variables\' must (vector of) be character string(s)")
                     }

                     if (is.null(data)) stop("Either \'data\' or \'variables\' must be supplied.")

                     #set data field
                     self$set_data(data)

                     #set properties field
                     self$set_properties(properties)
                     },
                   set_data = function(data) {
                     if (any(c("data.frame", "data.table", "tibble", "matrix") %in% class(data))) {
                       private$data <- data
                       return(invisible(self))
                     } else stop("data must be of type data.frame, data.table, tibble or matrix")
                   },
                   #set properties. If props is supplied, manually add properties, check consistency and
                   #produce warning if relevant. If props is not supplied, set them automatically
                   #by looking at the data.
                   set_properties = function(props = NULL) {
                     if (is.null(props)) {
                       private$properties <- private$autodetect_properties()
                     } else {
                       private$properties <- Set$new("DataProperty", props)
                     }
                   },
                   get_data = function() {
                     private$data
                   },
                   get_properties = function() {
                     private$properties
                   },
                   check_consistency = function() {
                     result <- FALSE
                     autoProps <- private$autodetect_properties()

                     if (length(setdiff(autoProps, private$properties) == 0)) {
                       result <- TRUE
                     } else {
                       message(paste("Detected non-consistency between data and stored data properties.",
                                     "Auto-detected properties in data:",
                                     autoProps,
                                     "Stored data properties:",
                                     private$properties, sep = "\n"))
                     }
                     result
                   }
                 ),
                 private = list(
                   data = NULL,
                   properties = NA,
                   autodetect_properties = function() {
                     #infer properties from data field automatically
                      #[do stuff]

                     NULL
                   }
                 )
)

oo <- Observations$new(data = data.frame(a = 1:10, b= c("a", "b")))
oo
oo$get_data()
oo$set_properties()
oo$get_properties()

pp <- Observations$new(variables = c("a", "b"))
