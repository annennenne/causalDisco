#' @imports R6
#' @imortFrom pander pander
#' @export
Log <- R6::R6Class("Log",
               public = list(
                 initialize = function() {
                   self$add_entry("Log initialized")
                 },
                 get = function() {
                   data.frame(time = private$time$get(),
                              author = private$author$get(),
                              entry = private$entry$get())
                 },
                 add_entry = function(string = deparse(sys.call())) {
                   private$entry$add(string)
                   private$time$add(Sys.time())
                   private$author$add(Sys.info()["login"])
                   invisible(self)
                 },
                 print = function() {
                   pander::pander(data.frame(time = private$time$get(),
                                             author = private$author$get(),
                                             entry = private$entry$get()))
                 }
               ),
               private = list(
                 time = CharStack$new(),
                 entry = CharStack$new(),
                 author = CharStack$new()
               )
)


a <- Log$new()
a$get()
a$add_entry("Made an entry")
bbbbb <- Log$new()
bbbbb$get()
