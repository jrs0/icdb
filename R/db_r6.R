Database <- R6::R6Class("Database",
                        public = list(
                          #' Title
                          #'
                          #' @param val A value to pass
                          #'
                          #' @return A new (R6) Database object
                          #' @export
                          #'
                          #' @examples
                          #'
                          #' db <- Database$new(12)
                          initialize = function(val)
                          {
                            private$val <- val
                          }


                        ),
                        private = list(val = 10))


# The lines below is necessary to surpress a warning about no
# imports from R6 (see "https://stackoverflow.com/questions/64055049/
# unexpected-note-namespace-in-imports-field-not-imported-from-r6)"
#
#' @import R6
NULL
