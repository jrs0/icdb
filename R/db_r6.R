Database <- R6::R6Class("Database",
                        public = list(
                          #' Title
                          #'
                          #' @param val
                          #'
                          #' @return
                          #' @export
                          #'
                          #' @examples
                          initialize = function(val)
                          {
                            self$val <- val
                          }


                        ),
                        private = list(val = 10))
