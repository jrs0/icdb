Database <- R6::R6Class(
  "Database",
  public = list(
    #' Construct a Database object
    #'
    #' This object manages a database connection. The connection is configured
    #' using a json file which stores configuration information and credentials.
    #' The fields of this configuration file are as follows:
    #'
    #' driver:
    #'
    #' @param db_config The absolute path of a configuration file (csv
    #' format) containing database connection information and credentials.
    #' The default value is the credential file stored in the inst/ directory.
    #'
    #' @return A new (R6) Database object
    #' @export
    #'
    #' @examples
    #'
    #' db <- Database$new()
    initialize = function(db_config = system.file("extdata",
                                                  "db_config.json",
                                                  package = "rdatabase"))
    {
      if (!file.exists(db_config))
        stop("Database connection failed, input credential file does not exist")
      private$config <- rjson::fromJSON(file = db_config)
    }


  ),
  private = list(config = NULL,
                 connection = NULL)
)

# The lines below is necessary to surpress a warning about no
# imports from R6 (see "https://stackoverflow.com/questions/64055049/
# unexpected-note-namespace-in-imports-field-not-imported-from-r6)"
#
#' @import R6
NULL
