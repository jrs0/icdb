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
      print(private$config)
      private$connection <- DBI::dbConnect(
        driver = "SQL Server",
        #RMariaDB::MariaDB(),
        odbc::odbc(),
        host = private$config$host,
        port = private$config$port,
        database = private$config$dbname,
        #user = private$config$user,
        #password = private$config$password
      )
      print(private$connection)
    }


  ),
  private = list(config = NULL,
                 connection = NULL)
)

# The lines below is necessary to surpress a warning about no
# imports from R6 (see "https://stackoverflow.com/questions/64055049/
# unexpected-note-namespace-in-imports-field-not-imported-from-r6)"
#
# I'm not really sure that this is the best way to do things, but it
# gets rid of the check() notes for now.
#
#' @import R6
#' @import rjson
NULL
