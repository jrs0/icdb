#' Database R6 class
#'
#' A wrapper for a database connection to an SQL Server (Microsoft)
Database <- R6::R6Class(
  "Database",
  public = list(

    #' Construct a Database object
    #'
    #' This object manages a database connection. The connection is configured
    #' using either a data source name, or a json file which stores
    #' configuration information and credentials. After this function has run
    #' without errors, you should have a new Database object containing a valid
    #' connection.
    #'
    #' A data source name (DNS) is a string that refers to a connection set
    #' using the "ODBC data sources" app on Windows (8). Open this app; in the
    #' "User DSN" tab, add a new data source using "Add". Select "SQL Server",
    #' and then follow through the wizard. Choose a name for the data source,
    #' which is arbitrary -- this is the data source name. Set the "Server"
    #' field to the server name (the same as in SQL Management Studio). In one
    #' of the pages, make sure you pick a default database to connect to.
    #' Finally, at the end of the wizard, check the connection. If it fails,
    #' debug it before moving on to using this function.
    #'
    #' If you cannot make the DSN work, you can also supply a configuration file,
    #' in JSON format. Three keys are required: "driver" must be set to the
    #' database driver (potentially "SQL Server"); "server" must be set the to
    #' server name; and "database" must be set to the name of the database to
    #' connect to. An example configuration file is in extdata/db_config.json.
    #'
    #' doneness 0
    #'
    #' @param data_source_name The data source name (DSN) for the database.  If
    #'   this argument is provided, it will be preferred to the config file.
    #'
    #' @param db_config The absolute path of a configuration file (csv format)
    #'   containing database connection information and credentials. The default
    #'   value is the credential file stored in the inst/ directory.
    #'
    #' @return A new (R6) Database object
    #'
    initialize = function(data_source_name = NULL,
                          db_config = NULL)
    {
      # If the data source name argument was passed, connect using that
      if (!is.null(data_source_name))
      {
        private$dsn = data_source_name
        message("Connecting using data source name (DSN): ", private$dsn)
        private$connection <-
          DBI::dbConnect(odbc::odbc(), private$dsn)
      }
      else if (!is.null(db_config))
      {
        if (!file.exists(db_config))
        {
          stop("The supplied db_config file ", db_config, " does not exist.")
        }
        message("Connecting using config file")
        private$config <- rjson::fromJSON(file = db_config)
        private$connection <- DBI::dbConnect(
          odbc::odbc(),
          driver = private$config$driver,
          server = private$config$server,
          database = private$config$database,
        )
      }
      else
      {
        stop("You must provide a data source name or a config file argument.")
      }
    },

    #' Close database connection on object deletion
    #'
    #' This function closes the previously open database connection when
    #' the object is deleted (e.g. when rm(db) is run).
    #'
    #' Doneness 0
    #'
    finalize = function()
    {
      message("Removing Database object")
      if (DBI::dbIsValid(private$connection))
      {
        message("Closing previously open database connection")
        DBI::dbDisconnect(private$connection)
      }
    },

    #' Print the state of the Database object
    #'
    #' Doneness 0
    #'
    #' @return Invisible self
    #'
    print = function()
    {
      if(!is.null(private$dsn))
      {
        message("Database connection via data source name (DSN): ", private$dsn)
      }
      else
      {
        message("Database connection via config file: ", private$config)
      }
      message("Connection information:")
      print(private$connection)
      invisible(self)
    }


  ),
  private = list(dsn = NULL,
                 config = NULL,
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
