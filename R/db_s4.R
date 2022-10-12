#' Database class wrapping an SQL server connection
#'
#' @slot connection Microsoft SQL Server.
#' @slot config list. Database connection information as a named list
#' @slot dsn
#'
#' @export
setClass(
  "DatabaseS4",
  slots = c(
    connection = "Microsoft SQL Server",
    config = "list",
    dsn = "character"
  ),
  prototype = list(
    connection = DBI::dbConnect(odbc::odbc(), "HES_ABI"),
    config = list(),
    dsn = NULL
  )
)

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
#' @return A new (S4) Database object
#'
DatabaseS4 <- function(data_source_name = NULL,
                      db_config = NULL)
{
  # If the data source name argument was passed, connect using that
  if (!is.null(data_source_name))
  {
    message("Connecting using data source name (DSN): ", data_source_name)
    new("DatabaseS4",
        connection = DBI::dbConnect(odbc::odbc(), data_source_name),
        dsn = data_source_name)
  }
  else if (!is.null(db_config))
  {
    if (!file.exists(db_config))
    {
      stop("The supplied db_config file ", db_config, " does not exist.")
    }
    message("Connecting using config file")
    # private$config <- rjson::fromJSON(file = db_config)
    # new("DatabaseS4", DBI::dbConnect(odbc::odbc(), data_source_name),
    #     dsn = data_source_name)
    # private$connection <- DBI::dbConnect(
    #   odbc::odbc(),
    #   driver = private$config$driver,
    #   server = private$config$server,
    #   database = private$config$database,
    #)
  }
  else
  {
    stop("You must provide a data source name or a config file argument.")
  }
}

setGeneric("dsn", function(x) {
  standardGeneric("dsn")
})
setGeneric("dsn<-", function(x, value) {
  standardGeneric("dsn<-")
})

setMethod("dsn", "DatabaseS4", function(x) {
  x@dsn
})
setMethod("dsn<-", "DatabaseS4", function(x, value)
{
  x@dsn <- value
  x
})
