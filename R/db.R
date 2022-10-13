#' @importFrom methods new show
#' @importFrom magrittr %>%
#' @importClassesFrom DBI DBIConnection
#' @export
NULL

#' Database class wrapping an SQL server connection
#'
#' @slot connection Microsoft SQL Server.
#' @slot config list. Database connection information as a named list
#' @slot dsn
#'
#' @export
setClass(
  "Database",
  slots = c(
    connection = "DBIConnection",
    config = "list",
    dsn = "character"
  ),
  prototype = list(
    connection = NULL,
    config = list(),
    dsn = NA_character_
  )
)

#' Construct a Database object
#'
#' This object manages a database connection. The connection is configured
#' using either a data source name, or a json file which stores
#' configuration information and credentials. After this function has run
#' without errors, you should have a new Database object containing a valid
#' connection. The data source is specified by a data source name, which is
#' configured using a Windows program. This is the preferred connection method.
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
Database <- function(data_source_name = NULL,
                     db_config = NULL)
{
  # If the data source name argument was passed, connect using that
  if (!is.null(data_source_name))
  {
    message("Connecting using data source name (DSN): ", data_source_name)
    new(
      "Database",
      connection = DBI::dbConnect(odbc::odbc(), data_source_name),
      dsn = data_source_name
    )
  }
  else if (!is.null(db_config))
  {
    if (!file.exists(db_config))
    {
      stop("The supplied db_config file ", db_config, " does not exist.")
    }
    message("Connecting using config file")

    j <- rjson::fromJSON(file = db_config)

    con <- DBI::dbConnect(
      odbc::odbc(),
      driver = j$driver,
      server = j$server,
      database = j$database,
    )

    new("Database", connection = con, config = j)
  }
  else
  {
    stop("You must provide a data source name or a config file argument.")
  }
}

setGeneric("dsn", function(x) standardGeneric("dsn"))
setMethod("dsn", "Database", function(x) {
  x@dsn
})

setGeneric("tables", function(x)
  standardGeneric("tables"))
setMethod("tables", "Database", function(x) {
  DBI::dbListTables(x@connection)
})

setGeneric("fn", function(x) standardGeneric("fn"))
setMethod("fn", "Database", function(x) {
  vars <- c("PersonTitle")
  tbl <- dplyr::tbl(x@connection, "tbl_AE_SEM_ALL")

  #tbl %>% dplyr::select(dplyr::all_of(vars)) %>% dplyr::show_query() %>%
  #  dplyr::collect()

  # Put filter before select!

  tbl %>% utils::head(n = 10) %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::show_query() %>%
    dplyr::collect()
})

#' Print out the Database object
#'
#' @docType methods
#' @aliases show-Database show, Database-method
#'
#' @param object The object to be printed
#' @export
setMethod("show", "Database", function(object) {
  message("Wrapper around database connection")
  message("Database name: ", object@connection@info$dbname)
  if (!is.na(object@dsn))
  {
    message("Database connection via data source name (DSN): ", object@dsn)
  }
  else
  {
    message("Database connection via config file")
  }
})
