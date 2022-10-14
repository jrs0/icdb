#' @importFrom methods new show
#' @importFrom magrittr %>%
#' @importClassesFrom DBI DBIConnection
#' @export
NULL

#' A Table class for a table in the Database class
#'
#' The purpose of this class is to wrap the dplyr table shell object for
#' storing in the Database class. The Database class is essentially a list
#' of these objects, named by the table name. This permits tab-completion of
#' table names when a database is created. However, the innards of this class
#' is only populated when the table is created, so that it is not necessary
#' to store thousands of dplyr::tab objects. There is more optimisation work
#' to do, but this will test the principle.
#'
#' @slot tabname character The name of the table
#'
#' @export
#'
setClass(
    "Table",
    contains = "list",
    slots = representation(
      tabname = "character"),
    prototype = prototype(
      tabname = NA_character_)
)

#' Create a new Table object (not for use outside Database class)
#'
#' @param tabname The table name (a string)
#'
#' @return The new Table object
#' @export

Table <- function(tabname)
{
  new("Table", tabname = tabname)
}

#' Database class wrapping an SQL server connection
#'
#' @slot connection Microsoft SQL Server.
#' @slot config list. Database connection information as a named list
#' @slot dsn
#'
#' @export
setClass(
  "Database",
  contains = "list",
  slots = representation(
    connection = "DBIConnection",
    config = "list",
    dsn = "character"
  ),
  prototype = prototype(
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
    db <- new(
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

    db <- new("Database", connection = con, config = j)
  }
  else
  {
    stop("You must provide a data source name or a config file argument.")
  }

  # Copy the list of tables into the inherited list class (get rid of for)
  tables <- DBI::dbListTables(db@connection)

  # This is the problem part of the code -- it really needs to store a
  # function to return the table object, but that doesn't work (yet).
  for (t in tables)
  {
    db[[t]] <- t
  }

  db

}


#' Search the tables and columns in a database for partial names
#'
#' Several of the databases are very large, with hundreds of tables each
#' with hundreds of columns. This function is designed to help with finding
#' relevant columns in the tables of the database, by partially matching the
#' table and column names and printing the results.
#'
#'
#'
#' @param db The Database object to query
#' @param col_pattern The pattern to match column names (regexp)
#'
#' @return
#' @export
#'
#' @examples
setGeneric("searchCols", function(db, col_pattern, tab_pattern)
  standardGeneric("searchCols"))


setMethod("searchCols", "Database",
          function(db, col_pattern, tab_pattern) {

  # Filter table names
  tab_matches <- grep(tab_pattern, db, value=TRUE)

  for (t in tab_matches) {
    tryCatch(
      expr = {
        tbl <- table(db, t)
        names <- colnames(tbl)
        col_matches <- grep(col_pattern, names, value=TRUE)
        if (length(col_matches) > 0) {
          print(paste0("Found these colums in table '", t, "':"))
          writeLines(paste0("  ", capture.output(print(col_matches))))
          cat("\n")
        }
      },
      error = function(x) {
        warning(x)
      }
    )

  }
})

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


#' Access a table in a Database object
#'
#' Use this function to access a table in the Database object. The Database
#' object behaves like a list, but the elements of the list are evaluated only
#' when this function is called. When a call like db$table_name is made, this
#' function creates the corresponding dplyr::tbl object and returns it. Since
#' this is not a particularly long operation, the results do not need to be
#' cached. Use the resulting object for any processing you can do with
#' dplyr::tbl, e.g. pipe it to some SQL-type queries and collect().
#'
#' @param x The database
#' @param name The table to access
#'
#' @return A dplyr::tbl data source wrapper
#' @export
setMethod("$", "Database", function(x, name) {
  table(db, name)
})


#' Get a table in the database in a form ready for dplyr processing
#'
#' Use this function to obtain a dplyr::tbl (a kind of shell object) from a
#' table in the database. This object does not contain the full data to begin
#' with. Instead, it can be used as the basis of dplyr-based SQL queries,
#' finishing with a %>% collect() call which does actually produce data. This
#' function is an alternative to using a direct sql query using the sql()
#' generic
#'
#' @param tab The name of the Table object to use read
#'
#' @return A dply::tbl object which wraps a database table
#' @export
#'
setGeneric("table", function(db, tab) standardGeneric("table"))

#' Get the dplyr table wrapper for a table passed as a string
#'
#' @param db .
#' @param tab .
#'
#' @export
setMethod("table", c(db = "Database", tab="character"), function(db, tab) {
  dplyr::tbl(db@connection, tab)
})

#' Submit an SQL query to a database object
#'
#' @param db The database to query
#' @param query A string with the SQL query
#'
#' @return A tibble containing the results
#' @export
#'
setGeneric("sqlQuery", function(db, query) standardGeneric("sqlQuery"))

#' Perform an SQL query by directly passing the SQL string
#'
#' @param db The Database to submit to query to
#' @param query The query to submit (character string)
#'
#' @export
setMethod("sqlQuery", c("Database", "character"), function(db, query) {

  # Submit the SQL query
  res <- DBI::dbSendQuery(db@connection, query)

  # Fetch all results
  df <- DBI::dbFetch(res, n=-1)

  # Clear the results
  DBI::dbClearResult(res)

  # Return the dataframe of results as a tibble
  tibble::as_tibble(df)
})

setGeneric("sqlFromFile", function(db, file) standardGeneric("sqlFromFile"))

#' Perform an SQL query from a file
#'
#' @param db The Database to submit to query to
#' @param query The query to submit (character string)
#'
#' @export
setMethod("sqlFromFile", c("Database", "character"), function(db, file) {

  # Read the query as a string
  str <- readr::read_file(file)

  # Do the query
  sqlQuery(db, str)
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
