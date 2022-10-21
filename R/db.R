##' @importFrom methods new show
##' @importFrom magrittr %>%
##' @importFrom utils capture.output tail
##' @importClassesFrom DBI DBIConnection
##' @export
NULL

##' Class for wrapping a list of tables
##'
##' The only purpose of this at the moment is to allow overloading
##' the '$' operator for table access. There might be more uses for
##' it later.
##' 
##' @title Tables class 
setClass(
    "Tables",
    contains = "list",
    slots = representation(
        # Empty
    ),
    prototype = prototype(
        # Empty
    )
)

##' Get a new Tables object, a simple wrapper around a list of
##' database tables.
##'
##' @title Make a new list of tables
##' @return The new Tables object
Tables <- function()
{
    new("Tables")
}

##' Get the tree of accessible objects in the database connection
##'
##' Objects in a database connection are stored as a tree. At the root of
##' the tree is a starting prefix, which may represent a collection of
##' databases, a database, or even a table in a database. This function
##' recursively traverses the objects in the database using DBI::dbListObjects()
##' and stores the results in a tree structure for the purpose of automcompleting
##' object names.
##'
##' The entire object tree for a database server can be quite large. This
##' causes the database connection to lag when it is first opened. However, there
##' is no way to avoid this delay if the list-based-autocompleting method is
##' used, which is currently the only implemented option. Setting a specific
##' prefix may help alleviate this performance problem if the area of interest
##' is known.
##'
##' It may be possible to set the default database prefix in the data source
##' setup (Windows).
##' 
##' @title Build the tree of accessible database object
##' @param con The database connection object
##' @param prefix The starting prefix defining the root of the tree (ID type)
##' 
##' @return Nested list structure containing the object tree
build_object_tree <- function(con, prefix)
{
    ## Get the list of objects underneath the current prefix. This
    ## is returned as two columns: a table column containing the object
    ## (a character), and a flag prefix which is TRUE if this string
    ## represents an object.
    objs <-con %>%
        DBI::dbListObjects(prefix = prefix) %>%
        tibble::as_tibble()

    ## Get the labels of the IDs
    labels <- objs %>% purrr::pmap(~ tail(.x@name,1))

    ## Create the sublists underneath the labels
    values <- objs %>% purrr::pmap(~ if(.y == TRUE) {
                                        build_object_tree(con, .x)
                                    } else {
                                        table_getter(con, .x)
                                    })

    ## Bind the labels and values into a named list and return it
    names(values) <- labels

    ## Return the list
    values
}

##' Get the table with the specified name as a dplyr::tbl
##'
##' @title Overload $ for Tables so that it returns the dplyr::tbl
##' @param x The Tables object (element in a Databases object)
##' @param name The table name to get
##' @return The dplyr::tbl for the selected table
setMethod("$", "Tables", function(x, name) {
    x[[name]]()
})

##' Databases class wrapping an SQL server connection
##'
##' @slot connection Microsoft SQL Server.
##' @slot config list. Databases connection information as a named list
##' @slot dsn Domain source name (Windows only)
##' @slot .Data From the contained list
##'
##' @export
setClass(
    "Databases",
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

##' Construct a Databases object
##'
##' This object manages a database connection. The connection is configured
##' using either a data source name, or a json file which stores
##' configuration information and credentials. After this function has run
##' without errors, you should have a new Databases object containing a valid
##' connection. The data source is specified by a data source name, which is
##' configured using a Windows program. This is the preferred connection method.
##'
##' A data source name (DNS) is a string that refers to a connection set
##' using the "ODBC data sources" app on Windows (8). Open this app; in the
##' "User DSN" tab, add a new data source using "Add". Select "ODBC Driver 17 for
##' SQL Server", and then follow through the wizard. Choose a name for the data
##' source, which is arbitrary -- this is the data source name. Set the "Server"
##' field to the server name (the same as in SQL Management Studio). In one
##' of the pages, make sure you pick a default database to connect to.
##' Finally, at the end of the wizard, check the connection. If it fails,
##' debug it before moving on to using this function.
##'
##' If you cannot make the DSN work, you can also supply a configuration file,
##' in JSON format. Three keys are required: "driver" must be set to the
##' database driver (potentially "SQL Server"); "server" must be set the to
##' server name; and "database" must be set to the name of the database to
##' connect to. An example configuration file is in extdata/db_config.json.
##'
##' Note: it is very important to select the "ODBC Driver 17 for SQL Server",
##' not the "SQL Server" driver, even though the latter may appear to work.
##' The "SQL Server" driver will fail for certain tables with long columns;
##' read here for more information: https://github.com/r-dbi/odbc/issues/309.
##' If you get errors like "Invalid Descriptor Names" when you try to read
##' tables and do queries, consider this problem.
##'
##' doneness 0
##'
##' @param data_source_name The data source name (DSN) for the database.  If
##'   this argument is provided, it will be preferred to the config file.
##'
##' @param config The absolute path of a configuration file (csv format)
##'   containing database connection information and credentials. The default
##'   value is the credential file stored in the inst/ directory.
##'
##' @return A new (S4) Databases object
##' 
##' @export
Databases <- function(data_source_name = NULL,
                     config = NULL)
{
    ## If the data source name argument was passed, connect using that
    if (!is.null(data_source_name))
    {
        message("Connecting using data source name (DSN): ", data_source_name)
        db <- new(
            "Databases",
            ## Note the bigint argument, see comment below
            connection = DBI::dbConnect(odbc::odbc(), data_source_name,
                                        bigint = "character"),
            dsn = data_source_name
        )
    }
    else if (!is.null(config))
    {
        if (!file.exists(config))
        {
            stop("The supplied config file ", config, " does not exist.")
        }
        message("Connecting using config file")

        conf <- rjson::fromJSON(file = config)
        
        ## Create the mapping from strings to drivers
        drv_map <- list(
            "ODBC Driver 17 for SQL Server" = odbc::odbc(), ## For Microsoft
            "mysql" = RMariaDB::MariaDB(), ## Both mysql and mariadb using RMariaDB
            "mariadb" = RMariaDB::MariaDB()
        )
        conf$drv <- drv_map[[conf$drv]]

        ## This parameter is really important for getting bigints
        ## (often used in ID columns) in a format that will work
        ## with dplyr (storing the bigint as a character string)
        conf$bigint <- "character"
        con <- do.call(DBI::dbConnect, conf)

        db <- new("Databases", connection = con, config = conf)
    }
    else
    {
        stop("You must provide a data source name or a config file argument.")
    }

    ## Copy the list of databases into a list, ready to store in the object
    databases <- db@connection %>%
       DBI::dbGetQuery("SELECT name FROM master.sys.databases")

    ## This is the problem part of the code -- it really needs to store a
    ## function to return the table object, but that doesn't work (yet).
    for (d in databases$name)
    {
        ## Get the list of tables associated with this database.
        ## Need to double check that this catalog_name is the right
        ## argument to specify the database name for all backends.
        tables <- db@connection %>% DBI::dbListTables(catalog_name = d)

        ## Store the tables under a named entry for the database
        db[[d]] <- Tables()
        for (tabname in tables)
        {
            db[[d]][[tabname]] <- table_getter(db, d, tabname)
        }
    }

    db
}
##' Function factory returning a function that gets a dplyr::tbl 
##'
##' This function is the way to associate a function with every table
##' object in the db list, and replaces the need to overload `$`, while
##' also avoiding the problem of storing the table name and database
##' name in the functions environment. This is the purpose of a function
##' factory; to capture variables in the enclosing environment and allow
##' them to persist when the function is called. Read this page and the
##' associated environment sections for a full explanation:
##' https://adv-r.hadley.nz/function-factories.html
##'
##' This function needs a bit of work -- this is only a first draft.
##' The main issue is hard coding the dbo object, which should really
##' come from somewhere and is probably going to lead to bugs down the
##' line. When I find a failure case, it can be fixed here. Currently, the
##' function just forms the name database.dbo.tabname, which seems to work
##' fine. However, it would be better to understand what the catalog and
##' schema really are and make this robust.
##'
##' @title Get a function which returns a table object
##' @param db The database object to use (containing the connection)
##' @param database The database name
##' @param tabname The table name
##' 
table_getter <- function(db, database, tabname)
{
    force(database)
    force(tabname)
    function()
    {
        dplyr::tbl(db@connection,
                   dbplyr::in_catalog(database, "dbo", tabname))
    }
}

##' Search the tables and columns in a database for partial names
##'
##' Several of the databases are very large, with hundreds of tables each
##' with hundreds of columns. This function is designed to help with finding
##' relevant columns in the tables of the database, by partially matching the
##' table and column names and printing the results.
##'
##' @param db The Databases object to query
##' @param col_pattern The pattern to match column names (regexp)
##' @param tab_pattern The pattern to match table names (regexp)
##'
##' @export
##'
setGeneric("searchCols", function(db, col_pattern, tab_pattern)
    standardGeneric("searchCols"))

##' Search the tables and columns in a database for partial names
##'
##' @param db The Databases object to query
##' @param col_pattern The pattern to match column names (regexp)
##' @param tab_pattern The pattern to match table names (regexp)
##'
##' @export 
setMethod("searchCols", "Databases",
          function(db, col_pattern, tab_pattern) {

              ## Filter table names
              tab_matches <- grep(tab_pattern, db, value=TRUE)

              for (t in tab_matches) {
                  tryCatch(
                      expr = {
                          tbl <- table(db, t)
                          names <- colnames(tbl)
                          col_matches <- grep(col_pattern, names, value=TRUE)
                          if (length(col_matches) > 0) {
                              print(paste0("Found these colums in table '", t, "':"))
                              writeLines(paste0("  ", utils::capture.output(print(col_matches))))
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
setMethod("dsn", "Databases", function(x) {
    x@dsn
})

setGeneric("tables", function(x)
    standardGeneric("tables"))
setMethod("tables", "Databases", function(x) {
    DBI::dbListTables(x@connection)
})

## Access a table in a Databases object
##
## Use this function to access a table in the Databases object. The Databases
## object behaves like a list, but the elements of the list are evaluated only
## when this function is called. When a call like db$table_name is made, this
## function creates the corresponding dplyr::tbl object and returns it. Since
## this is not a particularly long operation, the results do not need to be
## cached. Use the resulting object for any processing you can do with
## dplyr::tbl, e.g. pipe it to some SQL-type queries and collect().
##
## @param x The database
## @param name The table to access
##
## @return A dplyr::tbl data source wrapper
## @export


##' Get a table in the database in a form ready for dplyr processing
##'
##' Use this function to obtain a dplyr::tbl (a kind of shell object) from a
##' table in the database. This object does not contain the full data to begin
##' with. Instead, it can be used as the basis of dplyr-based SQL queries,
##' finishing with a %>% collect() call which does actually produce data. This
##' function is an alternative to using a direct sql query using the sql()
##' generic
##'
##' @param db The database to read from
##' @param tab The name of the table object to use read
##'
##' @return A dply::tbl object which wraps a database table
##' @export
##'
setGeneric("table", function(db, tab) standardGeneric("table"))

##' Get the dplyr table wrapper for a table passed as a string
##'
##' @param db .
##' @param tab .
##'
##' @export
setMethod("table", c(db = "Databases", tab="character"), function(db, tab) {
    dplyr::tbl(db@connection, tab)
})

##' Submit an SQL query to a database object
##'
##' @param db The database to query
##' @param query A string with the SQL query
##'
##' @return A tibble containing the results
##' @export
##'
setGeneric("sqlQuery", function(db, query) standardGeneric("sqlQuery"))

##' Perform an SQL query by directly passing the SQL string
##'
##' Submit an SQL query to the Databases and obtain the results of the query in
##' a tibble dataframe. The query results are cached in a file on the disk,
##' so that if do the same query a second time, the local results are used.
##'
##' @param db The Databases to submit to query to
##' @param query The query to submit (character string)
##'
##' @export
setMethod("sqlQuery", c("Databases", "character"), function(db, query) {

    ## Search for the cached file
    result <- read_cache(query)
    if (!is.null(result))
    {
        message("Found cached results for this query, using that")

        ## Return the cached data
        result
    }
    else
    {
        ## Record the start time
        start <- lubridate::now()
        
        ## Submit the SQL query
        ##
        ## It appears there is a bug in odbc (or maybe somewhere else) relating
        ## to getting tables that have long nvarchar fields:
        ## https://stackoverflow.com/questions/45001152/r-dbi-odbc-error
        ## -nanodbc-nanodbc-cpp3110-07009-microsoftodbc-driver-13-fo
        ##
        ## Trying to get columns from these tables causes the dbFetch routine
        ## to fail, and corrupts the connection. The issue likely affects the
        ## dplyr collect() too. The solution seems to be some kind of
        ## reordering the select elements in the SQL query to put these fields
        ## at the end.
        ##
        ## This issues also affects the table() generic for accessing the
        ## table using e.g. db$table_name, because that probably does a
        ## query behind the scenes.
        ##
        ## See also: https://github.com/r-dbi/odbc/issues/309
        ##
        res <- DBI::dbSendQuery(db@connection, query)

        ## Fetch all results
        df <- DBI::dbFetch(res, n=-1)

        ## Clear the results
        DBI::dbClearResult(res)

        ## Create a tibble from the dataframe
        t <- tibble::as_tibble(df)

        ## Save the results in the cache
        write_cache(query, t, lubridate::now() - start)

        ## Return the dataframe of results as a tibble
        t
    } 
})

##' Submit an SQL query from a file and get the results
##'
##' @param db The Databases object to submit the query to
##' @param file The path to the file containing SQL
##'
##' @return A tibble containing the results
##' @export
setGeneric("sqlFromFile", function(db, file) standardGeneric("sqlFromFile"))

##' Perform a SQL query from a file
##'
##' If you have a file of SQL, submit it as a query to the database using this
##' function. It reads the file into a string and passes it to the sqlQuery
##' function. This function also caches the results of the query in a file
##' so that next time the function is called, the query will attempt to use
##' the cached file before using the SQL server. See the documentation for
##' sqlQuery for more information about how this works.
##'
##' @param db The Databases to submit to query to
##' @param file The file containing the query to submit
##'
##' @export
setMethod("sqlFromFile", c("Databases", "character"), function(db, file) {

    ## Read the query as a string
    str <- readr::read_file(file)

    ## Do the query
    sqlQuery(db, str)
})

##' Print out the Databases object
##'
##' @docType methods
##' @aliases show-Databases show, Databases-method
##'
##' @param object The object to be printed
##' @export
setMethod("show", "Databases", function(object) {
    message("Wrapper around database connection")
    message("Databases name: ", object@connection@info$dbname)
    if (!is.na(object@dsn))
    {
        message("Database server connection via data source name (DSN): ", object@dsn)
    }
    else
    {
        message("Database server connection via config file")
    }
})
##' This function is a replacement for the dplyr::collect() function
##' that is used to submit an SQL query to a database. This function
##' is called in the same way, by piping to collect(), but it caches
##' the query behind the scenes, so that the next time the query is
##' fetched, the results are available quicker.
##'
##' In order to avoid ambiguity caused by the order in which icdb and
##' dplyr are loaded, this function is not called collect. You can still
##' use collect, but 
##' 
##' @title Collect and cache SQL query results
##' @param x The dplyr SQL query to collect
##' @param ... Other arguments for dplyr::collect()
##' @return The query results
##'
##' @export
run <- function(x, ...)
{
    ## Generate an SQL string for the query
    output <- capture.output(x %>% dplyr::show_query())
    query <- paste(tail(output, -1), collapse="")    
    
    ## Search for the cached file
    result <- read_cache(query)
    if (!is.null(result))
    {
        message("Found cached results for this query, using that")

        ## Return the cached data
        result
    }
    else
    {
        ## Record the start time
        start <- lubridate::now()
        
        ## Get the results
        t <- x %>% dplyr::collect(...)

        ## Save the results in the cache
        write_cache(query, t, lubridate::now() - start)

        ## Return the dataframe of results as a tibble
        t
    } 
}
