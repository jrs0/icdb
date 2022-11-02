## Information for developers:
##
## The structure of the Server object is the most important part of this
## file. The Server is a nested list of list, where each level corresponds
## to an object in the database (first databases, then tables -- schemas
## are omitted for now). The innermost list contains functions which
## return Table objects. These objects inherit from dplyr::tbl, and
## form the base for other classes that contain additional information.
##
## The need to store functions in the nested list is to prevent a
## potentially large number of SQL queries for large databases, and
## also to avoid bugs (e.g. mysql) with some of the tables not working
## with DBI (the backend for this library).
## 
##
##



##' @importFrom methods new show
##' @importFrom magrittr %>%
##' @importFrom utils capture.output tail
##' @importClassesFrom DBI DBIConnection
##' @export
NULL


##' Make helper functions to turn on and off the cache
make_cache_fns <- function()
{
    ## Global flag indicating whether caching should be used or not
    cache_flag <- FALSE

    ## Return the use_cache and get_cache_flag function in a vector
    c(
        use_cache = function(choice)
        {
            cache_flag <<- choice
        },
        get_cache_flag = function()
        {
            cache_flag
        }
    )
}

cache_fns <- make_cache_fns()

##' Use this function to turn on or off caching. If caching is
##' enabled, then query results will be saved behind the scenes.
##' If caching is disabled, queries will always return results
##' directly from the database. Caching is disabled by default.
##' You must explicitly call this function if you want to use it.
##' You only need to call this function once, when you load the
##' ICDB library using library(icdb).
##' 
##' If caching is enabled, and you perform exactly the same query
##' again (for example, by running the same script), then the
##' cached results will be used, which will speed up the running
##' of the script. This can make development and debugging easier,
##' because the turnaround time for running commands and scripts
##' is reduced.
##'
##' If caching is disabled, then results will always come from the
##' database. This can make code take longer to run, but will
##' always guarantee that results are up to date.
##'
##' @title Turn on or off caching
##' @param choice TRUE will turn on the cache, FALSE will turn it off
##' 
##' @export
use_cache <- cache_fns$use_cache

##' Internal function for getting the value of the cache flag
get_cache_flag <- cache_fns$get_cache_flag

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

    ## TODO: when parsing mysql information_schema, the table
    ## COLUMNS_EXTENSIONS contains JSON fields, which are not
    ## supported by RMariaDB currently (see
    ## https://www.rapids.science/1.9/common-errors/. See also
    ## https://bugzilla.redhat.com/show_bug.cgi?id=1546113.
    ##
    ## This error has become a problem because of the eager
    ## evaluation of the dplyr::tbl -- before, it only
    ## tried to create the tbl when it was requested, which
    ## meant the error never showed up (because no-one selected
    ## information_schema.COLUMNS_EXTENSIONS). For now, I think
    ## going back to this situation is better, and the real
    ## solution can be left for another time (especially because
    ## there might not be any solution using DBI).
    ##
    
    ## Create the sublists underneath the labels
    values <- objs %>% purrr::pmap(~ if(.y == TRUE) {
                                        build_object_tree(con, .x)
                                    } else {
                                        make_table_getter(con, .x)
                                    })

    ## Bind the labels and values into a named list and return it
    names(values) <- labels

    ## Return the list
    values
}

##' Simple wrapper to print a user error. This class inherits from
##' function, which means it is expecting to be called. The purpose
##' of wrapping this in a class is to throw an error in the show
##' method, to catch the case where a user tries to view a table
##' by just writing table name without (). Without this, the user
##' would just get the body of the function, which is not really very
##' helpful for debugging.
setClass(
    "TableGetter",
    contains = "function",
    slots = representation(
        ## Nothing here
    ),
    prototype = prototype(
        ## Nothing here
    )
)

setMethod("show", "TableGetter", function(object) {
    stop("You must use parentheses () after the table name to get the tibble.")
})

##' Make a function that returns a table getter. The function which
##' is returned can be called to produce a dplyr::tbl.
##'
##' Even though the code uses () to access the table name, this level
##' of indirection is still necessary because of the bug referenced
##' in the build_object_tree body -- using a table getter means that
##' the tbl is only created when the user asks for it, removing some
##' edge cases with "bad" tables (like COLUMNS_EXTENSIONS in mysql
##' information_schema, which contains JSON columns).
##'
##' @title Make a table getter
##' @param con The connection to the database
##' @param id The id (from DBI::dbListObjects) referencing the table
##' @return A function which, when called, returns a dplyr::tbl
##' 
make_table_getter <- function(con, id)
{
    force(con)
    force(id)
    function()
    {
        dplyr::tbl(con, id)
    }
}

##' Server class wrapping an SQL server connection
##'
##' @slot connection Microsoft SQL Server.
##' @slot config list. Server connection information as a named list
##' @slot dsn Domain source name (Windows only)
##' @slot .Data From the contained list
##'
##' @export
setClass(
    "Server",
    contains = "list",
    slots = representation(
        con = "DBIConnection",
        config = "list",
        dsn = "character"
    ),
    prototype = prototype(
        con = NULL,
        config = list(),
        dsn = NA_character_
    )
)

##' Construct a Server object
##'
##' This object manages a connection to a database server. The connection
##' is configured using either a data source name, or a json file which stores
##' configuration information and credentials. After this function has run
##' without errors, you should have a new Server object containing a valid
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
##' @param interactive This parameter specifies whether the list of databases
##' and tables is populated. 
##' 
##' @return A new (S4) Databases object
##' 
##' @export
Server <- function(data_source_name = NULL,
                   config = NULL,
                   interactive = TRUE)
{
    ## If the data source name argument was passed, connect using that
    if (!is.null(data_source_name))
    {
        message("Connecting using data source name (DSN): ", data_source_name)
        db <- new(
            "Server",
            ## Note the bigint argument, see comment below
            con = DBI::dbConnect(odbc::odbc(), data_source_name,
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

        tryCatch(
            error = function(cnd)
            {
                stop("An error occured parsing the config file '",
                     config,
                     "' supplied to Databases()", call.=FALSE)
            },
            conf <- rjson::fromJSON(file = config)
        )

        ## Create the mapping from strings to drivers
        drv_map <- list(
            "ODBC Driver 17 for SQL Server" = odbc::odbc(), ## For Microsoft
            "mysql" = RMariaDB::MariaDB(), ## Both mysql and mariadb using RMariaDB
            "mariadb" = RMariaDB::MariaDB(),
            "sqlite" = RSQLite::SQLite()
        )

        ## Duplicate the config file to use as arguments in DBI::dbConnect
        conf_args <- conf
        drv <- drv_map[[conf$driver]]
        if (!is.null(drv))
        {
            conf_args$drv <- drv
        }
        else
        {
            stop("Unrecognised driver '", conf$driver,
                 "' specified in config file '", config,
                 call.=FALSE)
        }
        
        ## This parameter is really important for getting bigints
        ## (often used in ID columns) in a format that will work
        ## with dplyr (storing the bigint as a character string)
        conf$bigint <- "character"

        ## If the testdata flag is true, then replace the dbname with
        ## the correct path to the database
        if (conf$testdata == TRUE)
        {
            conf_args$dbname <- system.file("testdata", conf_args$dbname, package="icdb")
        }
        
        ## Open the database connection
        con <- do.call(DBI::dbConnect, conf_args)

        db <- new("Server", con = con, config = conf)
    }
    else
    {
        stop("You must provide a data source name or a config file argument.")
    }

    ## When you get here, the connection is open. Return if interactive mode is
    ## disabled. If interactive mode is enabled, then move on to the next stage
    ## which constructs the nested list
    if (interactive == FALSE)
    {
        return(db)
    }
    
    ## Most database drivers return the databases and tables as a tree of objects,
    ## via the dbListObjects function. SQL Server does not work like this, so treat
    ## it separately
    if (!is.na(db@dsn) || grepl("SQL Server", conf$driver))
    {
        ## Copy the list of databases into a list, ready to store in the object
        databases <- db@con %>%
            DBI::dbGetQuery("SELECT name FROM master.sys.databases")
        
        ## This is the problem part of the code -- it really needs to store a
        ## function to return the table object, but that doesn't work (yet).
        for (d in databases$name)
        {
            tryCatch (   
            {
                ## Get the list of tables associated with this database,
                ## and their associated schemas
                tables <- db@con %>%
                    DBI::dbGetQuery(paste0("SELECT table_schema,table_name FROM ", d,
                                           ".INFORMATION_SCHEMA.TABLES"))
                
                ## Put the tables in the database
                db[[d]] <- tables %>% purrr::pmap(~ make_table_getter(db, d, .x, .y))
                names(db[[d]]) <- tables$table_name
            },
            error = function(cond)
            {
                ## Currently, do nothing. This is a quick way to ensure that access problems
                ## do not break the code.
                ## TODO  Come back and fix this to read permissions
            }        
            )
        }
    }
    else
    {
        ## All other database drivers support the object tree approach -- use that
        ## to populate the nested list. Currently, these lists will store an
        ## evaluated dplyr::tbl because it is not possible to overload $ in this
        ## case.
        ## TODO think up a method to make lazy evaluation of list items work here
        db@.Data <- build_object_tree(con, NULL)
    }
        
        db
}

##' This is the function for obtaining a table in non-interactive Databases mode.
##' The purpose of this function is to speed up the initial loading of the
##' Databases object, in an non-interactive environment. However, that means that
##' it is necessary to recheck the database server for the schema name each
##' time a table is requested. This does not add too much overhead.
##'
##' Currently, this function only supports Microsoft SQL Server
##' 
##' @title Get a dplyr::tbl corresponding to a table
##' @param srv The Databases object containing the server connection
##' @param database The name of the database
##' @param table The name of the table
##' @return The dplyr::tbl (shell) for the table
##' 
get_tbl <- function(srv, database, table)
{
    res <- srv@con %>%
        DBI::dbGetQuery(paste0("SELECT table_schema FROM ",
                               database,
                               ".INFORMATION_SCHEMA.TABLES WHERE table_name = '",
                               table, "'"))

    ## Pick the first schema in the list. This is potentially a bug
    ## TODO Come back and look at this
    schema <- res$table_schema[[1]]   

    ## Get the table
    dplyr::tbl(srv@con, dbplyr::in_catalog(database,
                                           schema,
                                           table))    
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

##' @title Get a function which returns a table object
##' @param srv The Server object to use (containing the connection)
##' @param database The database name
##' @param table_schema The table schema name
##' @param table_name The table name
##' 
make_table_getter <- function(srv, database, table_schema, table_name)
{
    force(srv)
    force(database)
    force(table_schema)
    force(table_name)
    function()
    {
        ## Create the reference to the table in the database
        id <- dbplyr::in_catalog(database, table_schema, table_name)
        ## Get the table shell object
        dplyr::tbl(srv@con, id)
    }
}

##' Search the tables and columns in a database for partial names
##'
##' Several of the databases are very large, with hundreds of tables each
##' with hundreds of columns. This function is designed to help with finding
##' relevant columns in the tables of the database, by partially matching the
##' table and column names and printing the results.
##'
##' @param srv The Server object to query
##' @param col_pattern The pattern to match column names (regexp)
##' @param tab_pattern The pattern to match table names (regexp)
##'
##' @export
##'
setGeneric("grep", function(srv, col_pattern, tab_pattern)
    standardGeneric("grep"))

##' Search the tables and columns in a database for partial names
##'
##' @param db The Databases object to query
##' @param col_pattern The pattern to match column names (regexp)
##' @param tab_pattern The pattern to match table names (regexp)
##'
##' @export 
setMethod("grep", "Server",
          function(srv, col_pattern, tab_pattern) {

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
setMethod("sqlQuery", c("Server", "character"), function(db, query) {

    ## Search for the cached file, if caching is enabled
    result <- NULL
    if (get_cache_flag() == TRUE)
    {
        result <- read_cache(query)
    }
    
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
        res <- DBI::dbSendQuery(db@con, query)

        ## Fetch all results
        df <- DBI::dbFetch(res, n=-1)

        ## Clear the results
        DBI::dbClearResult(res)

        ## Create a tibble from the dataframe
        t <- tibble::as_tibble(df)

        ## Save the results in the cache
        if (get_cache_flag() == TRUE)
        {
            write_cache(query, t, lubridate::now() - start)
        }
        
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
setMethod("sqlFromFile", c("Server", "character"), function(db, file) {

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
##' TODO: This function needs improvement -- at the moment it is really just a
##' placeholder for something more useful.
##' 
##' @param object The object to be printed
##' @export
setMethod("show", "Server", function(object) {
    if (!is.na(object@dsn) || grepl("SQL Server", object@config$driver))
    {
        message("Database connection (Microsoft SQL Server)")
        message("Databases name: ", object@con@info$dbname)
        if (!is.na(object@dsn))
        {
            message("Database server connection via data source name (DSN): ", object@dsn)
        }
        else
        {
            message("Database server connection via config file")
        }
    }
    else
    {
        message("Database connection (Other database)")
        message("Database: ", object@con@dbname)
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
    
    ## Search for the cached file, if caching is enabled
    result <- NULL
    if (get_cache_flag() == TRUE)
    {
        result <- read_cache(query)
    }
    
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
        if (get_cache_flag() == TRUE)
        {
            write_cache(query, t, lubridate::now() - start)
        }
        
        ## Return the dataframe of results as a tibble
        t
    } 
}
