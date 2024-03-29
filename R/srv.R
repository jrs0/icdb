## Information for developers:
##
## The structure of the server object is the most important part of this
## file. The server is a nested list of list, where each level corresponds
## to an object in the database (first databases, then tables -- schemas
## are omitted for now). The innermost list contains functions which
## return table_node objects. These objects inherit from dplyr::tbl, and
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
                                         table_wrapper(make_table_getter(con,
                                                                         id = .x))
                                     })

    ## Bind the labels and values into a named list and return it
    names(values) <- labels

    ## Return the list
    node(values)
}

new_table_node <- function(tbl, ..., class=character())
{
    structure(tbl,
              class=c("table_node", class, class(tbl))
              )
}

##' Make a new table_node object, which is the basic type used to
##' store tables in the library. This function should be
##' used in the table_getter to give a tibble object back
##' to the user.
##'
##' @title Make a table_node
##' @param tbl The underlying tbl to use
##' @param ... Further arguments from other constructors
##' @param class Other classes (used for subclasses)
##' @return The new table_node object
##'
table_node <- function(tbl, ..., class=character())
{
    new_table_node(tbl, ..., class=class)
}

##' The server object is a nested list of nodes, which each
##' represent an object in the database. This is the S3 class
##' for a node.
##'
##' The one job of the node is to correctly handle the `$`
##' overload for the object being accessed in the object
##' tree. For the server, `$` is overloaded to list the top
##' level objects. All other objects in the tree are either
##' nodes or table_nodes. The `$` operator for a node is overloaded
##' in such a way that it checks whether the node below it is
##' a node or a table_node. If it is a node, it prints a list of the
##' contents of that node. If it is a table_node, it automatically
##' fetches the dplyr::tbl.
##'
##' The node object is a named list of either nodes or table_nodes.
##'
##' @title node object for intermediate positions in the object tree.
##' @return A new node S3 object
##'
##' @param subobjects The list of nodes or table_nodes in this node
##' @param ... Other parameters for derived classes
##' @param class The class parameter for derived class
new_node <- function(subobjects, ..., class=character())
{
    structure(subobjects,
              class=c("node", class)
              )
}

node <- function(subobjects, ..., class=character())
{
    new_node(subobjects, ..., class=class)
}


##' @export
print.node <- function(x, ...)
{
    print(names(x))
}

##' @export
`$.node` <- function(x,i)
{
    obj <- NextMethod()
    if (methods::is(obj,"node"))
    {
        ## Return for printing or otherwise
        obj
    }
    else if (methods::is(obj, "table_wrapper"))
    {
        ## Call the object, which returns a table, and
        ## return the result
        obj()
    }
    else
    {
        stop("Unexpected object class in node while calling `$`")
    }
}


new_table_wrapper <- function(table_getter)
{
    structure(table_getter, class = "table_wrapper")
}

table_wrapper <- function(table_getter)
{
    new_table_wrapper(table_getter)
}

setOldClass("node")

##' server class wrapping an SQL server connection
##'
##' @slot con Microsoft SQL server.
##' @slot .S3Class The inherited node object
##'
##' @export
##'
setClass(
    "server",
    contains = "node",
    slots = representation(
        con = "DBIConnection"
        ## config = "list",
        ## dsn = "character"
    ),
    prototype = prototype(
        con = NULL
        ## config = list(),
        ## dsn = NA_character_
    )
)

##' Construct a server object
##'
##' This object manages a connection to a database server. The connection
##' is configured using either a data source name, or a YAML file which stores
##' configuration information and credentials. After this function has run
##' without errors, you should have a new server object containing a valid
##' connection. The data source is specified by a data source name, which is
##' configured using a Windows program. This is the preferred connection method.
##'
##' A data source name (DNS) is a string that refers to a connection set
##' using the "ODBC data sources" app on Windows (8). See the package
##' documentation for more information about how to set this up.
##'
##' If you cannot make the DSN work, you can also supply a configuration file,
##' in YAML format. Three keys are required: "driver" must be set to the
##' database driver (potentially "SQL server"); "server" must be set the to
##' server name; and "database" must be set to the name of the database to
##' connect to.
##'
##' @param data_source_name The data source name (DSN) for the database.  If
##' this argument is provided, it will be preferred to the config file.
##' @param config The path of a configuration file (YAML format)
##' containing database connection information and credentials. The default
##' value is the credential file stored in the inst/ directory.
##' @param interactive This parameter specifies whether the list of databases
##' and tables is populated. This is used to make the MappedSrv object load
##' faster, by only fetching the tables specified in the mapping.
##' @return A new (S4) server object
##'
##' @export
server <- function(data_source_name = NULL,
                   config = NULL,
                   interactive = TRUE)
{
    ## If the data source name argument was passed, connect using that
    if (!is.null(data_source_name))
    {
        message("Connecting using data source name (DSN): ", data_source_name)
        ## Make sure to pass bigint = character to avoid using 64-bit ints
        ## in R.
        con <- DBI::dbConnect(odbc::odbc(), data_source_name,
                              bigint = "character")
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
                message(cnd$message)
                stop("Could not connect to server because of the YAML ",
                     "parsing error above", call.=FALSE)
            },
            conf <- yaml::read_yaml(config)
        )

        ## Store the driver name to use below
        driver_name <- conf$driver

        ## Create the mapping from strings to drivers
        drv_map <- list(
            "ODBC Driver 17 for SQL Server" = odbc::odbc(), ## For Microsoft
            "SQL Server Native Client 11.0" = odbc::odbc(), ## For Microsoft
            "mysql" = RMariaDB::MariaDB(), ## Both mysql and mariadb using RMariaDB
            "mariadb" = RMariaDB::MariaDB(),
            "sqlite" = RSQLite::SQLite()
        )

        ## Check if the database is specified as a file or as a name.
        ## If a file is used, expand to the path of the file
        if (!is.null(conf$dbfile))
        {
            if (fs::is_file(conf$dbfile))
            {
                ## Use the file in the current directory
                conf$dbname <- conf$dbfile
            }
            else
            {
                stop("Could not find database file '", conf$dbfile,
                     "' in the current working directory")
            }

            ## Remove the dbfile name from the conf
            conf$dbfile <- NULL
        }

        ## The conf file will be used as arguments in DBI::dbConnect.
        ## First, replace the driver element with the drv object
        conf$drv <- drv_map[[conf$driver]]

        ## For non-Microsoft databases, remove the driver key.
        if (!grepl("SQL Server", conf$driver))
        {
            conf$driver <- NULL
        }

        if (is.null(conf$drv))
        {
            stop("Unrecognised driver '", conf$driver,
                 "' specified in config file '", config,
                 call.=FALSE)
        }

        ## This parameter is really important for getting bigints
        ## (often used in ID columns) in a format that will work
        ## with dplyr (storing the bigint as a character string)
        conf$bigint <- "character"
        print(conf)

        ## Open the database connection
        con <- do.call(DBI::dbConnect, conf)
    }
    else
    {
        stop("You must provide a data source name or a config file argument.")
    }

    ## When you get here, the connection is open. Return if interactive mode is
    ## disabled. If interactive mode is enabled, then move on to the next stage
    ## which constructs the nested list
    if (interactive == FALSE)
    {F
        return(new("server", node(list()), con = con))
    }

    ## Most database drivers return the databases and tables as a tree of objects,
    ## via the dbListObjects function. SQL server does not work like this, so treat
    ## it separately
    if (!is.null(data_source_name) || grepl("SQL server", driver_name))
    {
        ## Create a top level node object
        node <- node(list())

        ## Copy the list of databases into a list, ready to store in the object
        databases <- con %>%
            DBI::dbGetQuery("SELECT name FROM master.sys.databases")

        ## This is the problem part of the code -- it really needs to store a
        ## function to return the table object, but that doesn't work (yet).
        for (d in databases$name)
        {
            tryCatch (
            {
                ## Get the list of tables associated with this database,
                ## and their associated schemas
                tables <- con %>%
                    DBI::dbGetQuery(paste0("SELECT table_schema,table_name FROM ", d,
                                           ".INFORMATION_SCHEMA.TABLES"))

                ## Put the tables in the database
                node[[d]] <- tables %>%
                    purrr::pmap(~ table_wrapper(make_table_getter(con, d, .x, .y))) %>%
                    node()

                names(node[[d]]) <- tables$table_name
            },
            error = function(cond)
            {
                ## Currently, do nothing. This is a quick way to ensure that access problems
                ## do not break the code.
                ## TODO  Come back and fix this to read permissions
                ##
                ## NOTE TO DEVELOPERS: this clause will catch any errors, even ones while
                ## you are developing. Uncomment the line below if something is going wrong
                ## and you want to see what.
                ##print(cond)
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
        node <- build_object_tree(con, NULL)
    }

    ## Now create and return the server object
    new("server", node, con = con)
}

##' This is the function for obtaining a table in non-interactive Databases mode.
##' The purpose of this function is to speed up the initial loading of the
##' Databases object, in an non-interactive environment. However, that means that
##' it is necessary to recheck the database server for the schema name each
##' time a table is requested. This does not add too much overhead.
##'
##' Currently, this function only supports Microsoft SQL server.
##'
##' @title Get a dplyr::tbl corresponding to a table
##' @param srv The Databases object containing the server connection
##' @param source A named list containing a table, optionally a schema, and
##' optionally a database.
##' @return The dplyr::tbl (shell) for the table
##'
get_tbl <- function(srv, source)
{
    if (!("table" %in% names(source)))
    {
        stop("You must specify at least 'table' in source (get_tbl)")
    }

    if ("schema" %in% names(source))
    {
        ## e.g., for Microsoft
        id <- rlang::exec(dbplyr::in_catalog, !!!source)
    }
    else if ("catalog" %in% names(source) & !"schema" %in% names(source))
    {
        ## e.g. for a mysql database that only has tables under catalog names, and no schema
        ## basically added without much testing to make Nick's home database work
        source$schema <- source$catalog
        source$catalog <- NULL
        id <- rlang::exec(dbplyr::in_schema, !!!source)
    }
    else if ("catalog" %in% names(source))
    {
        ## e.g., for mysql
        id <- rlang::exec(dbplyr::in_schema, !!!source)
    }
    else
    {
        ## e.g., for sqlite
        id <- source$table
    }

    dplyr::tbl(srv@con, id)
}


##' Make a function that returns a table getter. The function which
##' is returned can be called to produce a table_node object.
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
##' Even though the code uses () to access the table name, this level
##' of indirection is still necessary because of the bug referenced
##' in the build_object_tree body -- using a table getter means that
##' the tbl is only created when the user asks for it, removing some
##' edge cases with "bad" tables (like COLUMNS_EXTENSIONS in mysql
##' information_schema, which contains JSON columns).
##'
##' @title Get a function which returns a table object
##' @param con The database connection (from server)
##' @param database The database name
##' @param table_schema The table schema name
##' @param table_name The table name
##' @param id You can also pass the complete Id, if it is available
##'
make_table_getter <- function(con, database, table_schema, table_name, id = NULL)
{
    force(con)
    if (is.null(id))
    {
        force(database)
        force(table_schema)
        force(table_name)
    }
    else
    {
        force(id)
    }

    function()
    {
        ## Create the reference to the table in the database
        if (is.null(id))
        {
            id <- dbplyr::in_catalog(database, table_schema, table_name)
        }

        ## Get the table shell object
        tbl <- dplyr::tbl(con, id)

        ## Return th table_node object wrapping the tbl
        table_node(tbl)
    }
}

##' Submit an SQL query to a database object
##'
##' @param db The database to query
##' @param query A string with the SQL query
##'
##' @return A tibble containing the results
##' @export
##'
setGeneric("sql_query", function(db, query) standardGeneric("sql_query"))

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
setMethod("sql_query", c("server", "character"), function(db, query) {

    ## Search for the cached file
    result <- read_cache(query)
    if (!is.null(result))
    {
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
setGeneric("sql_from_file", function(db, file) standardGeneric("sql_from_file"))

##' Perform a SQL query from a file
##'
##' If you have a file of SQL, submit it as a query to the database using this
##' function. It reads the file into a string and passes it to the sql_query
##' function. This function also caches the results of the query in a file
##' so that next time the function is called, the query will attempt to use
##' the cached file before using the SQL server. See the documentation for
##' sql_query for more information about how this works.
##'
##' @param db The Databases to submit to query to
##' @param file The file containing the query to submit
##'
##' @export
setMethod("sql_from_file", c("server", "character"), function(db, file) {

    ## Read the query as a string
    str <- readr::read_file(file)

    ## Do the query
    sql_query(db, str)
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
setMethod("show", "server", function(object) {
    print(object)
})

##' This function is a replacement for the dplyr::collect() function
##' that is used to submit an SQL query to a database. This function
##' is called in the same way, by piping to collect(), but it caches
##' the query behind the scenes, so that the next time the query is
##' fetched, the results are available quicker.
##'
##' The cache is disabled by default. Make sure you call use_cache
##' (see the documentation) if you want to use the cache.
##'
##' You can use the lifetime parameter to overwrite the cache lifetime
##' specified in the arguments to use_cache().
##'
##' @title Collect and cache SQL query results
##' @param x The dplyr SQL query to collect
##' @param lifetime The default amount of time that cache results
##' will remain valid. Specified as a lubridate duration (e.g.
##' lubridate::dhours(24)), with default value 24 hours.
##' @param ... Other arguments for dplyr::collect()
##' @return The query results
##'
##' @export
run <- function(x, lifetime = NULL, ...)
{
    ## Generate an SQL string for the query
    output <- capture.output(x %>% dplyr::show_query())
    query <- paste(tail(output, -1), collapse="")

    ## Search for the cached file
    result <- read_cache(query, lifetime)

    if (!is.null(result))
    {
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
