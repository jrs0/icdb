##'
##' 
NULL

APC <- function(table)
{
    ## Read the mapping config file
    mapping <- yaml::read_yaml(system.file("extdata", "mapping.yaml", package = "icdb"))

    
    table %>% dplyr::select(mapping$apc$episode$start) %>% head(10000) %>% icdb::run()
}

setClass(
    "MappedDB",
    contains = "list",
    slots = representation(
        mapping = "list"
    ),
    prototype = prototype(
        mapping = list()
    )    
)

logical_table_getter <- function(srv, source_database, source_table)
{
    force(srv)
    force(source_database)
    force(source_table)
    function()
    {
        ## Get the name of the logical table and fetch the table
        tbl <- srv[[source_database]][[source_table]]()
        tbl
    }
}

make_docs <- function(logical_db)
{
    list(values = logical_db)
}

MappedDB <- function(srv, mapping = system.file("extdata", "mapping.yaml", package="icdb"))
{
    ## The structure of the yaml file makes a recursive implementation of this function
    ## much more appropriate, but this will do for now. In general, it would be good if
    ## each level in the yaml file inherited fields from the level above (for example,
    ## source databases and tables), to support a more general nested structure of logical
    ## objects
    
    m <- yaml::read_yaml(mapping)

    mdb <- new("MappedDB", mapping = m)

    ## ldb stands for logical database. This loop should really be replaced by something
    ## recursive, because a new level is needed for each level in the yaml file.
    for (ldb_name in names(m))
    {
        ## Get the names of the source database and source table
        database <- m[[ldb_name]]$source_database

        mdb[[ldb_name]] <- Tables()
        mdb[[ldb_name]]@.Data <- m[[ldb_name]]$tables %>%
            purrr::map(~ Tab(logical_table_getter(srv, database, .x),
                             make_docs(m[[ldb_name]])))
        names(mdb[[ldb_name]]@.Data) <- names(m[[ldb_name]]$logical_tables)
    }

    mdb
}



##' This function parses the tree returned by reading the yaml mapping
##' file, and returns a named list of the contents of the current level
##' passed as the argument. The function is recursive, and will descend
##' through the configuration, generating a tree of documentation,
##' database and table information that acts like a logical database.
##'
##' 
##' @title Mapping-tree parser
##' @param mapping A named list storing a level of the yaml config file
##' @return A named list containing the results of parsing the file
##' 
parse_mapping <- function(mapping, srv, source_database = NULL, source_table = NULL)
{
    if ("databases" %in% names(mapping))
    {
        ## When you get to a list of databases, parse each database is turn
        d <- list()
        for (database in names(mapping$databases))
        {
            d[[database]] <- parse_mapping(mapping$databases[[database]], srv)
        }
        d
    }
    else if ("tables" %in% names(mapping))
    {
        ## If there is a tables field, then the current mapping is a logical
        ## database. Record the database name for the next function execution
        ## environment. 
        source_database <- mapping$source_database
        message("parsing a database")
        message("source database: ", source_database)
        t <- list()
        for (table in names(mapping$tables))
        {
            t[[table]] <- parse_mapping(mapping$tables[[table]], srv,
                                        source_database = source_database)
        }
        t
    }
    else if ("columns" %in% names(mapping))
    {
        ## If there is a columns field, then the current mapping is a table.
        ## Record the source table name for the next execution environment
        source_table <- mapping$source_table

        ## Check if there is a source_database key -- if there is, it must
        ## overwrite the value inherited from the calling environment, to
        ## support the possibility that tables in the same logical database
        ## in fact originate from separate source databases.
        if (!is.null(mapping$source_database))
        {
            source_database <- mapping$source_database
        }

        ## Next, create a table object for the database
        message("parsing a table")
        message("source table: ", source_table)
        message("source database: ", source_database)
        return(logical_table_getter(srv, source_database, source_table))
        
        c <- list()
        for (column in names(mapping$columns))
        {
            c[[column]] <- parse_mapping(mapping$columns[[column]], srv,
                                         source_database = source_database,
                                         source_table = source_table)
        }
        c
    }
    else if ("strategy" %in% names(mapping))
    {
        message("Parsing strategy")
        mapping$strategy  
    }
    else
    {
        stop("Error in config file: at least one of tables, columns, or strategy must be ",
             "present at each level")
    }
}

