##'
##' 
NULL

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

logical_table_getter <- function(srv, source_database, source_table, columns)
{
    force(srv)
    force(source_database)
    force(source_table)
    force(columns)
    
    function()
    {
        ## Get the name of the logical table and fetch the table
        tbl <- get_tbl(srv, source_database, source_table)

        ## The first step is to select the relevant real columns
        real_columns <- list()
        for (logical_column in columns)
        {
            real_columns <- c(real_columns, names(logical_column$source_columns))
        }
        tbl <- tbl %>% dplyr::select(all_of(unlist(real_columns)))
        
        ## Next, rename the columns according to names derived from the logical
        ## column name
        for (logical_column_name in names(columns))
        {
            logical_column <- columns[[logical_column_name]]
            
            ## Loop over the constituent columns that make up the logical column
            count <- 1
            for (old_name in names(logical_column$source_columns))
            {
                new_name <- paste0(logical_column_name,"_",count)
                tbl <- tbl %>% dplyr::rename_with(~ new_name, old_name)
                count <- count + 1 
            }
        }

        
        tbl
    }
}

make_table_docs <- function(table)
{
    str <- "\n-- Documentation for logical table object ---\n\n"
    str <- str %>% paste0(table$docs, "\n\n")

    ## Document all the logical column names
    str <- str %>% paste0("Documentation for logical column names:\n\n")
    for (logical_column_name in names(table$columns))
    {
        logical_column <- table$columns[[logical_column_name]]
        str <- str %>% paste0("\t", logical_column_name, ":\n")
        str <- str %>% paste0("\t\t", logical_column$docs, ".\n")
        str <- str %>% paste0("\t\tUnderlying database columns:\n")

        for (real_column_name in names(logical_column$source_columns))
        {
            str <- str %>% paste0("\t\t\t", real_column_name, "\n")
        }

        str <- str %>% paste0("\t\tReduce strategy: ", logical_column$strategy, ".\n")

    }

    str
}

##' Create a new mapped database object. A mapped database is an object that contains
##' logical databases, logical tables and logical column names, with documentation.
##' The structure of the database is defined by a mapping.yaml file, which describes
##' how to obtain the fields from an underlying data source.
##'
##' To make a mapped database, pass the Data Source Name (see Databases documentation)
##' along with a mapping.yaml file.
##'
##' @title Create a mapped databases (logical database object) 
##' @param dsn Data source name 
##' @param mapping the path to a mapping.yaml file
##' @return A new MappedDB object
##'
##' @export
MappedDB <- function(dsn, mapping = system.file("extdata", "mapping.yaml", package="icdb"))
{
    ## Connect to the server
    srv <- Databases(dsn, interactive = FALSE)

    ## Read the yaml mapping file
    m <- yaml::read_yaml(mapping)

    mdb <- new("MappedDB", mapping = m)

    ## Write the parsed config file tree to the object
    mdb@.Data <- parse_mapping(m, srv)
    
    mdb
}

setClass(
    "DocNode",
    contains = "list",
    slots = representation(
        docs = "character"
    ),
    prototype = prototype(
        docs = ""
    )
)

DocNode <- function(item_list, docs)
{
    new("DocNode", item_list, docs = docs)
}

setMethod("docs", "DocNode", function(x) {
    cat(x@docs, "\n")
})

setClass(
    "MappedTab",
    contains = "Tab",
    slots = representation(
        logical_columns = "list"
    ),
    prototype = prototype(
        logical_columns = list()
    )
)

##' Create a mapped table object. This object stores the table itself
##' (a Tab), along with metadata providing the logical column names,
##' which underlying physical columns make up these logical columns,
##' documentation for the columns, and reduce strategies for collapsing
##' each logical column into one column.
##'
##' @title Create a MappedTab object
##' @param tab The underlying Tab to use
##' @param logical_columns The columns portion of the parsed yaml config file
##' @return A new MappedTab object
MappedTab <- function(tab, logical_columns)
{
    new("MappedTab", tab, logical_columns = logical_columns)
}

##' @export
setGeneric("reduce", function(x) standardGeneric("reduce"))

##' @export
setMethod("reduce", "MappedTab", function(x)
{
    ## Loop over all the logical columns, reducing by the specified
    ## strategy
    tbl <- x()
    for (logical_column_name in names(x@logical_columns))
    {
        column <- x@logical_columns[[logical_column_name]]
        strategy <- column$strategy
        tbl <- do.call(paste0("strategy_", strategy),
                       list(tbl, logical_column_name))
    }
    tbl
})

##' Print the contents of a mapped table object
##'
##' This shows the associated dplyr::tbl and also the logical
##' column names with documentation
##'
##' @title Show a mapped table object.
##' @param object The object to show
setMethod("show","MappedTab", function(object)
{
    cat("--- Mapped table object ---\n\n")
    print(object@.Data())
    cat("\n\n--- Logical columns ---\n\n")
    for (logical_column_name in names(object@logical_columns))
    {
        column <- object@logical_columns[[logical_column_name]]
        cat(logical_column_name, ":", "[", column$strategy,"]\n")
        cat("\t", column$docs,"\n")
        cat("\t Underlying columns:\n")
        for (source_column_name in names(column$source_columns)) {
            cat("\t - ", source_column_name, "\n")
        }
    }
})

##' This function parses the tree returned by reading the yaml mapping
##' file, and returns a named list of the contents of the current level
##' passed as the argument. The function is recursive, and will descend
##' through the configuration, generating a tree of documentation,
##' database and table information that acts like a logical database.
##'
##' @title Mapping-tree parser
##' @param mapping A named list storing a level of the yaml config file
##' @param srv The Databases object with the underlying connection
##' @param source_database The name of the source database for the current
##' level
##' @param source_table The name of the source table for the current level
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
        t <- list()
        for (table in names(mapping$tables))
        {
            t[[table]] <- parse_mapping(mapping$tables[[table]], srv,
                                        source_database = source_database)
        }
        
        ## Get the documentation at this level
        docs = ""
        if ("docs" %in% names(mapping))
        {
            docs = mapping$docs
        }
        DocNode(t, docs)
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

        ## Next, create the function which will return the the tbl object
        ## corresponding to this logical table
        tab <- Tab(logical_table_getter(srv, source_database, source_table, mapping$columns),
                   make_table_docs(mapping))
        mtab <- MappedTab(tab, logical_columns = mapping$columns)
        tbl <- mtab %>% icdb::reduce()
    }
    else if ("strategy" %in% names(mapping))
    {
        stop("This function does not parse the source_columns")
        
        ## If there is a strategy field, then the current mapping element is
        ## a logical column. A logical column contains a list of source_columns,
        ## which are real columns in the database. The logical column also contains
        ## a strategy field, which informs higher levels of the program how the
        ## columns should be reduced to one column.

        ## Get the list of source columns (names are column names, values are
        ## documentation strings)
        r <- names(mapping$source_column)

        ## Do something with the strategy
        ##mapping$strategy  

        r
    }
    else
    {
        stop("Error in config file: at least one of tables, columns, or strategy must be ",
             "present at each level")
    }
}

