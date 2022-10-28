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

logical_table_getter <- function(srv, source_database, source_table, columns)
{
    force(srv)
    force(source_database)
    force(source_table)
    force(columns)
    
    function()
    {
        ## Get the name of the logical table and fetch the table
        tbl <- srv[[source_database]][[source_table]]()

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

MappedDB <- function(srv, mapping = system.file("extdata", "mapping.yaml", package="icdb"))
{
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
        docs = "list",
        prev = "ANY"
    ),
    prototype = prototype(
        docs = list(),
        prev = NULL
    )
)

setClass(
    "DocLeaf",
    contains = "Tab",
    slots = representation(
        docs = "list",
        prev = "ANY"
    ),
    prototype = prototype(
        docs = list(),
        prev = NULL
    )
)

##' Makes an item of type DocNode, suitable for storing in the logical object
##' tree. The arguments are a list of item below this level in the tree (children
##' of the current node), and the docs is a named list of documentation items
##' associated with this level
##'
##' @title Make a DocNode object
##' @param item_list List of objects below this level
##' @param docs Named list of documentation items describing this level
##' @return 
##' @author 
DocNode <- function(item_list = list(), docs = list(), prev = NULL)
{
    new("DocNode", item_list, docs = docs, prev = prev)
}

DocLeaf <- function(table, docs = list(), prev = NULL)
{
    new("DocLeaf", table, docs = docs, prev = prev)
}




setMethod("docs", "DocNode", function(x)
{
    if (!is.null(x@prev))
    {
        docs(x@prev)
    }
    cat(paste0(names(x@docs), ": ", x@docs,"\n"))
})

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
parse_mapping <- function(mapping, srv, source_database = NULL, source_table = NULL,
                          prev = NULL)
{
    if ("databases" %in% names(mapping))
    {
        ## When you get to a list of databases, parse each database is turn
        d <- list()
        for (database in names(mapping$databases))
        {
            docs <- list()
            docs[[database]] <- mapping$databases[[database]]$docs
            item_list <- parse_mapping(mapping$databases[[database]], srv)
            node <- DocNode(item_list, srv, docs, prev = NULL)
            d[[database]] <- node
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
            docs <- list()
            docs[[table]] <- mapping$databases[[table]]$docs
            node <- DocLeaf(docs = docs)
            item_list <- parse_mapping(mapping$tables[[table]], srv,
                                       source_database = source_database,
                                       prev = node)
            node@.Data <- item_list
            t[[table]] <- node            
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

        ## Next, create the function which will return the the tbl object
        ## corresponding to this logical table
        Tab(logical_table_getter(srv, source_database, source_table, mapping$columns),
            make_table_docs(mapping))
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

