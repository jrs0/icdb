##'
##'
NULL

setOldClass("node")
setClass(
    "mapped_server",
    contains = "node",
    slots = representation(
        mapping = "list"
    ),
    prototype = prototype(
        mapping = list()
    )
)

##' @title Print the mapped_server object 
##' @param object mapped_server to print
##' @export
setMethod("show", "mapped_server", function(object) {
    print(object)
})

make_mapped_table_getter <- function(srv, source, table)
{
    force(srv)
    force(source)
    columns <- table$columns

    ## Create the table documentation
    mapping <- table

    function()
    {
        
        ## Get the name of the logical table and fetch the table
        tbl <- get_tbl(srv, source)

        print("here")
        return(2)
        
        ## If the table is marked as raw, return the entire table
        ## unmodified
        if (!is.null(mapping$raw) && mapping$raw == TRUE)
        {
            return(tbl)
        }
        
        ## The first step is to select the relevant real columns
        real_columns <- list()
        for (logical_column in columns)
        {
            ## Only include the columns if the use key is set to TRUE
            ## in the logical column
            if (!is.null(logical_column$use) && logical_column$use)
            {
                real_columns <- c(real_columns, names(logical_column$source_columns))
            }
        }
        tbl <- tbl %>% dplyr::select(all_of(unlist(real_columns)))

        ## Next, rename the columns according to names derived from the logical
        ## column name
        for (logical_column_name in names(columns))
        {
            logical_column <- columns[[logical_column_name]]

            ## If the logical column is not marked with use: TRUE, then
            ## ignore this logical column
            if (!is.null(logical_column$use) && !logical_column$use)
            {
                next
            }
            
            ## Loop over the constituent columns that make up the logical column
            count <- 1
            for (old_name in names(logical_column$source_columns))
            {
                new_name <- paste0(logical_column_name,"_",count)
                tbl <- tbl %>% dplyr::rename_with(~ new_name, old_name)
                count <- count + 1
            }
        }

        ## Loop over all the logical columns, reducing by the specified
        ## strategies. The strategies are presented as a list -- the order
        ## in the yaml file specifies the order in which they are executed.
        for (logical_column_name in names(columns))
        {
            logical_column <- columns[[logical_column_name]]
            strategies <- logical_column$strategy

            ## If the logical column is not marked with use: TRUE, then
            ## ignore this logical column
            if (!is.null(logical_column$use) && !logical_column$use)
            {
                next
            }

            ## Loop over the strategies, applying one-by-one
            for (strategy in strategies)
            {
                
                ## If the item is not a list, then it is a simple function
                ## which can be called to process the item
                if (is.null(names(strategy)))
                {
                    tbl <- do.call(strategy, list(tbl=tbl,
                                                  name=logical_column_name))
                }
                else
                {
                    ## If the strategy is a list, then the first element
                    ## is the function name and the subsequent elements are
                    ## the arguments
                    tbl <- do.call(names(strategy),
                                   list(tbl=tbl,
                                        name=logical_column_name,
                                        strategy[[1]]))
                }
            }
        }
            
        mapped_table(tbl, mapping)
    }
}

##' Create a new mapped database object. A mapped database is an object that contains
##' mapped databases, mapped tables and mapped column names. The mapping operation is a
##' simple preprocessing operation, that performs tasks such as renaming and coalescing
##' columns, and certain basic filtering and mutate operations. In addition, the mapped
##' objects can store documentation about themselves, which provides an easy way to look
##' up what a column is or where it came from. The mapped_server uses an underlying server
##' object for the database connection.
##' 
##' The structure of the database is defined by a .yaml file, which describes
##' how to obtain the fields from an underlying data source. See the package documentation
##' for how to structure this file.
##'
##' To make a mapped database, pass any arguments that are required for the underlying
##' server object (e.g. a data source name or a config file path), along with a
##' mapping.yaml file.
##'
##' @title Create a mapped databases (logical database object)
##' @param ... Any arguments that should be passed to the underlying server function
##' @param mapping the path to a mapping.yaml file
##' @return A new MappedDB object
##'
##' @export
##' 
mapped_server <- function(..., mapping = system.file("extdata", "mapping.yaml", package="icdb"))
{
    ## Connect to the server
    srv <- server(..., interactive = FALSE)

    ## Read the yaml mapping file
    m <- yaml::read_yaml(mapping)

    ## Write the parsed config file tree to the object
    node <- parse_mapping(m, srv)
    mdb <- new("mapped_server", node, mapping = m)
}

new_mapped_table <- function(tbl, mapping)
{
    mtab <- table_node(tbl, class="mapped_table")
    attr(mtab, "mapping") <- mapping
    mtab
}

mapped_table <- function(tbl, mapping)
{
    new_mapped_table(tbl, mapping)
}

print_mapping <- function(mapping, level = 0)
{
    ## First print the top level summary information
    cat(crayon::bold("\nSUMMARY\n"))
    cat(stringr::str_wrap(mapping$docs),"\n")

    ## Next, print information about the logical columns
    cat(crayon::bold("\nMAPPED COLUMNS\n"))
    for (logical_column_name in names(mapping$columns))
    {
        logical_column <- mapping$columns[[logical_column_name]]

        ## This is a quick hack just to restrict to only the
        ## used columns. Really, the unused columns should not
        ## even be here, but that can be part of the proper
        ## documentation parsing step (which does not exist yet)
        if (!is.null(logical_column$use) && !logical_column$use)
        {
            next
        }

        cat(crayon::blue(logical_column_name), "\n")
        cat(stringr::str_wrap(crayon::bold(logical_column$docs)), "\n")
        cat("Generated from:\n")
        for (real_column_name in names(logical_column$source_columns))
        {
            cat(paste0(" - ", real_column_name, "\n"))
        }

        cat("Reduce strategy: ")
        for (strategy in logical_column$strategy)
        {
            cat(paste0(strategy, ", "))
        }
        cat("\n\n")
    }
        
}


##' @export
print.mapped_table <- function(x,...)
{
    print_mapping(attr(x,"mapping"))

    cat(crayon::bold("MAPPED TABLE\n"))
    NextMethod()
}

##' Read an included configuration file. The search path is the
##' current working directory first, followed by the extdata
##' folder.
##'
##' This is used for the include key in the config files. This function
##' represents a generic approach to opening files, which will
##' search the working directory first and then extdata. All files
##' will be found this way. May want to consider modifying this
##' mechanism later, in which case it will be good to isolate it in
##' a few functions like this (the other one is currently get_codes,
##' which doesn't quite work like this and needs changing).
##' 
##' @title Read an included config file.
##' @param include_file The path to the config file
##' @return The nested list for the data stored in the included file
##' 
read_include <- function(include_file)
{
    if (fs::is_file(include_file))
    {
        ## Return the contents of the file in working directory
        yaml::read_yaml(include_file)
    }
    else if (fs::is_file(system.file("extdata", include_file, package="icdb")))
    {
        ## Return the contents of the file in extdata
        yaml::read_yaml(system.file("extdata", include_file, package="icdb"))
    }
    else
    {
        stop("Could not find include config file '", include_file, "'")
    }
}

##' This function parses the tree returned by reading the yaml mapping
##' file, and returns a named list of the contents of the current level
##' passed as the argument. The function is recursive, and will descend
##' through the configuration, generating a tree of documentation,
##' database and table information that acts like a logical database.
##'
##' @title Mapping-tree parser
##' @param mapping A list storing a level of the yaml config file
##' @param srv The Databases object with the underlying connection
##' @return A node object containing the results of parsing the file
##'
parse_mapping <- function(mapping, srv)
{
    ## For now, process all the includes first, so as to form the
    ## full list of objects at this level. This does require two
    ## passes over mapping, but there's likely no efficiency concern
    ## with this.
    copy <- mapping
    for (object in copy)
    {
        if ("include" %in% names(object))
        {
            print("Parsing include")
            
            ## If the current object is an include, then read the
            ## contents of the included file and put them in the current
            ## list level 
            mapping <- c(mapping, read_include(object$include))
        }     
    }

    ## Drop the include objects
    mapping <- mapping[
        !sapply(mapping, function(x) {"include" %in% names(x)})
    ]
    
    ## The mapping argument is a list of object -- loop over them
    ## recursively processing the contents. The results will be
    ## put in this named list, which will be returned as a node
    ## type at the end
    result <- list()
    
    for (object in mapping)
    {
        ## Check which of the three valid objects is being processed
        if ("database" %in% names(object))
        {
            print("Parsing database")

            ## Check validity
            if("tables" %in% names(object))
            {
                result[[object$database]] <-
                    parse_mapping(object$tables, srv)
            }
            else
            {
                stop("Expected 'tables' key in database object ",
                     object$database)
            }  
        }
        else if ("table" %in% names(object))
        {
            message("Parsing table ", object$table)
            
            ## Check validity
            if(!("columns" %in% names(object)) &&
               !("raw" %in% names(object)))
            {
                stop("Expected 'columns' or 'raw' key in table object ",
                     object$table)
            }

            ## If there is a columns field, then the current mapping is a table.
            ## Record the source table name for the next execution environment
            source <- mapping$source
            
            ## corresponding to this logical table
            result[[object$table]] <-
                table_wrapper(make_mapped_table_getter(srv,
                                                       source,
                                                       mapping))
        }
        else
        {
            stop("Error in config file: expected a database, table, column, or include key.")
        }
    }

    ## Return the results as a node
    node(result)
}



## parse_mapping <- function(mapping, srv, source_database = NULL, source_table = NULL)
## {
##     ## Need to add something here to parse include files. Might want to refactor
##     ## this function completely and/or the file format.
##     ##
##     ## If an include key is found at this level of the mapping, then all other keys
##     ## are ignored, because the include is treated first. This currently happens
##     ## with no warning -- consider adding a function to check for other keys
##     if ("include" %in% names(mapping))
##     {
##         parse_mapping(read_include(mapping$include),
##                       srv, source_database, source_table)
##     }    
##     else if ("databases" %in% names(mapping))
##     {
##         ## When you get to a list of databases, parse each database in turn
##         d <- list()
##         for (database in names(mapping$databases))
##         {
##             d[[database]] <- parse_mapping(mapping$databases[[database]], srv)
##         }
##         node(d)
##     }
##     else if ("tables" %in% names(mapping))
##     {
##         ## If there is a tables field, then the current mapping is a logical
##         ## database. Record the database name for the next function execution
##         ## environment.
##         source_database <- mapping$source_database
##         t <- list()
##         for (table in names(mapping$tables))
##         {
##             t[[table]] <- parse_mapping(mapping$tables[[table]], srv,
##                                         source_database = source_database)
##         }
##         ## Return the list of tables
##         node(t)
##     }
##     else if ("columns" %in% names(mapping))
##     {
##         ## This is never used? This function needs an overhaul.
        
##         ## If there is a columns field, then the current mapping is a table.
##         ## Record the source table name for the next execution environment
##         source_table <- mapping$source_table

##         ## Check if there is a source_database key -- if there is, it must
##         ## overwrite the value inherited from the calling environment, to
##         ## support the possibility that tables in the same logical database
##         ## originate from separate source databases.
##         if (!is.null(mapping$source_database))
##         {
##             source_database <- mapping$source_database
##         }

##         ## Next, create the function which will return the the Mapped object
##         ## corresponding to this logical table
##         tab <- table_wrapper(make_mapped_table_getter(srv, source_database, source_table, mapping))
##     }
##     else if ("strategy" %in% names(mapping))
##     {
##         stop("This function does not parse the source_columns")

##         ## If there is a strategy field, then the current mapping element is
##         ## a logical column. A logical column contains a list of source_columns,
##         ## which are real columns in the database. The logical column also contains
##         ## a strategy field, which informs higher levels of the program how the
##         ## columns should be reduced to one column.

##         ## Get the list of source columns (names are column names, values are
##         ## documentation strings)
##         r <- names(mapping$source_column)

##         ## Do something with the strategy
##         ##mapping$strategy

##         r
##     }
##     else
##     {
##         stop("Error in config file: at least one of tables, columns, or strategy must be ",
##              "present at each level")
##     }
## }
