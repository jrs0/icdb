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
                real_columns <- c(real_columns, logical_column$source)
            }
        }
        tbl <- tbl %>% dplyr::select(dplyr::all_of(unlist(real_columns)))

        ## Next, rename the columns according to names derived from the logical
        ## column name
        for (logical_column in columns)
        {
            logical_column_name <- logical_column$column

            ## If the logical column is not marked with use: TRUE, then
            ## ignore this logical column
            if (!is.null(logical_column$use) && !logical_column$use)
            {
                next
            }

            old_name <- logical_column$source
            new_name <- logical_column_name
            tbl <- tbl %>% dplyr::rename_with(~ new_name, old_name)
            
        }

        ## Loop over all the logical columns, reducing by the specified
        ## strategies. The strategies are presented as a list -- the order
        ## in the yaml file specifies the order in which they are executed.
        for (logical_column in columns)
        {
            logical_column_name <- logical_column$column
            strategies <- logical_column$strategy

            ## If the logical column is not marked with use: TRUE, then
            ## ignore this logical column
            if (!is.null(logical_column$use) && !logical_column$use)
            {
                next
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
mapped_server <- function(..., mapping = system.file("extdata", "bnssg/mapping.yaml", package="icdb"))
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
            message("Reading included file '", object$include, "'")

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
            message("Adding database '", object$database, "'")

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
            message("Adding table '", object$table, "'")

            ## Check validity
            if (!("columns" %in% names(object)) &&
               !("raw" %in% names(object)))
            {
                stop("Expected 'columns' or 'raw' key in table object ",
                     object$table)
            }

            ## Check validity
            if (!("source" %in% names(object)))
            {
                stop("Expected required or 'source' key in table object ",
                     object$table)
            }

            ## Record the source table name for the next execution environment
            source <- object$source

            ## corresponding to this logical table
            result[[object$table]] <-
                table_wrapper(make_mapped_table_getter(srv, source, object))
        }
        else
        {
            stop("Error in config file: expected a database, table, column, or include key.")
        }
    }

    ## Return the results as a node
    node(result)
}
