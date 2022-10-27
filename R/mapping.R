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

logical_table_getter <- function(srv, database, source_table, logical_table)
{
    force(srv)
    force(database)
    force(source_table)
    force(logical_table)
    function()
    {
        tbl <- srv[[database]][[source_table]]()

        ## TODO There is an important potential bug in this code, where a column name in
        ## the database might conflict with a logical column name. The chance is quite slim,
        ## but if it does, the routine below may rename the wrong columns. This needs
        ## testing and possibly modifying to cover this case.
        
        ## Make the set of columns to select (old names,
        ## union of columns in mapping file)
        flat_column_names <- names(unlist(logical_table))
        old_cols <- flat_column_names[grepl("columns",flat_column_names)] %>%
            strsplit("\\.") %>% purrr::map(~ tail(.x,n=1)) %>%
            unlist()
        
        tbl <- tbl %>% dplyr::select(old_cols)
        
        ## Loop over column names
        for (logical_column in names(logical_table))
        {
            ## Loop over the constituent columns that make up the logical column
            count <- 1
            for (old_name in names(logical_table[[logical_column]]$columns))
            {
                new_name <- paste0(logical_column,"_",count)
                tbl <- tbl %>% dplyr::rename_with(~ new_name, old_name)
                count <- count + 1 
            }
        }
        tbl
    }
}

MappedDB <- function(srv, mapping = system.file("extdata", "mapping.yaml", package="icdb"))
{
    m <- yaml::read_yaml(mapping)

    mdb <- new("MappedDB", mapping = m)

    # ldb stands for logical database
    for (ldb_name in names(m))
    {
        ## Get the names of the source database and source table
        database <- m[[ldb_name]]$database
        source_table <- m[[ldb_name]]$source_table
        
        mdb[[ldb_name]] <- Tables()
        mdb[[ldb_name]]@.Data <- m[[ldb_name]]$logical_tables %>%
            purrr::map(~ logical_table_getter(srv, database, source_table, .x))
        names(mdb[[ldb_name]]@.Data) <- names(m[[ldb_name]]$logical_tables)
    }

    mdb
}