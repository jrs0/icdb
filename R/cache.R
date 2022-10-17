##' @importFrom methods new show
##' @importFrom magrittr %>%
##' @importClassesFrom DBI DBIConnection
##' @export
NULL


pkg_env <- new.env(parent = emptyenv())
pkg_env$cache_object <- list(path = "cache/", level1 = list())

##' Create a new cache object
##'
##' @param path The directory in which to store the cache, defaults to "cache/" relative
##' to the current working directory.
##' 
##' @return A new Cache object
Cache <- function(path = "cache/")
{
    ## Make the object
    c <- list(
        path = path,
        level1 = list()
    )
    
    ## Create the directory if it does not exist
    if (!dir.exists(path))
    {
        dir.create(path)
    }

    ## Return the new object
    c
}

##' Global cache object, ideally not accessible to users of the package
##' (not achieved that yet). This should probably be an R6 class. An R4
##' class will not work because it is not mutable. To fix later.
## pkg_env$cache_object <- Cache("cache/")

record_hit <- function(metadata)
{
    metadata$hits <-  metadata$hits + 1
    metadata$last_access <- Sys.time()
    metadata
}

##' Write an element into the cache
##'
##' This function creates an entry in the cache. It should not be called by the
##' user of the package. The data comprises two parts: an object, which is
##' typically large, which is used to store "results" in the cache. The second
##' part is data about the results, which is used to generate a key. The
##' data can be anything, but a named list is a simple way to organise the
##' necessary data.
##'
##' Data is stored in two places: the level 1 cache, which is stored in memory;
##' and the level 2 cache, which is written to disk. Identical data is stored in
##' both locations, in terms of access time, etc. This function initialises the
##' hits to 1, meaning that this is the first access. All subsequent accesses of
##' the data should use the readCache function, which modifies the metadata in the
##' cache.
##' 
##' This function will not check the cache before writing to it, so any duplicate
##' data will be overwritten. Only call this function after establish via
##' readCache that the data is not already present in the cache.
##'
##' @param data Data about the object, used to generate the hash
##' @param object The main (typically large) object to store in the cache
##'
write_cache <- function(data, object)
{
    ## Make the hash out of the metadata
    hash <- rlang::hash(data)
    
    ## The metadata is not saved directly. Instead, it is encapsulated inside a
    ## structure that also holds information generic to all pkg_env$cache_object entries: the
    ## number of hits, last access, etc. This information is used to track how
    ## the entry is used, and provide summary information.
    now <- Sys.time()
    metadata <- list(
        hits = 1,
        level1 = TRUE,
        level2 = TRUE,
        write_time = now,
        last_access = now,

        ## Finally, store the user provided data
        data = data
    )

    ## Write the object to the level 1 pkg_env$cache_object first here
    
    ## After writing to the level 1 pkg_env$cache_object, check whether anything needs
    ## to be deleted (currently, if it has too many elements)
    
    ## Create the level 2 directory if it does not exist
    if (!dir.exists(pkg_env$cache_object$path))
    {
        dir.create(pkg_env$cache_object$path)
    }
    
    ## Create the object filename and the metadata filename
    obj_file <- paste0(pkg_env$cache_object$path, "/", hash, ".obj.rds")
    meta_file <- paste0(pkg_env$cache_object$path, "/", hash, ".meta.rds")

    ## Store the metadata and the object to the level 2 pkg_env$cache_object directory
    saveRDS(metadata, file = meta_file)
    saveRDS(object, file = obj_file)
}

##' Read an object from the cache
##'
##' Use this function to get an object from
##'
##' @param data The same data object passed to the writeCache function
##'
##' @return Returns the object associated with the data. Null if not found.
read_cache <- function(data)
{
    ## Make the has out of the metadata
    hash <- rlang::hash(data)

    ## First, attempt to read the data from the level 1 pkg_env$cache_object here

    ## If data is not in the L1 pkg_env$cache_object, check if L2 directory exists
    if (!dir.exists(pkg_env$cache_object$path))
    {
        NULL
    }

    ## Create the object filename and the metadata filename
    obj_file <- paste0(pkg_env$cache_object$path, "/", hash, ".obj.rds")
    meta_file <- paste0(pkg_env$cache_object$path, "/", hash, ".meta.rds")

    ## Check to see if the files exist
    if (file.exists(meta_file))
    {
        if (file.exists(obj_file))
        {
            ## Open the meta file and increment the update values
            metadata <- readRDS(meta_file)
            metadata <- record_hit(metadata)
            saveRDS(metadata, file = meta_file)

            ## Now open and return the object
            readRDS(obj_file)
        }
        else
        {
            stop("The pkg_env$cache_object is corrupt: missing .obj.rds file for .meta.rds file.")
        }
    }
    else
    {
        NULL
    }
}

show_cache <-function()
{
    message("The cache is stored in the folder: ", pkg_env$cache_object$path)

    ## Get the list of files
    file_list <- list.files(pkg_env$cache_object$path, pattern = "meta\\.rds")

    ## Make a function to get the data
    get_metadata <- function(file) {
        meta_file <- paste0(pkg_env$cache_object$path, "/", file)
        metadata <- readRDS(meta_file)
        print(metadata$last_access)

        ## Replace this
        list(data = metadata$data,
             hits = metadata$hits,
             write_time = metadata$write_time,
             last_access = metadata$last_access)
    }

    res <- file_list %>%
        purrr::map(get_metadata) %>%
        purrr::transpose() %>%
        purrr::map(unlist)

    t <- do.call(dplyr::tibble, res)

    ## Print the summary
    t
}


##' Clear the cache
##'
##' Delete all the cached results in the cache folder.
##'
clear_cache <- function()
{
    ## Check if directory exists
    if (dir.exists(pkg_env$cache_object$path) && length(list.files(pkg_env$cache_object$path)) > 0)
    {
        list.files(pkg_env$cache_object$path) %>%
            stringr::str_c(pkg_env$cache_object$path,.) %>%
            purrr::map(file.remove)
        message("Cleared pkg_env$cache_object.")
    }
    else
    {
        message("Pkg_Env$Cache_Object already empty.")
    }
    invisible(pkg_env$cache_object)
}


