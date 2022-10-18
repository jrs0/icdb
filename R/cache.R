##' @importFrom methods new show
##' @importFrom magrittr %>%
##' @importClassesFrom DBI DBIConnection
##' @export
NULL


## Currently using an environment to prototype, can change to an R6 class to
## make it slightly better. What would be even better is if a global variable
## (local to the package) just worked, but that might not be possible.
pkg_env <- new.env(parent = emptyenv())
pkg_env$cache <- list(
    level1 = list(meta = dplyr::tibble(hash=character(),
                                       hits = numeric(),
                                       level1 = logical(),
                                       level2 = logical(),
                                       write_time = as.Date(character()),
                                       last_access = as.Date(character())),
                  objects = list()),
    path = "cache/"
)

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
    ## structure that also holds information generic to all pkg_env$cache entries: the
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

    ## Write the object to the level 1 cache first here
    pkg_env$cache$level1$meta <- pkg_env$cache$level1$meta %>%
        dplyr::add_row(hash = hash,
                       hits = 1,
                       level1 = TRUE,
                       level2 = TRUE,
                       write_time = now,
                       last_access = now)
    
    ## After writing to the level 1 cache, check whether anything needs
    ## to be deleted (currently, if it has too many elements)
    if (nrow(pkg_env$cache$level1$meta) > 1)
    {
        ## Find the oldest element in the cache, and write it to
        ## the level 2 cache. This assumes that it is dirty -- could
        ## add a flag to indicate whether the entry needs to be flushed
        row_to_delete <- pkg_env$cache$level1$meta %>% dplyr::filter(last_access == min(last_access))
        print(row_to_delete)
    }
    
    ## Create the level 2 directory if it does not exist
    if (!dir.exists(pkg_env$cache$path))
    {
        dir.create(pkg_env$cache$path)
    }
    
    ## Create the object filename and the metadata filename
    obj_file <- paste0(pkg_env$cache$path, "/", hash, ".obj.rds")
    meta_file <- paste0(pkg_env$cache$path, "/", hash, ".meta.rds")

    ## Store the metadata and the object to the level 2 pkg_env$cache directory
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

    ## First, attempt to read the data from the level 1 pkg_env$cache here

    ## If data is not in the L1 pkg_env$cache, check if L2 directory exists
    if (!dir.exists(pkg_env$cache$path))
    {
        NULL
    }

    ## Create the object filename and the metadata filename
    obj_file <- paste0(pkg_env$cache$path, "/", hash, ".obj.rds")
    meta_file <- paste0(pkg_env$cache$path, "/", hash, ".meta.rds")

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
            stop("The pkg_env$cache is corrupt: missing .obj.rds file for .meta.rds file.")
        }
    }
    else
    {
        NULL
    }
}

show_cache <-function()
{
    message("The cache is stored in the folder: ", pkg_env$cache$path)

    ## Get the list of files
    file_list <- list.files(pkg_env$cache$path, pattern = "meta\\.rds")

    ## Make a function to get the data
    get_metadata <- function(file) {
        meta_file <- paste0(pkg_env$cache$path, "/", file)
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
    if (dir.exists(pkg_env$cache$path) && length(list.files(pkg_env$cache$path)) > 0)
    {
        list.files(pkg_env$cache$path) %>%
            stringr::str_c(pkg_env$cache$path,.) %>%
            purrr::map(file.remove)
        message("Cleared pkg_env$cache.")
    }
    else
    {
        message("Pkg_Env$Cache already empty.")
    }
    invisible(pkg_env$cache)
}


