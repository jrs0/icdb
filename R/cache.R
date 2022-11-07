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
    level1 = list(meta = dplyr::tibble(hash=character(), # The key
                                       data = character(), # Used to generate the key
                                       hits = numeric(), # Number of times the cache entry was read
                                       write_time = as.Date(character()), # When the entry was written
                                       last_access = as.Date(character()), # When the entry was last accessed
                                       time = as.difftime(1, units="hours") # How long did the original computation take
                                       ),
                  max_size = 2,
                  objects = list()),
    path = "cache/",
    use_cache = FALSE,
    lifetime = lubridate::dhours(24)
)

##' Use this function to turn the cache on or off. If the cache is
##' on, then data will be written to and read from the cache. If
##' the cache is off, then the read_cache function will always
##' return NULL and the write cache function will return without
##' doing anything. Turning the cache off does not clear the
##' cache, and turning the cache back on later will make available
##' the same cache contents as before.
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
##' The cache is disabled by default. This is to ensure that, by
##' default, queries always return up-to-date results.
##'
##' When the cache is enabled, cached results will automatically
##' expire after a certain amount of time, which can be specified
##' using the lifetime parameter. The default cache lifetime is
##' 24 hours. Cached values that have expired are automatically
##' deleted from the cache when an attempt is made to fetch them.
##' The expiry time is based on the time when the cached results
##' were stored, not when the cached result was last accessed.
##' The purpose of the expiry is to ensure that data in the
##' cache is refreshed at least as often as the lifetime.
##' 
##' @title Turn the cache on or off
##' @param state TRUE to turn the cache on, FALSE to turn it off
##' @param lifetime The default amount of time that cache results
##' will remain valid. Specified as a lubridate duration (e.g.
##' lubridate::dhours(24)), with default value 24 hours. 
##' 
##' @export
##' 
use_cache <- function(state, lifetime = lubridate::dhours(24))
{
    pkg_env$cache$use_cache <- state
    pkg_env$cache$lifetime <- lifetime
}

record_hit <- function(metadata)
{
    metadata$hits <-  metadata$hits + 1
    metadata$last_access <- lubridate::now()
    metadata
}

##' Write en entry from the leve1 cache to the level2 cache
##'
##' @title Flush entry to the level2 cache
##' @param metadata The metadata as a named list
write_level2 <- function(metadata)
{
    ## Get the hash key of the cache entry
    hash <- metadata$hash
    
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
    saveRDS(pkg_env$cache$level1$objects[[hash]], file = obj_file)
}

write_level1 <- function(metadata, object)
{
    ## The metadata is not saved directly. Instead, it is encapsulated inside a
    ## structure that also holds information generic to all pkg_env$cache entries: the
    ## number of hits, last access, etc. This information is used to track how
    ## the entry is used, and provide summary information.
    now <- lubridate::now()

    ## Write the object to the level 1 cache first here
    pkg_env$cache$level1$meta <- pkg_env$cache$level1$meta %>%
        dplyr::add_row(hash = metadata$hash,
                       data = metadata$data,
                       hits = metadata$hits,
                       time = metadata$time,
                       write_time = metadata$write_time,
                       last_access = metadata$last_access)

    ## Write the new object to the level 1 cache
    pkg_env$cache$level1$objects[[metadata$hash]] <- object

    ## Prune the level 1 cache
    prune_level1()
}

##' Get the metadata associated with a hash as a list
##'
##' @title Get metadata list
##' @param hash The hash of the cache entry
get_metadata <- function(hash)
{
    tbl <- pkg_env$cache$level1$meta %>%
        dplyr::filter(hash == !!hash)
    stopifnot(nrow(tbl) == 1)
    as.list(tbl[1,])   
}

##' Write an element into the cache
##'
##' This function creates an entry in the cache. It should not be called by the
##' user of the package. The data comprises two parts: an object, which is
##' typically large, which is used to store "results" in the cache. The second
##' part is data about the results, which is used to generate a hash to identify
##' the large object in the cache. This data must currently be a character.
##'
##' Data is stored in two places: the level 1 cache, which is stored in memory;
##' and the level 2 cache, which is written to disk. Identical data is stored in
##' both locations, in terms of access time, etc. This function initialises the
##' hits to 1, meaning that this is the first access. All subsequent accesses of
##' the data should use the readCache function, which modifies the metadata in the
##' cache.
##'
##' The level 2 cache is not written directly -- instead, data is only written to
##' the disk when the object must be removed from the level 1 (memory) cache.
##' 
##' This function will not check the cache before writing to it, so any duplicate
##' data will be overwritten. Only call this function after establish via
##' readCache that the data is not already present in the cache.
##'
##' @param data Data about the object, used to generate the hash
##' @param object The main (typically large) object to store in the cache
##' @param time The duration of the operation that requires caching
##'
write_cache <- function(data, object, time)
{
    ## If the cache is disabled, return without doing anything
    if (pkg_env$cache$use_cache == FALSE)
    {
        return(NULL)
    }
    
    ## Make the hash out of the metadata
    hash <- rlang::hash(data)

    ## Initialise the metadata
    now = lubridate::now()
    metadata <- list(
        hash = hash,
        data = data,
        hits = 1,
        write_time = now,
        last_access = now,
        time = time
    )

    ## Write new entry to the level1 cache
    write_level1(metadata, object)
}

##' Remove entries from the level 1 cache, flushing them to level 2
##'
##' @title Flush and prune the level 1 cache
prune_level1 <- function()
{
    ## After writing to the level 1 cache, check whether anything needs
    ## to be deleted (currently, if it has too many elements)
    if (nrow(pkg_env$cache$level1$meta) > pkg_env$cache$level1$max_size)
    {
        ## Find the oldest element in the cache, and write it to
        ## the level 2 cache. This assumes that it is dirty -- could
        ## add a flag to indicate whether the entry needs to be flushed
        metadata <- pkg_env$cache$level1$meta %>%
            dplyr::filter(last_access == min(last_access)) %>%
            as.list()
        
        ## Write the row to the level2 cache
        write_level2(metadata)

        ## Now delete the entry from the level1 cache
        pkg_env$cache$level1$meta <- pkg_env$cache$level1$meta %>%
            dplyr::filter(hash != metadata$hash)
    }    
}

##' Read an object from the cache
##'
##' Use this function to get an object from the cache. If the object
##' is found, then it will be returned. If the object is not found,
##' then NULL is returned. If the object is present in the cache, but
##' it has expired (see the use_cache function documentation), then
##' the object will be deleted from the cache and NULL will be returned.
##'
##' If the cache is disabled, then NULL is always returned.
##' 
##' @param data The same data object passed to the writeCache function
##' @return Returns the object associated with the data. NULL if not found.
##' 
read_cache <- function(data)
{
    ## If the cache is disabled, return NULL
    if (pkg_env$cache$use_cache == FALSE)
    {
        return(NULL)
    }
    
    ## Make the has out of the metadata
    hash <- rlang::hash(data)
    now <- lubridate::now()
    
    ## First, attempt to read the data from the level 1 cache here
    res <- pkg_env$cache$level1$meta %>%
        dplyr::filter(hash == !!hash)
    
    if (nrow(res) == 1)
    {
        message("Found data in level 1 cache")

        ## Get the record as a list
        metadata <- as.list(res)        

        ## Update the metadata
        metadata <- record_hit(metadata)
        
        ## Delete the entry from the level 1 cache
        pkg_env$cache$level1$meta <- pkg_env$cache$level1$meta %>%
            dplyr::filter(hash != !!hash)

        ## Now re-add the entry from the list
        pkg_env$cache$level1$meta <-
            dplyr::bind_rows(pkg_env$cache$level1$meta, metadata)
    
        ## Data present in level 1 cache
        return(pkg_env$cache$level1$objects[[hash]])
    }
    
    
    ## If data is not in the L1 pkg_env$cache, check if L2 directory exists
    if (!dir.exists(pkg_env$cache$path))
    {
        return(NULL)
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
            object <- readRDS(obj_file)
            
            ## Promote the entry to the level 1 cache
            write_level1(metadata, object)

            ## Return the object
            object
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
##' Show the contents of the cache as a dataframe
##'
##' @title Summarise the cache
##' @return A tibble containing cache entries
##'
##' @export
show_cache <-function()
{
    message("The cache is stored in the folder: ", pkg_env$cache$path)

    ## Print the level 1 cache metadata
    message("Level 1 cache metadata:")
    tbl <- pkg_env$cache$level1$meta

    tbl <- tbl %>% tibble::add_column(in_memory = TRUE)
    
    ## Next, get the level 2 files and remove those that are in level 1
    file_list <- list.files(pkg_env$cache$path, pattern = "meta\\.rds") %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(value = stringr::str_replace(value, ".meta.rds", "")) %>%
        dplyr::filter(!(value %in% tbl$hash)) %>%
        dplyr::mutate(value = paste0(value, ".meta.rds"))

    ## Make a function to get the data
    fn <- function(file) {
        meta_file <- paste0(pkg_env$cache$path, "/", file)
        readRDS(meta_file)
    }
    
    res <- file_list[["value"]] %>%  purrr::map_dfr(fn)
    tbl <- dplyr::bind_rows(tbl, res)

    ## Replace NA in mem column by FALSE
    tbl[is.na(tbl)] <- FALSE

    tbl %>% dplyr::arrange(desc(last_access))
}


##' Clear the cache
##'
##' Delete all the cached results in the cache folder.
##'
##' @export
clear_cache <- function()
{
    ## Clear the level 1 cache
    pkg_env$cache$level1$meta = dplyr::tibble(hash=character(),
                                              data = character(),
                                              hits = numeric(),
                                              write_time = as.Date(character()),
                                              last_access = as.Date(character()),
                                              time = as.difftime(1, units="hours"))
    pkg_env$cache$level1$objects = list()
    
    ## Check if directory exists
    if (dir.exists(pkg_env$cache$path) && length(list.files(pkg_env$cache$path)) > 0)
    {
        list.files(pkg_env$cache$path) %>%
            stringr::str_c(pkg_env$cache$path,.) %>%
            purrr::map(file.remove)
    }
    message("Cleared cache.")
    invisible(pkg_env$cache)
}


