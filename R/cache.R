##' @importFrom methods new show
##' @importFrom magrittr %>%
##' @importClassesFrom DBI DBIConnection
##' @export
NULL

##' This function creates an empty level 1 cache tibble, containing
##' the the columns: hash (for the unique key for the item);
##' data (the thing that is hashed, e.g. an sql query string);
##' hits (the number of times the entry was used); write_time;
##' last_access; and time (how long the original computation of
##' the cached data took).
##'
##' @title Make an empty level 1 metadata tibble
##' @return The empty cache tibble
make_empty_metadata <- function()
{
    tibble::tibble(
                hash = character(),
                data = character(),
                hits = numeric(),
                write_time = as.Date(character()),
                last_access = as.Date(character()),
                time = as.difftime(1, units="hours")
            )
}

Cache <- R6::R6Class(
                 "Cache",
                 public = list(
                     level1 = list(
                         meta = make_empty_metadata(),
                         max_size = 5,
                         objects = list()
                     ),
                     path = "cache/",
                     use_cache = FALSE,
                     lifetime = lubridate::dhours(24),
                     finalize = function() {
                         flush_level1()
                     }
                 )
             )

cache <- Cache$new()

##' Use this function to turn the cache on or off. If the cache is
##' on, then data will be written to and read from the cache. If
##' the cache is off, then the read_cache function will always
##' return NULL and the write_cache function will do anything.
##' Turning the cache off does not clear the
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
##' @param size Use this parameter to set the size of the level 1
##' (memory) cache, which is the maximum number of cached queries
##' that can be stored before older queries get flushed to the disk.
##' The default size is 5.
##'
##' @export
##'
use_cache <- function(state, lifetime = lubridate::dhours(24), size = 5)
{
    cache$use_cache <- state
    cache$lifetime <- lifetime
    cache$level1$max_size <- size
}

record_hit <- function(metadata)
{
    metadata$hits <-  metadata$hits + 1
    metadata$last_access <- lubridate::now()
    metadata
}

##' Write entries from the level 1 cache to the level 2 cache.
##' The entries are supplied a rows of a tibble.
##'
##' @title Write entries to the level2 cache
##' @param metadata The metadata as a tibble
write_level2 <- function(metadata)
{
    if (!dir.exists(cache$path))
    {
        dir.create(cache$path)
    }
    metadata %>%
        dplyr::pull(hash) %>%
        purrr::map(function(this_hash)
        {
            obj_file_name <- paste0(cache$path, "/", this_hash, ".obj.rds")
            meta_file_name <- paste0(cache$path, "/", this_hash, ".meta.rds")
            metadata_row <- metadata %>%
                dplyr::filter(hash == this_hash)
            saveRDS(metadata_row, file = meta_file_name)
            saveRDS(cache$level1$objects[[this_hash]], file = obj_file_name)
        })
}

write_level1 <- function(metadata, object)
{
    ## The metadata is not saved directly. Instead, it is encapsulated inside a
    ## structure that also holds information generic to all cache entries: the
    ## number of hits, last access, etc. This information is used to track how
    ## the entry is used, and provide summary information.
    now <- lubridate::now()

    ## Write the object to the level 1 cache first here
    cache$level1$meta <- cache$level1$meta %>%
        dplyr::add_row(hash = metadata$hash,
                       data = metadata$data,
                       hits = metadata$hits,
                       time = metadata$time,
                       write_time = metadata$write_time,
                       last_access = metadata$last_access)

    ## Write the new object to the level 1 cache
    cache$level1$objects[[metadata$hash]] <- object

    ## Prune the level 1 cache
    prune_level1()
}

##' Get the metadata associated with a hash as a list
##'
##' @title Get metadata list
##' @param hash The hash of the cache entry
get_metadata <- function(hash)
{
    tbl <- cache$level1$meta %>%
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
    if (cache$use_cache == FALSE)

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

##' Move all items in the level 1 cache to level 2
##' 
##' @title Flush the level 1 cache
flush_level1 <- function()
{
    message("Flushing all cached items to disk")
    cache$level1$meta %>%
        write_level2()
    ## And also delete the level1 here too...
}

##' If the level 1 cache is full, move older entries to the level 2 cache.
##' The element that is moved is the oldest element.
##' 
##' @title Prune the level 1 cache
##' 
prune_level1 <- function()
{
    if (nrow(cache$level1$meta) > cache$level1$max_size)
    {
        metadata <- cache$level1$meta %>%
            dplyr::filter(last_access == min(last_access)) %>%
            write_level2()
        cache$level1$meta <- cache$level1$meta %>%
            dplyr::filter(last_access != min(last_access))
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
##' @param lifetime You can specify a custom lifetime here which will
##' override the default lifetime specified in use_cache.
##' @return Returns the object associated with the data. NULL if not found.
##'
read_cache <- function(data, lifetime = NULL)
{
    ## If the cache is disabled, return NULL
    if (cache$use_cache == FALSE)
    {
        return(NULL)
    }

    ## Make the has out of the metadata
    hash <- rlang::hash(data)
    now <- lubridate::now()

    ## First, attempt to read the data from the level 1 cache here
    res <- cache$level1$meta %>%
        dplyr::filter(hash == !!hash)

    if (nrow(res) == 1)
    {
        message("Found cached results in memory, using that")
        
        ## Get the record as a list
        metadata <- as.list(res)

        ## Check whether the object in the cache has
        ## expired. If it has, delete it from both the
        ## level 1 and level 2 cache and return NULL
        now <- lubridate::now()
        if (is.null(lifetime))
        {
            lifetime <- cache$lifetime
        }
        if (now - metadata$write_time >= lifetime)
        {
            message("Cached object has expired, removing from the cache")

            ## Cached object has expired, delete it from
            ## the level 1 cache
            cache$level1$meta <- cache$level1$meta %>%
                dplyr::filter(hash != !!hash)
            cache$level1$objects$hash <- NULL

            ## Also delete the entry from the level 2 cache, if
            ## it exists there
            if (dir.exists(cache$path))
            {
                ## Create the object filename and the metadata filename
                obj_file <- paste0(cache$path, "/", hash, ".obj.rds")
                meta_file <- paste0(cache$path, "/", hash, ".meta.rds")

                ## Now remove the files if they exist
                if (fs::file_exists(obj_file))
                {
                    file.remove(c(obj_file, meta_file))
                }
            }

            return(NULL)
        }

        ## Update the metadata
        metadata <- record_hit(metadata)

        ## Delete the entry from the level 1 cache
        cache$level1$meta <- cache$level1$meta %>%
            dplyr::filter(hash != !!hash)

        ## Now re-add the entry from the list
        cache$level1$meta <-
            dplyr::bind_rows(cache$level1$meta, metadata)

        ## Data present in level 1 cache
        return(cache$level1$objects[[hash]])
    }
    else if (nrow(res) != 0)
    {
        stop("Found multiple entries in the level 1 cache with the same hash, ",
             "cache is corrupt (report bug).")
    }

    ## If data is not in the L1 cache, check if L2 directory exists
    if (!dir.exists(cache$path))
    {
        return(NULL)
    }

    ## Create the object filename and the metadata filename
    obj_file <- paste0(cache$path, "/", hash, ".obj.rds")
    meta_file <- paste0(cache$path, "/", hash, ".meta.rds")

    ## Check to see if the files exist
    if (file.exists(meta_file))
    {
        if (file.exists(obj_file))
        {
            message("Found cached results on disk, reading RDS...")
            
            ## Open the meta file and check for expiry
            metadata <- readRDS(meta_file)
            now <- lubridate::now()
            if (is.null(lifetime))
            {
                lifetime <- cache$lifetime
            }
            if (now - metadata$write_time >= lifetime)
            {
                message("Cached object has expired, removing from the cache")

                ## Cached object has expired, delete the
                ## entry from the level 2 cache and return NULL
                file.remove(c(obj_file, meta_file))
                return(NULL)

            }

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
            stop("The cache is corrupt: missing .obj.rds file for .meta.rds file.")
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
##' @importFrom rlang .data
##' @export
show_cache <-function()
{
    tbl <- cache$level1$meta

    tbl <- tbl %>% tibble::add_column(in_memory = TRUE)

    ## Next, get the level 2 files and remove those that are in level 1
    file_list <- list.files(cache$path, pattern = "meta\\.rds") %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(value = stringr::str_replace(.data$value, ".meta.rds", "")) %>%
        dplyr::filter(!(.data$value %in% tbl$hash)) %>%
        dplyr::mutate(value = paste0(.data$value, ".meta.rds"))

    ## Make a function to get the data
    fn <- function(file) {
        meta_file <- paste0(cache$path, "/", file)
        readRDS(meta_file)
    }

    res <- file_list[["value"]] %>%  purrr::map_dfr(fn)
    tbl <- dplyr::bind_rows(tbl, res)

    ## Replace NA in mem column by FALSE
    tbl[is.na(tbl)] <- FALSE

    tbl %>% dplyr::arrange(dplyr::desc(.data$last_access))
}


##' Use this function to delete entries from the cache based on a
##' tibble returned by show_cache(). You can filter the tibble
##' prior to using the function to delete based on specific
##' criteria.
##'
##' @title Clear the cache
##' @param tbl An optional tbl specifying which objects in the
##' cache should be deleted. If this parameter is set to NULL
##' (the default if the tbl argument is not specified), then
##' everything in the cache is deleted
##'
##' @export
clear_cache <- function(tbl = NULL)
{
    if (is.null(tbl))
    {
        ## Delete everything in the cache

        ## Clear the level 1 cache
        cache$level1$meta = make_empty_cache()
        cache$level1$objects = list()

        ## Check if directory exists
        if (dir.exists(cache$path) && length(list.files(cache$path)) > 0)
        {
            list.files(cache$path) %>%
                stringr::str_c(cache$path,.) %>%
                purrr::map(file.remove)
        }
        message("Cleared cache.")
        invisible(cache)
    }
    else
    {
        ## Delete the elements listed in tbl

        ## Clear results from level1 cache
        cache$level1$meta <- cache$level1$meta %>%
            dplyr::filter(!(hash %in% tbl$hash))
        cache$level1$objects <-
            cache$level1$objects[!(names(cache$level1$objects) %in% tbl$hash)]

        ## Clear the results from the level2 cache
        if (dir.exists(cache$path))
        {
            for (hash in tbl$hash)
            {
                ## Create the object filename and the metadata filename
                obj_file <- paste0(cache$path, "/", hash, ".obj.rds")
                meta_file <- paste0(cache$path, "/", hash, ".meta.rds")

                file.remove(c(obj_file, meta_file))
            }
        }
    }
}
