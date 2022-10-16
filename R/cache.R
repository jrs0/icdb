##' Class for managing a query results cache
##'
##' @slot connection Microsoft SQL Server.
##' @slot config list. Database connection information as a named list
##' @slot dsn
##'
##' @export
setClass(
    "Cache",
    slots = representation(
        level1 = "list",
        path = "character"
    ),
    prototype = prototype(
        path = "cache/",
        level1 = list()
        
    )
)


##' Create a new cache object
##'
##' @return A new Cache object
Cache <- function(path = "cache/")
{
    ## Create the new object
    c <- new("Cache", path = path)

    ## Create the directory if it does not exist
    if (!dir.exists(path))
    {
        dir.create(path)
    }

    ## Return the new object
    c
}


setMethod("summary", "Cache", function(object, ...) {

    ## Open the cache folder and count the number of items
    
    ## Print the disk space used
    
    ## Show other summaries
    
})

setGeneric("getContents", function(c) standardGeneric("getContents"))

setMethod("getContents", "Cache", function(c) {


})

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
##' @param cache A Cache object in which to store the data
##' @param data Data about the object, used to generate the hash
##' @param object The main (typically large) object to store in the cache
##' @return The modified cache object
##'
writeCache <- function(cache, data, object)
{
    ## Make the hash out of the metadata
    hash <- rlang::hash(data)
    
    ## The metadata is not saved directly. Instead, it is encapsulated inside a
    ## structure that also holds information generic to all cache entries: the
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

    ## Write the object to the level 1 cache first
    cache@level1[[hash]] <- list(metadata = metadata, object = object)

    ## Create the level 2 directory if it does not exist
    if (!dir.exists(cache@path))
    {
        dir.create(cache@path)
    }
    
    ## Create the object filename and the metadata filename
    obj_file <- paste0(cache@path, "/", hash, ".obj.rds")
    meta_file <- paste0(cache@path, "/", hash, ".meta.rds")

    ## Store the metadata and the object to the level 2 cache directory
    saveRDS(metadata, file = meta_file)
    saveRDS(object, file = obj_file)

    ## Return the cache object
    cache
}

##' Read an object from the cache
##'
##' Use this function to get an object from
##'
##' @param cache The Cache object to read from
##' @param data The same data object passed to the writeCache function
##'
##' @return Returns the object associated with the data. Null if not found.
readCache <- function(cache, data)
{
    ## Make the has out of the metadata
    hash <- rlang::hash(data)

    ## First, attempt to read the data from the level 1 cache
    val <- cache@level1[[hash]]
    if (!is.null(val))
    {
        message("Found data in level 1 cache, using that.")
        
        ## Update the metadata
        val$metadata$hits <- val$metadata$hits + 1
        val$metadata$last_access <- Sys.time()

        ## Now return the object
        val$object
    }
    
    ## If data is not in the L1 cache, check if L2 directory exists
    if (!dir.exists(cache@path))
    {
        NULL
    }

    ## Create the object filename and the metadata filename
    obj_file <- paste0(cache@path, "/", hash, ".obj.rds")
    meta_file <- paste0(cache@path, "/", hash, ".meta.rds")

    ## Check to see if the files exist
    if (file.exists(meta_file))
    {
        if (file.exists(obj_file))
        {
            ## Open the meta file and increment the update values
            metadata <- readRDS(meta_file)
            metadata$hits <-  metadata$hits + 1
            metadata$last_access <- Sys.time()
            saveRDS(metadata, file = meta_file)

            ## Now open and return the object
            readRDS(obj_file)
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

setMethod("summary", "Cache", function(object, ...) {

    message("The cache is stored in the folder: ", object@path)

    ## Get the list of files
    file_list <- list.files(object@path, pattern = "meta\\.rds")

    ## Make a function to get the data
    get_metadata <- function(file) {
        meta_file <- paste0(object@path, "/", file)
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
})

##' Clear the cache
##'
##' Delete all the cached results in the cache folder.
##'
setGeneric("clear", function(cache) standardGeneric("clear"))

##' Clear the cache
##'
##' Delete all the cached results in the cache folder.
##'
setMethod("clear", "Cache", function(cache) {
    ## Check if directory exists
    if (dir.exists(cache@path) && length(list.files(cache@path)) > 0)
    {
        list.files(cache@path) %>%
            stringr::str_c(cache@path,.) %>%
            purrr::map(file.remove)
        message("Cleared cache.")
    }
    else
    {
        message("Cache already empty.")
    }
    invisible(cache)
})

