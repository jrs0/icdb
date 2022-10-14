#' Class for managing a query results cache
#'
#' @slot connection Microsoft SQL Server.
#' @slot config list. Database connection information as a named list
#' @slot dsn
#'
#' @export
setClass(
  "Cache",
  slots = representation(
    path = "character"
  ),
  prototype = prototype(
    path = "cache/"
  )
)


#' Create a new cache object
#'
#' @return A new Cache object
Cache <- function(path = "cache/")
{
  # Create the new object
  c <- new("Cache", path = path)

  # Create the directory if it does not exist
  if (!dir.exists(path))
  {
    dir.create(path)
  }

  # Return the new object
  c
}


setMethod("summary", "Cache", function(object, ...) {

  # Open the cache folder and count the number of items

  # Print the disk space used

  # Show other summaries

})

setGeneric("getContents", function(c) standardGeneric("getContents"))

setMethod("getContents", "Cache", function(c) {


})

#' Write an element into the cache
#'
#' This function creates an entry in the cache. It should not be called by the
#' user of the package. The data comprises two parts: an object, which is
#' typically large, which is used to store "results" in the cache. The second
#' part is data about the results, which is used to generate a key. The
#' data can be anything, but a named list is a simple way to organise the
#' necessary data.
#'
#' This function will not check the cache before writing to it, so any duplicate
#' data will be overwritten. Only call this function after establish via
#' readCache that the data is not already present in the cache.
#'
#' @param cache A Cache object in which to store the data
#' @param object The main (typically large) object to store in the cache
#' @param data Data about the object, used to generate the hash
#'
writeCache <- function(cache, object, data)
{
  # Create the directory if it does not exist
  if (!dir.exists(cache@path))
  {
    dir.create(cache@path)
  }

  # Make the has out of the metadata
  hash <- rlang::hash(data)

  # Create the object filename and the metadata filename
  obj_file <- paste0(cache@path, "/", hash, ".obj.rds")
  meta_file <- paste0(cache@path, "/", hash, ".meta.rds")

  # Store the object
  saveRDS(object, file = obj_file)

  # The metadata is not saved directly. Instead, it is encapsulated inside a
  # structure that also holds information generic to all cache entries: the
  # number of hits, last access, etc. This information is used to track how
  # the entry is used, and provide summary information.
  metadata <- list(
    hits = 0,
    write_time = Sys.time(),
    last_access = NULL,

    # Finally, store the user provided data
    data = data
  )

  # Write the metadata to the file
  saveRDS(metadata, file = meta_file)

}
