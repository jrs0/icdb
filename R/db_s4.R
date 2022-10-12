#' Database class wrapping an SQL server connection
#'
#' @slot connection Microsoft SQL Server.
#' @slot config numeric.
#'
#' @return
#' @export
#'
#' @examples
setClass("DatabaseS4",
            slots = c(
                connection = "Microsoft SQL Server",
                config = "numeric",
                something = "numeric"
            )
)

setGeneric("something", function(x) {standardGeneric("something")})
setGeneric("something<-", function(x, value) { standardGeneric("something<-")})

setMethod("something", "DatabaseS4", function(x) { x@something })
setMethod("something<-", "DatabaseS4", function(x, value)
  {
    x@something <- value
    x
  })
