Database <- R6::R6Class(
  "Database",
  public = list(
    #' Construct a Database object
    #'
    #' @param db_cred_file The absolute path of a configuration file (csv
    #' format) containing database connection information and credentials.
    #' The default value is the credential file stored in the inst/ directory
    #'
    #' @return A new (R6) Database object
    #' @export
    #'
    #' @examples
    #'
    #' db <- Database$new()
    initialize = function(db_cred_file = "db_cred_file.json")
    {
      if (!file.exists(db_cred_file))
        stop("Database connection failed, input credential file does not exist")
      json <- rjson::fromJSON(file = db_cred_file)
      print(json)
    }


  ),
  private = list(
    driver = NA_character_,
    server = NA_character_,
    host = NA_character_,
    trusted_connection = NA,
    user = NA_character_,
    password = NA_character_,
    name = NA_character_,
    schema = NA_character_,
    table = NA_character_,
    port = NA_integer_,
    connection = NA
  )
)


# The lines below is necessary to surpress a warning about no
# imports from R6 (see "https://stackoverflow.com/questions/64055049/
# unexpected-note-namespace-in-imports-field-not-imported-from-r6)"
#
#' @import R6
NULL
