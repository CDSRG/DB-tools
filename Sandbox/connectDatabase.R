######### connectDB.R
### The purpose of this function is to prepare for and establish a connection to a database.
### first draft: 10/16/2019
### last edit: 10/16/2019

### input: server name, database name
### output: database connection

# define generic connectDB() function
connectDB <- function(server, database, ...) {
  UseMethod("connectDB", server, database)
}

# method to handle missing input
setMethod("connectDB", "missing", 
  function(server, database, ...) {
    warning("argument is missing, with no default")
    return()
  }
)

# method to handle NULL input
setMethod("connectDB", "NULL",
  function(server, database, ...) {
    warning("argument cannot be NULL")
    return()
  }
)

# method to handle character input
setMethod("connectDB", "character",
  function(server, database, ...) {
    # install RODBC package
    if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
      warning("installing missing package 'RODBC'")
      install.packages("RODBC", quiet = TRUE)
    }
    if (!isNamespaceLoaded("RODBC")) {
      suppressPackageStartupMessages(require("RODBC"))
    }
    # open channel
    conString <- paste("driver = {SQL Server}; server = ", server, "; database = ", database, "; trusted_connection=TRUE")
    con <- odbcDriverConnect(connection=conString)
    # test connection and clear error log
    tryCatch(odbcClearError(con), 
      error = function(e) {
        warning(e)
        return()
      }
    )
    return(con)
  }
)