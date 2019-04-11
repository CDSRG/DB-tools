# define generic strength of association function
assocDB <- function (x, y=NULL, ...) {
	UseMethod("assocDB", x)
}

# assocDB() method to handle missing input
setMethod("assocDB", "missing",
	function(x, ...) {
		warning("argument 'x' is missing, with no default")
		return()
	}
)

# assocDB() method to handle NULL input
setMethod("assocDB", "NULL",
	function(x, ...) {
		warning("argument 'x' cannot be NULL")
		return()
	}
)

# assocDB() method to handle matrix input
setMethod("assocDB", "matrix",
	function(x, ...) {
		assocDB(as.data.frame(x), ...)
	}
)

# assocDB() method to handle data.frame input
setMethod("assocDB", "data.frame",
	function(x, ...) {
		# CALL TO ACTION METHOD PASSING (x, ...)
	}
)

# assocDB() method to handle two vectors
# should add try/catch statements in case either vector is null
setMethod("assocDB", "vector",
	function(x, y, ...) {
		x <- data.frame(x, y)
		# CALL TO ACTION METHOD PASSING (x, ...)
	}
)

# assocDB() method to handle DB connection + query input
setMethod("assocDB", "RODBC",
	function(x, query=NULL, ...) {
		if(!is.character(query)) {
			warning("argument 'query' is not valid (must of type 'character')")
			return()
		}
		results <- tryCatch(
			sqlQuery(x, query, ...),
			error = function(e) {
				warning(paste("error evaluating query '", query, "' using provided DB connection", sep=""))
				warning(e)
				return()
			}
		)
		# CALL TO ACTION METHOD PASSING (results, ...)
		return(results)
	}
)

