install.packages('ggplot2')
library(ggplot2)

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
		assocDBviz(x, ...)
	}
)

# assocDB() method to handle two vectors
# should add try/catch statements in case either vector is null
setMethod("assocDB", "vector",
	function(x, y, ...) {
		x <- data.frame(x, y)
		assocDBviz(x, ...)
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
		assocDBviz(results, ...)
		return(results)
	}
)

# define function to visualize association of variables by scatter plot with quantile lines
assocDBviz <- function(x,...) {
	assocPlot <- ggplot(x, aes_string(x = "", y = "")) + geom_point() + geom_quantile()
	print(assocPlot)
}

# need to add variables to replace empty strings in aes_string call
# better to format plot as pdf as in upsetDBviz?
# need to find out more about mode==presence stuff in upsetDBviz
# this function works best for two continuous variables
# many more great possibilities in ggplot2
# need to work on other data types and variable types
# use set methods to get any type of data into data frame and then
#	use overloaded method calls to generate different plots based on data type?

