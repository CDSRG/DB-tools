# Define generic upsetDB() function
upsetDB <- function (x, ...) {
	UseMethod("upsetDB", x)
}

# upsetDB() method to handle missing input
setMethod("upsetDB", "missing",
	function(x, ...) {
		warning("argument 'x' is missing, with no default")
		return()
	}
)

# upsetDB() method to handle NULL input
setMethod("upsetDB", "NULL",
	function(x, ...) {
		warning("argument 'x' cannot be NULL")
		return()
	}
)

# upsetDB() method to handle matrix input
setMethod("upsetDB", "matrix",
	function(x, ...) {
		upsetDB(as.data.frame(x), ...)
	}
)

# upsetDB() method to handle data.frame input
setMethod("upsetDB", "data.frame",
	function(x, ...) {
		upsetDBviz(x, ...)
	}
)

# upsetDB() method to handle DB connection + query input
setMethod("upsetDB", "RODBC",
	function(x, query=NULL, table=NULL, ...) {
		if (!is.null(query)) {
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
		} else if (!is.null(table)) {
			if(!is.character(table)) {
				warning("argument 'table' is not valid (must of type 'character')")
				return()
			}
		}
		else {
			warning("must specify either argument 'query' or 'table'")
			return()
		}
		upsetDBviz(results, ...)
		return(results)
	}
)


# DEFINE upsetDBviz() FUNCTION to visualize upset plot of columns
# -- in this case, tailored to summarize completeness of data
upsetDBviz <- function(data, pdf=NULL, width=NA, height=NA, nsets=NA, mode=NULL, use.columns=NULL, ...) {
	ncols <- dim(data)[2]
	if (ncols <= 0) { 
		warning("cannot visualize upset plot for empty data")
		return()
	}
	if (is.null(use.columns)) {
		cols <- 1:ncols
	}
	else {
		cols <- use.columns	
	}
	mode <- match.arg(mode, choices=c("presence"))
	if (mode == "presence") {
		for (i in cols) {
			data[,i] <- as.numeric(data[,i])
			na.data <- is.na(data[,i])
			data[which(!na.data), i] <- 1
			data[which(na.data), i] <- 0
		}
	}
	if (is.na(width)) { width <- max(8, floor(ncols/5)) }
	if (is.na(height)) { height <- max(8, floor(ncols/2)) }
	if (is.na(nsets)) { nsets <- ncols }
	if (!is.null(pdf)) {
    	pdf(file=pdf, width=width, height=height)

#adding variable for parameters
		upset(data[,cols], nsets=nsets, ...)
 		dev.off()
	}
	else {

#adding variable for parameters
		upset(data[,cols], nsets=nsets, ...)
	}
}
