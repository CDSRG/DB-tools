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
	function(x, query=NULL, table=NULL, use.columns=NULL, sample=NULL, ...) {
		if ((!is.null(sample)) & (length(sample) == 1) & (is.numeric(sample)) & (sample >= 0.01) & (sample < 1)) {
			sample <- as.integer(sample*100)
		}
		else if (!is.null(sample)) {
			warning("ignoring argument 'sample' (must be number between 0.01-0.99 [i.e. 1-99%])")
			sample <- FALSE
		}
		else {
			sample <- FALSE
		}
		if (!is.null(query)) {
			if(!is.character(query)) {
				warning("argument 'query' is not valid (must of type 'character')")
				return()
			}
			if (sample) {
				query <- paste(query, " TABLESAMPLE (", sample, " PERCENT)", sep="")
			}
			results <- tryCatch(
				sqlQuery(x, query, ...),
				error = function(e) {
					warning(paste("error evaluating query '", query, "' using provided DB connection", sep=""))
					warning(e)
					return()
				}
			)
			# if results returns NADA because of tablesample error, can try rerunning without the sampling and then subselect results in R based on random select
			# this would involve test/check here and then rerunning of SQL statement to get query without tablesample
		} else if (!is.null(table)) {
			if(!is.character(table)) {
				warning("argument 'table' is not valid (must of type 'character')")
				return()
			}
			base.table <- tryCatch(
				sqlQuery(x, 
					paste(
						"SELECT TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME=N'",
						table, "'", sep=""
					)
				) == "BASE TABLE",
				error = function(e) {
					warning(paste("table '", table, "' is not a BASE TABLE", sep=""))
					warning(e)
					return(FALSE)
				}
			)
			if ((is.null(use.columns)) || (any(use.columns == "*"))) {
				use.columns <- "*"
			}
			else if (is.numeric(use.columns)) {
				table.columns <- try(sqlColumns(x, table)[,"COLUMN_NAME"], silent=TRUE)
				use.columns <- try(
					table.columns[use.columns[which(
						(use.columns >= 1) & (use.columns <= length(table.columns))
					)]],
					silent=TRUE
				)
			}
			else if (all(is.na(use.columns))) {
				warning(paste("cannot query NA columns in table ", table, sep=""))
				return()
			}
			else {
				use.columns <- try(intersect(use.columns, sqlColumns(x, table)[,"COLUMN_NAME"]), silent=TRUE)
			}
			if (length(use.columns) < 1) {
				warning(paste("no matching columns to query in table ", table, sep=""))
				return()
			}
			results <- tryCatch(
				sqlQuery(x, 
					paste(
						"SELECT ", 
						paste(use.columns, sep="", collapse=","),
						" FROM ", table, 
						if (sample & base.table) {
							paste(" TABLESAMPLE (", sample, " PERCENT)", sep="")
						} else if (sample) {
							# ALTERNATIVE METHOD HERE FOR SELECTING RANDOM ROWS FROM 'VIEW' TABLES
							# e.g. see https://www.brentozar.com/archive/2018/03/get-random-row-large-table/
						},
						sep=""
					),
					...
				),
				error = function(e) {
					warning(paste("error querying table '", table, "' using provided DB connection", sep=""))
					warning(e)
					return()
				}
			)
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
	if ((is.null(use.columns)) || (any(use.columns == "*"))) {
		cols <- 1:ncols
	}
	else if (all(is.na(use.columns))) {
		warning("cannot visualize upset plot for empty data (NA data in argument 'use.columns')")
		return()
	}
	else {
		cols <- use.columns[which(!is.na(use.columns))]
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
