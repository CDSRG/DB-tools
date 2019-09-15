# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial=TRUE, quietly = TRUE)) {
	message("installing missing package 'RODBC'")
	suppressMessages(install.packages("RODBC", quiet=TRUE))
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
}

# Load +/- install UpSetR package
if (!requireNamespace("UpSetR", partial=TRUE, quietly = TRUE)) {
	message("installing missing package 'UpSetR'")
	suppressMessages(install.packages("UpSetR", quiet=TRUE))
}
if (!isNamespaceLoaded("UpSetR")) {
	suppressPackageStartupMessages(require("UpSetR"))
}

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
suppressMessages(setMethod("upsetDB", "RODBC",
	function(x, query=NULL, table=NULL, use.columns=NULL, sample=NULL, verbose=TRUE, ...) {
		# test database connection and clear error log
		tryCatch(
			odbcClearError(x),
			error=function(e) {
				warning(e)
				return()
			}
		)
		# ensure argument 'sample' is number between 0.01-0.99 (%)
		if (!is.null(sample)) {
			if ((length(sample) != 1) || (is.na(sample)) || (!is.numeric(sample)) || (sample < 0.01) || (sample >= 1)) {
				warning("ignoring argument 'sample' (must be number between 0.01-0.99 [i.e. 1-99%])")
				sample <- FALSE			
			}
			else {
				sample <- as.integer(sample*100)
			}
		}
		else {
			sample <- FALSE
		}
		# perform SQL query with specified input
		if (!is.null(query)) {
			if(!is.character(query)) {
				warning("argument 'query' is not valid (must of type 'character')")
				return()
			}
			if (length(query) != 1) {
				warning("argument 'query' must specify a single SQL query")
				return()
			}
			# initiate query WITH table sampling - successful?
			if (verbose) {
				print("Attempting to query DB... ")
			}
			if (odbcQuery(x,
					paste(query, if (sample) {
						paste(" TABLESAMPLE (", sample, " PERCENT)", sep="")
					}, sep="")
			) < 0) {
				# if sampling error -> try full query and sample after return of full results
				if (sample) {
					odbcClearError(x)
					if (verbose) {
						print("ERROR\nAttempting modified query... ")
					}
					if (odbcQuery(x, query) < 0) {
						if (verbose) {
							print("ERROR\n")
						}
						warning("error evaluating query '", query, "' using provided DB connection")
						warning(odbcGetErrMsg(x))
						return()
					}
					if (verbose) {
						print("SUCCESS\n")
					}
					results <- getSqlResults(x, as.is=TRUE)
					nrows <- dim(results)[1]
					if (nrows >= 2) {
						results <- results[sample(nrows, size=floor(nrows*sample/100)),]
						if (dim(results)[1] < 2) {
							warning("query '", query, "' returned no actionable results after sampling")
							return(results)
						}
					}
				}
				# otherwise it's a query error -> return warnings and exit
				else {
					if (verbose) {
						print("ERROR\n")
					}
					warning("error evaluating query '", query, "' using provided DB connection")
					warning(odbcGetErrMsg(x))
					return()
				}
			}
			# successful query -> get results
			else {
				if (verbose) {
					print("SUCCESS\n")
				}
				results <- sqlGetResults(x, as.is=TRUE)
			}
		# perform SQL query of a specified table
		} else if (!is.null(table)) {
			if(!is.character(table)) {
				warning("argument 'table' is not valid (must be of type 'character')")
				return()
			}
			if (length(table) != 1) {
				warning("argument 'table' must specify a single table")
				return()
			}
			# test table existence and whether it is a 'BASE TABLE' (which can be tablesampled)
			table_alias <- unlist(strsplit(gsub("([][])","",table),"[.]"))
			table_alias <- table_alias[length(table_alias)]
			if (odbcQuery(x,
					paste(
						"SELECT TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME=N'",
						table_alias, "'", sep=""
					)
			) < 0) {
				warning(odbcGetErrMsg(x))
				return()
			}
			base.table <- tryCatch(
				(sqlGetResults(x) == "BASE TABLE"),
				error=function(e) {
					return(FALSE)
				}
			)
			# process 'use.columns' arg to match within table constraints
			if ((is.null(use.columns)) || (any(use.columns == "*"))) {
				use.columns <- "*"
			}
			else if (is.numeric(use.columns)) {
				table.columns <- try(sqlColumns(x, table_alias)[,"COLUMN_NAME"], silent=TRUE)
				use.columns <- try(
					table.columns[use.columns[which(
						(use.columns >= 1) & (use.columns <= length(table.columns))
					)]],
					silent=TRUE
				)
			}
			else if (all(is.na(use.columns))) {
				warning("cannot query NA columns in table ", table)
				return()
			}
			else {
				use.columns <- try(intersect(use.columns, sqlColumns(x, table_alias)[,"COLUMN_NAME"]), silent=TRUE)
			}
			if (length(use.columns) < 1) {
				warning("no matching columns to query in table ", table)
				return()
			}
			# run query on table - successful?
			if (verbose) {
				print("Attempting to query table in DB... ")
			}
			if (odbcQuery(x,
				paste(
					"SELECT ", 
					paste(use.columns, sep="", collapse=","),
					" FROM ", table, 
					if (sample & base.table) {
						paste(" TABLESAMPLE (", sample, " PERCENT)", sep="")
					} else if (sample) {
						# ALTERNATIVE METHOD HERE FOR SELECTING RANDOM ROWS FROM 'VIEW' TABLES
						# e.g. see https://www.brentozar.com/archive/2018/03/get-random-row-large-table/
						# or just do the select and then afterwards do the sample (like query section above)
					},
					sep=""
				)
			) < 0) {
				if (verbose) {
					print("ERROR\n")
				}
				warning("error querying table '", table, "' using provided DB connection")
				warning(odbcGetErrMsg(x))
				return()
			}
			if (verbose) {
				print("SUCCESS\n")
			}
			results <- sqlGetResults(x, as.is=TRUE)
		}
		else {
			warning("must specify either argument 'query' or 'table'")
			return()
		}
		# visualization for results
		if (verbose) {
			print("Generating upset plot visualization... ")
		}
		upsetDBviz(results, ...)
		if (verbose) {
			print("SUCCESS\n")
		}
		return(results)
	}
))


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
