


# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	require("RODBC")
}

## function description here -- lists/explains each variable input, overall output, etc.
###
###
###
newFunction <- function(con, tableNames=NULL, targets=NULL, match=NULL, types=NULL, field="COLUMN_NAME") {
	# test database connection and clear error log
	tryCatch(
		odbcClearError(con),
		error=function(e) {
			warning(e)
			return()
		}
	)
	# allow for using ALL tables (as default)
	# build in error wrapping / handling for all these queries
	#possible also to write this code using individual tableExists checks
	tables <- as.character(sqlQuery(con, "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES")[,1])
	if ((is.null(tableNames)) || (any(tableNames == "*"))) {
		tableNames <- tables
	}
	else {
		tableNames <- intersect(as.character(tableNames), tables)
		#can do some error/warning throwing here for ones that didn't match
	}

		if (is.null(targets)) {
			warning("argument 'targets' must specify one or more column name(s)")
			return("")
		}
		else if (all(is.na(targets))) {
			warning("argument 'targets' must specify one or more column name(s)")
			return("")
		}
		else if (!is.character(targets)) {
			warning("argument 'targets' must specify one or more column name(s)")
			return("")
		}
		else if (any(grepl("'", targets))) {
			warning("value(s) in 'targets' cannot contain quotes: ", targets[grepl("'", targets)])
			targets <- targets[!grepl("'", targets)]
		}
		if (length(targets) < 1) {
			warning("argument 'targets' must specify one or more column name(s)")
			return("")
		}
		if (is.null(match)) {
			warning("argument 'match' must specify matching parameter(s) for each column name in 'targets'")
			return("")
		}
		else if (all(is.na(match))) {
			warning("argument 'match' must specify matching parameter(s) for each column name in 'targets'")
			return("")
		}
		else if (!is.character(match)) {
			warning("argument 'match' must specify matching parameter(s) for each column name in 'targets'")
			return("")
		}
		if (any(!match %in% c("exact", "like", "exclude", "notlike", "regexp"))) {
			warning("ignoring unexpected value(s) of 'match': ", 
				paste(match[which(!match %in% c("exact", "like", "exclude", "notlike", "regexp"))], collapse=", ")
			)
			match <- match[which(match %in% c("exact", "like", "exclude", "notlike", "regexp"))]
		}
		if (length(match) < 1) {
			warning("argument 'match' must specify matching parameter(s) for each column name in 'targets'")
			return("")
		}
		if (length(targets) != length(match)) {
			if (length(match) == 1) {
				match <- rep(match, length(targets))
			}
			else {
				warning("lengths of arguments 'targets' and 'match' must be the same")
				return("")
			}
		}
		if ((!is.character(field)) || (length(field) != 1)) {
			warning("argument 'field' must specify a single column name to search")
			return("")
		} 
		matches.exact <- which(match == "exact")
		exact.len <- length(matches.exact)
		matches.exclude <- which(match == "exclude")
		exclude.len <- length(matches.exclude)
		matches.like <- which(match == "like")
		like.len <- length(matches.like)
		matches.notlike <- which(match == "notlike")
		notlike.len <- length(matches.notlike)
		matches.regexp <- which(match == "regexp")
		regexp.len <- length(matches.regexp)
		if (exact.len == 1) {
			queryString.exact <- paste(field, " == '", targets[matches.exact], "'", sep="")
		}
		else if (exact.len > 1) {
			queryString.exact <- paste(field, " IN (",
				paste("'", targets[matches.exact], "'", collapse=",", sep=""),
	 			")", sep=""
	 		)
		}
		if (like.len >= 1) {
			queryString.like <- paste(field, " LIKE '%", targets[matches.like], "%'", collapse=" OR ", sep="")
		}
		if (exclude.len == 1) {
			queryString.exclude <- paste(field, " != '", targets[matches.exclude], "'", sep="")
		}
		else if (exclude.len > 1) {
			queryString.exclude <- paste(field, " NOT IN (",
				paste("'", targets[matches.exclude], "'", collapse=",", sep=""),
	 			")", sep=""
	 		)
		}
		if (notlike.len >= 1) {
			queryString.notlike <- paste(field, " NOT LIKE '%", 
				targets[matches.notlike], "%'", collapse=" OR ", sep=""
			)
		}
		searchString <- paste("WHERE ",
			if ((exact.len > 0) & (like.len > 0)) { "(" },
			if (exact.len > 0) { paste("(", queryString.exact, ")", sep="") },
			if ((exact.len > 0) & (like.len > 0)) { " OR " },
			if (like.len > 0) { paste("(", queryString.like, ")", sep="") },
			if ((exact.len > 0) & (like.len > 0)) { ")" },
			if ((exact.len+like.len > 0) & (exclude.len+notlike.len > 0)) { " AND " },
			if ((exclude.len > 0) & (notlike.len > 0)) { "(" },
			if (exclude.len > 0) { paste("(", queryString.exclude, ")", sep="") },
			if ((exclude.len > 0) & (notlike.len > 0)) { " OR " },
			if (notlike.len > 0) { paste("(", queryString.notlike, ")", sep="") },
			if ((exclude.len > 0) & (notlike.len > 0)) { ")" },
			sep=""
		)

		# AND DATA_TYPE IN ('type1','type2',...)

		restrictString <- paste(" AND DATA_TYPE IN (", paste("'", types, "'", collapse=", ", sep=""), ")", sep = "")

	targetQuery <- as.data.frame(
		sqlQuery(
			con, 
			paste("SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS ", 
					searchString, 
					restrictString, 
					sep = ""
			)
		)
	)
	
	## insert regexp processing here -- query all columns from schema and then in R perform regexp matching
	## then do unique combo of targetQuery and regexp-processed list to return full set
	
	return(targetQuery[which(targetQuery[,1] %in% tableNames),])
}







findIdentifiers <- function(results, con, ...) {

	tableNames <- unique(results[,1])

	identifierColumns <- data.frame(matrix(ncol=0,nrow=0))

	for (i in 1:length(tableNames)) {

		idQuery <- paste("SELECT TABLE_NAME, COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS
		WHERE COLUMN_NAME IN (SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS 
		WHERE TABLE_NAME = 'CohortCrosswalk') AND TABLE_NAME = '",
		tableNames[i], "'", sep = "")

	idColumns <- sqlQuery(con, idQuery)

	identifierColumns <- rbind(identifierColumns, idColumns)

	return(identifierColumns)
	}
}




# two data frames for final output?  one holding target values and one holding identifier values?
# or one data frame for final output with target and identifier values?
# using two is more complicated for ensuring values stay matched by entity
# drawbacks to using one? can subset it for analysis?
# make data frame with an  independent ID as one column and outputColumns as the other columns -- this will hold identifier values
# repeat with results -- this will hold target values
# match identifier column and target column using identifierColumns and results (RENAME RESULTS) - link by table name
# match output dataframes by independent IDs



# create column names for final output
# below gives identifier table.column names
createOutputColsID <- function(identifierColumns) {
	outputColsID <- vector()
	for (i in 1:dim(identifierColumns)[1]) {
	colName <- paste(identifierColumns[i,1], ".", identifierColumns[i,2], sep="")
	outputColsID <- c(outputColsID, colName)
	}
	return(outputColsID)
}

# below gives target table.column names
createOutputColsTarget <- function(results) {
	outputColsTarget <- vector()
	for (i in 1:dim(results)[1]) {
	colName <- paste(results[i,1], ".", results[i,2], sep="")
	outputColsTarget <- c(outputColsTarget, colName)
	}
	return(outputColsTarget)
}

#

