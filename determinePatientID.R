


# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	require("RODBC")
}


buildSearchString <- function(targets = NULL, match=c("exact", "like", "exclude", "notlike")) {
	# length(targets)==length(match) - or if length(targers)>1 and length(mathch)==1 -> then apply that same match type for all
	#various error checking here :) for types fo values, etc., matches should only be one of exact, like, exclude

	matches.exact <- which(match == "exact")
	exact.len <- length(matches.exact)
	matches.exclude <- which(match == "exclude")
	exclude.len <- length(matches.exclude)
	matches.like <- which(match == "like")
	like.len <- length(matches.like)
	matches.notlike <- which(match == "notlike")
	notlike.len <- length(matches.notlike)
	if (exact.len == 1) {
		queryString.exact <- paste("COLUMN_NAME == '", targets[matches.exact], "'", sep="")
	}
	else if (exact.len > 1) {
		queryString.exact <- paste("COLUMN_NAME IN (",
			paste("'", targets[matches.exact], "'", collapse=",", sep=""),
 			")", sep=""
 		)
	}
	if (like.len >= 1) {
		queryString.like <- paste("COLUMN_NAME LIKE '%", targets, "%'", collapse=" OR ", sep="")
	}
	if (exclude.len == 1) {
		queryString.exlude <- paste("COLUMN_NAME != '", targets[matches.exclude], "'", sep="")
	}
	else if (exclude.len > 1) {
		queryString.exclude <- paste("COLUMN_NAME NOT IN (",
			paste("'", targets[matches.exclude], "'", collapse=",", sep=""),
 			")", sep=""
 		)
	}
	if (notlike.len >= 1) {
		queryString.notlike <- paste("COLUMN_NAME NOT LIKE '%", 
			targets[matches.notlike], "%'", collapse=" OR ", sep=""
		)
	}
	searchString <- paste(if ((exact.len > 0) & (like.len > 0)) { "(" },
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
	return(searchString)
}

restrictDataType <- function(types = NULL) {
	# AND DATA_TYPE IN ('type1','type2',...)

	restrictString <- paste(" AND DATA_TYPE IN (", paste("'", types, "'", collapse=", ", sep=""), ")", sep = "")
	return(restrictString)
}



buildTargetQuery <- function(x = searchString, y = restrictString) {
	targetQuery <- paste("SELECT TABLE_NAME, COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE ", searchString, restrictString, sep = 0)
	return(targetQuery)
}


findTargets <- function(con, targetQuery, ...) {
	results <- as.data.frame(sqlQuery(con, query))
	return(results)
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

# create column names for final output
createOutputCols <- function(identifierColumns) {
	outputCols <- vector()
	for (i in 1:dim(identifierColumns)[1]) {
	colName <- paste(identifierColumns[i,1], ".", identifierColumns[i,2], sep="")
	outputCols <- c(outputCols, colName)
	}
	return(outputCols)
}
