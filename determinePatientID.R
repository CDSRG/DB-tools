


# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	require("RODBC")
}

buildSearchString <- function(target1 = NULL, target2 = NULL, target3 = NULL, target4 = NULL, target5 = NULL) {
	if (target1) {
		searchString <- paste("(COLUMN_NAME LIKE '", target1, "'", sep = 0)
	}
	if (target2) {
		searchString <- paste(searchString, " OR COLUMN_NAME LIKE '", target2, "'", sep = 0)
	}
	if (target3) {
		searchString <- paste(searchString, " OR COLUMN_NAME LIKE '", target3, "'", sep = 0)
	}
	if (target4) {
		searchString <- paste(searchString, " OR COLUMN_NAME LIKE '", target4, "'", sep = 0)
	}
	if (target5) {
		searchString <- paste(searchString, " OR COLUMN_NAME LIKE '", target5, "'", sep = 0)
	}
	searchString <- paste(searchString, ")")
	return(searchString)
}
buildSearchString <- function(target1 = NULL, target2 = NULL, target3 = NULL, target4 = NULL, target5 = NULL) {
	moreTargets <- c(target2, target3, target4, target5)
	searchString <- paste("(COLUMN_NAME LIKE '", target1, "'", sep = 0)
	for (i in 1:length(moreTargets)) { 
		searchString <- paste(searchString, " OR COLUMN_NAME LIKE '", moreTargets[i], "'", sep = 0)
	}
#use sapply instead?
	searchString <- paste(searchString, ")")
	return(searchString)
}
#possible to accept unknown number of parameters?

restrictDataType <- function(type1 = NULL, type2, = NULL, type3 = NULL) {
	restrictString <- paste(" AND DATA_TYPE IN ('", type1, "'", sep = 0)
	moreTypes <- c(type2, type3)
	for (i in length(moreTypes)) {
		restrictString <- paste(restrictString, ", '", moreTypes[i], "'", sep = 0)
	}
	restrictString <- paste(restrictString, ")")
	return(restrictString)
}
#same questions as above


buildTargetQuery <- function(x = searchString, y = restrictString, dbName = NULL) {
	targetQuery <- paste("SELECT TABLE_NAME, COLUMN_NAME FROM ", dbName, ".INFORMATION_SCHEMA.COLUMNS WHERE ", searchString, restrictString, sep = 0)
	return(targetQuery)
}


findTargets <- function(con, targetQuery, ...) {
	results <- as.data.frame(sqlQuery(con, query))
	return(results)
}
#add error checking all through
#over-functionalized? functions should call each other?
