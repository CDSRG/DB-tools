


# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	require("RODBC")
}

# 
buildSearchString <- function(targets = NULL, match=c("exact", "like", "exclude", "notlike")) {
	# length(targets)==length(match) - or if length(targers)>1 and length(mathch)==1 -> then apply that same match type for all
	#various error checking here :) for types fo values, etc., matches should only be one of exact, like, exclude

	matches.exact <- which(match == "exact")
	matches.exclude <- which(match == "exclude")
	matches.like <- which(match == "like")
	matches.notlike <- which(match == "notlike")
	if (length(matches.exact) == 1) {
		# COLUMN_NAME == 'targets[matches.exact]'
	}
	else if (length(matches.exact) > 1) {
		# COLUMN_NAME IN ('target1','target2',...)
	}

	if (length(matches.like) >= 1) {
		# COLUMN_NAME LIKE 'target1' OR COLUMN_NAME LIKE 'target2', ...
	}
	if (length(matches.exclude) == 1) {
		# COLUMN_NAME !=target
	}
	else if (length(matches.exclude) > 1) {
		# COLUMN_NAME NOT IN ('targe1','target2',...)
	}
	if (length(matches.notlike) >= 1) {
		# COLUMN_NAME NOT LIKE target1 AND COLUMN_NAME NOT LIKE target2, ....
	}
	# ((exact) OR (like)) AND ((exclude) OR (notlike))

	searchString <- paste("(COLUMN_NAME LIKE ", paste("'", targets, "'", collapse=" OR COLUMN_NAME LIKE ", sep=""), ")", sep="")
	return(searchString)
}

#possible to accept unknown number of parameters?

restrictDataType <- function(types = NULL) {
	# AND DATA_TYPE IN ('type1','type2',...)
	restrictString <- paste(" AND DATA_TYPE IN ('", type1, "'", sep = 0)
	moreTypes <- c(type2, type3)
	for (i in length(moreTypes)) {
		restrictString <- paste(restrictString, ", '", moreTypes[i], "'", sep = 0)
	}
	restrictString <- paste(restrictString, ")")
	return(restrictString)
}
#same questions as above


buildTargetQuery <- function(x = searchString, y = restrictString) {
	targetQuery <- paste("SELECT TABLE_NAME, COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE ", searchString, restrictString, sep = 0)
	return(targetQuery)
}


findTargets <- function(con, targetQuery, ...) {
	results <- as.data.frame(sqlQuery(con, query))
	return(results)
}
#add error checking all through
#over-functionalized? functions should call each other?
