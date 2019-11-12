# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial=TRUE, quietly = TRUE)) {
	message("installing missing package 'RODBC'")
	suppressMessages(install.packages("RODBC", quiet=TRUE))
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
}

# Load +/- install data.table package
if (!requireNamespace("data.table", partial=TRUE, quietly = TRUE)) {
	message("installing missing package 'data.table'")
	suppressMessages(install.packages("data.table", quiet=TRUE))
}
if (!isNamespaceLoaded("data.table")) {
	suppressPackageStartupMessages(require("data.table"))
}

vPrint <- function(verbose=FALSE, level=0, ...) {
  if (verbose > level) {
  	message <- paste(..., sep="")
  	if (length(message) > 0) {
	    print(message)
  	}
  }
}

prepQuery <- function(con, query=NULL, verbose=FALSE) {
	# test database connection and clear error log
	if (!RODBC:::odbcValidChannel(con)) {
		warning("Invalid DB connection")
		return(FALSE)
	}
	tryCatch(
		odbcClearError(con),
		error=function(e) {
			warning(e)
			return(FALSE)
		}
	)
	vPrint(verbose, 2, "Prepping query: ", query)
	if (odbcQuery(con, query) < 0) {
		warning("error evaluating query '", query, "' using provided DB connection")
		warning(odbcGetErrMsg(con))
		return(FALSE)
	}
	return(TRUE)
}

#function still needs to be tested/debugged/etc.
fetchQuery <- function(con, n=NULL, buffsize=1000, verbose=FALSE, keep=FALSE, FUN=NULL, ...) {
	# test database connection
	if (!RODBC:::odbcValidChannel(con)) {
		warning("Invalid DB connection")
		return(FALSE)
	}
	if (!is.numeric(n) | (length(n) != 1)) { 
		n <- 0
	}
	n <- max(0, floor(n), na.rm=TRUE)
	if (is.null(FUN)) {
		vPrint(verbose, 2, "Fetching query results", if (n > 0) { paste(" (n=", n, ")", sep="") })
		return(.Call(C_RODBCFetchRows, attr(con, "handle_ptr"), n, buffsize, NA_character_, TRUE))
	}
	FUN <- match.fun(FUN)
	counter <- 0
	results <- list()
	while(!identical(data <- .Call(C_RODBCFetchRows, attr(con, "handle_ptr"), n, buffsize, NA_character_, TRUE), -2)) {
		vPrint(verbose, 2, "Fetching query results", if (n > 0) { paste(" (", counter*n, "-", (counter+1)*n-1, ")", sep="") })
		counter <- counter + 1
		setDT(data)
		tryCatch(
			if (keep) {
				results[[counter]]$processed <- resultsforceAndCall(1, FUN, data, ...)
			} else {
				resultsforceAndCall(1, FUN, data, ...)
			},
			error=function(e) {
				warning(e)
				warning(odbcGetErrMsg(con))
				return(FALSE)
			}
		)
		if (keep) {
			results[[counter]]$raw <- data
		}
	}
	if (keep) { return(results) }
	return(TRUE)
}


###  CAN MULTIPLE CONNECTIONS RUN AT SAME TIME FROM R?!?!?  CAN THEN USE MULTIPLE CONNECTIONS TO GET PARTS OF RESULTS SIMULTANEOUSLY??
### The only workable method I've found to run multiple connections is to open multiple instances of R, and they don't share stuff 
### (stuff = data, environment?).  So you could do it, but you'd have to save the results parts and read them into your main instance.



#ICD9 <- c("99.%", "105.4", "531-534.999", "601-603.4")
#ICD10 <- c("K25-K28.999")
#whereConstruct(columns=list(ICD9code=ICD9, ICD10code=ICD10))


whereConstruct <- function(columns=list(), dates=NULL, required=NULL) {
	if (!is.list(columns) | length(columns) < 1) {
		return("")
	}
	fields <- names(columns)
	n <- length(fields)
	if (n < 1) {
		return("")
	}
	if (is.null(required) | all(is.na(required))) {
		required.fields <- rep(FALSE, n)
	}
	else if (identical(required, TRUE) | identical(required, "*")) {
		required.fields <- rep(TRUE, n)
	}
	else {
		required.fields <- fields %in% required
	}
	strings <- list()
	for (i in 1:n) {
		field <- fields[i]
		vals <- columns[[i]]
		vals.len <- length(vals)
		if (vals.len > 0) {
			vals.not <- grep("^[!]", vals)
#			vals.include <- setdiff(1:vals.len, vals.not)
			not.len <- length(vals.not)
			if (not.len > 0) {
				vals <- sub("^[!]", "", vals)
			}
			vals.range <- grep(".+[-].+", vals)
			vals.nrange <- intersect(vals.range, vals.not)
			vals.range <- setdiff(vals.range, vals.not)
			range.len <- length(vals.range)
			nrange.len <- length(vals.nrange)
			vals.like <- setdiff(grep("[%]", vals), c(vals.range, vals.nrange))
			vals.nlike <- intersect(vals.like, vals.not)
			vals.like <- setdiff(vals.like, vals.nlike)
			like.len <- length(vals.like)
			nlike.len <- length(vals.nlike)
			exact.len <- vals.len - (range.len + like.len + not.len)
			if (exact.len > 0) {
				vals.exact <- setdiff(1:vals.len, c(vals.range, vals.like, vals.not))
				vals.null <- which(vals[vals.exact] == "NULL")
				vals.exact <- setdiff(vals.exact, vals.null)
				null.len <- length(vals.null)
				exact.len <- exact.len - null.len
			}
			else {
				null.len <- 0
			}
			exclude.len <- not.len - (nrange.len + nlike.len)
			if (exclude.len > 0) {
				vals.exclude <- setdiff(vals.not, c(vals.nrange, vals.nlike))
				vals.nnull <- which(vals[vals.exclude] == "NULL")
				vals.exclude <- setdiff(vals.exclude, vals.nnull)
				nnull.len <- length(vals.nnull)
				exclude.len <- exclude.len - nnull.len
			}
			else {
				nnull.len <- 0
			}
			if (null.len > 0) {
				queryString.null <- paste(field, " IS NULL", sep="")
			}
			if (exact.len == 1) {
				queryString.exact <- paste(field, " == '", vals[vals.exact], "'", sep="")
			}
			else if (exact.len > 1) {
				queryString.exact <- paste(field, " IN (",
					paste("'", vals[vals.exact], "'", collapse=",", sep=""),
						")", sep=""
					)
			}
			if (like.len >= 1) {
				queryString.like <- paste(field, " LIKE '", vals[vals.like], "'", collapse=" OR ", sep="")
			}
			if (range.len >= 1) {
				vals.range <- unlist(lapply(
					strsplit(vals[vals.range],"-"), 
					function(x) {
						paste(" BETWEEN '", x[1], "' AND '",x[2], "'", sep="")
					}
				))
				queryString.range <- paste(
					if (range.len > 1) { "(" }, 
					field, 
					vals.range, 
					if (range.len > 1) { ")" }, 
					collapse=" OR ", sep=""
				)
			}
			if (nnull.len > 0) {
				queryString.nnull <- paste(field, " IS NOT NULL", sep="")
			}
			if (exclude.len == 1) {
				queryString.exclude <- paste(field, " != '", vals[vals.exclude], "'", sep="")
			}
			else if (exclude.len > 1) {
				queryString.exclude <- paste(field, " NOT IN (",
					paste("'", vals[vals.exclude], "'", collapse=",", sep=""),
	 				")", sep=""
	 			)
			}
			if (nlike.len >= 1) {
				queryString.nlike <- paste(field, " NOT LIKE '", 
					vals[vals.nlike], "'", collapse=" AND ", sep=""
				)
			}
			if (nrange.len >= 1) {
				vals.nrange <- unlist(lapply(
					strsplit(vals[vals.nrange],"-"), 
					function(x) {
						paste(" NOT BETWEEN '", x[1], "' AND '",x[2], "'", sep="")
					}
				))
				queryString.nrange <- paste(
					if (nrange.len > 1) { "(" }, 
					field, 
					vals.nrange, 
					if (nrange.len > 1) { ")" }, 
					collapse=" AND ", sep=""
				)
			}
			strings[[i]] <- paste(if (vals.len > not.len) { "(" },
				if (null.len > 0) { paste("(", queryString.null, ")", sep="") },
				if ((null.len > 0) & (exact.len > 0)) { " OR " },				
				if (exact.len > 0) { paste("(", queryString.exact, ")", sep="") },
				if ((exact.len > 0) & (like.len > 0)) { " OR " },
				if (like.len > 0) { paste("(", queryString.like, ")", sep="") },
				if ((like.len > 0) & (range.len > 0)) { " OR "},
				if (range.len > 0) { queryString.range },
				if (vals.len > not.len) { ")" },
				if ((vals.len > not.len) & (not.len > 0)) { " AND "},
				if (nnull.len > 0) {paste("(", queryString.nnull, ")", sep="") },
				if ((nnull.len > 0) & (exclude.len > 0)) { " AND " },				
				if (exclude.len > 0) {paste("(", queryString.exclude, ")", sep="") },
				if ((exclude.len > 0) & (nlike.len > 0)) { " AND " },
				if (nlike.len > 0) { paste("(", queryString.nlike, ")", sep="") },
				if ((nlike.len > 0) & (nrange.len > 0)) { " AND "},
				if (nrange.len > 0) { queryString.nrange },
				sep=""
			)
		}
		else {
			strings[[i]] <- ""
		}
	}
	valid.strings <- unlist(lapply(strings, function(x) { x != "" }))
	required.strings <- unlist(strings[required.fields & valid.strings])
	required.len <- length(required.strings)
	notrequired.strings <- unlist(strings[!required.fields & valid.strings])
	notrequired.len <- length(notrequired.strings)
	whereQuery <- paste("WHERE ",
		if ((notrequired.len > 0) & (required.len > 0)) { "(" },
		if (notrequired.len > 1) { "(" },
		if (notrequired.len > 0) { paste(notrequired.strings, collapse=" OR ") },
		if (notrequired.len > 1) { ")" },
		if ((notrequired.len > 0) & (required.len > 0)) { " AND " },
		if (required.len > 1) { "(" },
		if (required.len > 0) { paste(required.strings, collapse=" AND ") },
		if (required.len > 1) { ")" },
		if ((notrequired.len > 0) & (required.len > 0)) { ")" },
		sep=""
	)
	return(whereQuery)
}

