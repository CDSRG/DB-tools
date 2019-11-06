# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial=TRUE, quietly = TRUE)) {
	message("installing missing package 'RODBC'")
	suppressMessages(install.packages("RODBC", quiet=TRUE))
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
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
fetchQuery <- function(con, n=NULL, verbose=FALSE, keep=FALSE, FUN=NULL, ...) {
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
		return(odbcFetchRows(con, max=n))
	}
	FUN <- match.fun(FUN)
	counter <- 0
	results <- list()
	while(!identical(data <- odbcFetchRows(con, max=n), -2)) {
		vPrint(verbose, 2, "Fetching query results", if (n > 0) { paste(" (", counter*n, "-", (counter+1)*n-1, ")", sep="") })
		counter <- counter + 1
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

ICD9 <- c("99.%", "105.4", "531-534.999", "601-603.4")
ICD10 <- c("K25-K28.999")

temp <- function(ICD9=NULL, ICD10=NULL, ICD9field="ICD9code", ICD10field="ICD10code") {
	ICD9.len <- length(ICD9)
	if (ICD9.len > 0) {
		ICD9.range <- grep("[-]", ICD9)
		range.len <- length(ICD9.range)
		ICD9.like <- setdiff(grep("[%]", ICD9), ICD9.range)
		like.len <- length(ICD9.like)
		exact.len <- ICD9.len - (range.len + like.len)
		if (exact.len > 0) {
			ICD9.exact <- setdiff(1:ICD9.len, c(ICD9.range, ICD9.like))
		}
		if (exact.len == 1) {
			queryString.exact <- paste(ICD9field, " == '", ICD9[ICD9.exact], "'", sep="")
		}
		else if (exact.len > 1) {
			queryString.exact <- paste(ICD9field, " IN (",
				paste("'", ICD9[ICD9.exact], "'", collapse=",", sep=""),
					")", sep=""
				)
		}
		if (like.len >= 1) {
			queryString.like <- paste(ICD9field, " LIKE '", ICD9[ICD9.like], "'", collapse=" OR ", sep="")
		}
		if (range.len >= 1) {
			ICD9.range <- unlist(lapply(
				strsplit(ICD9[ICD9.range],"-"), 
				function(x) {
					paste(" BETWEEN", x[1], "AND",x[2])
				}
			))
			queryString.range <- paste(
				if (range.len > 1) { "(" }, 
				ICD9field, 
				ICD9.range, 
				if (range.len > 1) { ")" }, 
				collapse=" OR ", sep=""
			)
		}

		ICD9String <- paste("(",
			if (exact.len > 0) { paste("(", queryString.exact, ")", sep="") },
			if ((exact.len > 0) & (like.len > 0)) { " OR " },
			if (like.len > 0) { paste("(", queryString.like, ")", sep="") },
			if ((like.len > 0) & (range.len > 0)) { " OR "},
			if (range.len > 0) { queryString.range },
			")",
			sep=""
		)
	}

	ICD10.len <- length(ICD10)
	if (ICD10.len > 0) {
		ICD10.range <- grep("[-]", ICD10)
		range.len <- length(ICD10.range)
		ICD10.like <- setdiff(grep("[%]", ICD10), ICD10.range)
		like.len <- length(ICD10.like)
		exact.len <- ICD10.len - (range.len + like.len)
		if (exact.len > 0) {
			ICD10.exact <- setdiff(1:ICD10.len, c(ICD10.range, ICD10.like))
		}
		if (exact.len == 1) {
			queryString.exact <- paste(ICD10field, " == '", ICD10[ICD10.exact], "'", sep="")
		}
		else if (exact.len > 1) {
			queryString.exact <- paste(ICD10field, " IN (",
				paste("'", ICD10[ICD10.exact], "'", collapse=",", sep=""),
					")", sep=""
				)
		}
		if (like.len >= 1) {
			queryString.like <- paste(ICD10field, " LIKE '", ICD10[ICD10.like], "'", collapse=" OR ", sep="")
		}
		if (range.len >= 1) {
			ICD10.range <- unlist(lapply(
				strsplit(ICD10[ICD10.range],"-"), 
				function(x) {
					paste(" BETWEEN", x[1], "AND",x[2])
				}
			))
			queryString.range <- paste(
				if (range.len > 1) { "(" }, 
				ICD10field, 
				ICD10.range, 
				if (range.len > 1) { ")" }, 
				collapse=" OR ", sep=""
			)
		}
		ICD10String <- paste("(",
			if (exact.len > 0) { paste("(", queryString.exact, ")", sep="") },
			if ((exact.len > 0) & (like.len > 0)) { " OR " },
			if (like.len > 0) { paste("(", queryString.like, ")", sep="") },
			if ((like.len > 0) & (range.len > 0)) { " OR "},
			if (range.len > 0) { queryString.range },
			")",
			sep=""
		)
	}

	ICDQuery <- paste("WHERE ",
		if (ICD9.len > 0) { ICD9String },
		if ((ICD9.len > 0) & (ICD10.len > 0)) { " OR " },
		if (ICD10.len > 0) { ICD10String },
		sep=""
	)

	return(ICDQuery)
}

