# Load +/- install RODBC package
if (!requireNamespace("RODBC", partial=TRUE, quietly = TRUE)) {
	message("installing missing package 'RODBC'")
	suppressMessages(install.packages("RODBC", quiet=TRUE))
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
}

# Load +/- install logger package
if (!requireNamespace("logger", partial=TRUE, quietly = TRUE)) {
	message("installing missing package 'logger'")
	suppressMessages(install.packages("logger", quiet=TRUE))
}
if (!isNamespaceLoaded("logger")) {
	suppressPackageStartupMessages(require("logger"))
}

#specify logging behavior however you wish... e.g.:
#log_appender(index=1)
#t <- tempfile()
#log_appender(appender_file(t), index=2)

prepQuery <- function(con, query=NULL, rows_at_time=attr(con, "rows_at_time")) {
	# test database connection and clear error log
	if (!RODBC:::odbcValidChannel(con)) {
		log_error("Invalid DB connection")
		return(FALSE)
	}
	tryCatch(
		odbcClearError(con),
		error=function(e) {
			log_error(e$message)
			return(FALSE)
		}
	)
	if (is.null(query)) {
		log_error("Missing value for 'query'")
		return(FALSE)
	}
	if (!is.character(query)) {
		log_warn("Converting from non-character value provided for 'query'")
		query <- as.character(query)
	}
	if (length(query) != 1) {
		log_error("Single value required for 'query'")
		return(FALSE)
	}
	if (nchar(query) < 1) {
		log_error("Empty 'query' provided")
		return(FALSE)
	}
	log_info("Prepping query: ", query)	
	if (.Call(RODBC:::C_RODBCQuery, attr(con, "handle_ptr"), query, as.integer(rows_at_time)) < 0) {
		log_error("Error evaluating query using provided DB connection")
		log_error(odbcGetErrMsg(con))
		return(FALSE)
	}
	return(TRUE)
}


fetchQuery <- function(con, n=NULL, buffsize=1000, FUN=NULL, as.is=FALSE, ...) {
	# test database connection
	if (!RODBC:::odbcValidChannel(con)) {
		log_error("Invalid DB connection")
		return(FALSE)
	}
	cols <- .Call(RODBC:::C_RODBCNumCols, attr(con, "handle_ptr"))
	if (cols < 0L) {
		log_error("No data")
        return(FALSE)
    }
    cData <- .Call(RODBC:::C_RODBCColData, attr(con, "handle_ptr"))
	if (!is.numeric(n) | (length(n) != 1)) { 
		n <- 0
	}
	n <- max(0, floor(n), na.rm=TRUE)
	if (is.logical(as.is) & length(as.is) == 1) {
		as.is <- rep(as.is, length=cols)
    }
	else if (is.numeric(as.is)) {
		if (any(as.is < 1 | as.is > cols)) 
			log_warn("invalid numeric 'as.is' values: ", as.is[which(as.is < 1 | as.is > cols)])
		as.is <- as.is[which(as.is >= 1 & as.is <= cols)]
		i <- rep(FALSE, cols)
		i[as.is] <- TRUE
		as.is <- i
	}
	else if (is.character(as.is)) {
		as.is <- cData$names %in% as.is
	}
	if (length(as.is) != cols) {
		log_error("'as.is' has the wrong length ", length(as.is), " != cols = ", cols)
		return(FALSE)
	}
	as.is <- which(as.is)
	if (is.null(FUN)) {
		FUN <- return
		use.dots <- FALSE
	}
	else {
		tryCatch(
			FUN <- match.fun(FUN),
			error=function(e) {
				log_error(e$message)
				return(FALSE)				
			}
		)
		use.dots <- (...length() > 0L) & (length(formals(FUN)) > 1L)
	}
	counter <- 0
	nresults <- 0
	repeat {
		data <- tryCatch(.Call(RODBC:::C_RODBCFetchRows, attr(con, "handle_ptr"), n, buffsize, NA_character_, TRUE),
					error=function(e) {
						log_error(e)
						return(list(stat=-3))
					})
		if ((data$stat) < 0L) {
			switch(data$stat,
				-3 = log_error(paste0("Interrupted Connection: ", odbcGetErrMsg(con))),
				-2 = log_error(paste0("No Data: ", odbcGetErrMsg(con))),
				if (counter <= 0L) {
					log_error(odbcGetErrMsg(con))
				} else {
					log_info("Completed fetch (", nresults, " results)")
				}
			)
			break
		}
		log_info("Fetching query results", 
			if (n > 0) { 
				paste(" (", floor(counter*n), "-", (counter+1)*n-1, ")", sep="") 
			}
			else {
				" (all)"
			}
		)
		counter <- counter + 1
		names(data$data) <- cData$names
		for (i in as.is) {
			tryCatch(
				switch(cData$type[i],
					int = data$data[[i]] <- as.integer(data$data[[i]]),
					smallint = data$data[[i]] <- as.integer(data$data[[i]]),
					decimal = data$data[[i]] <- as.numeric(data$data[[i]]),
					date = data$data[[i]] <- as.Date(data$data[[i]]),
					timestamp = data$data[[i]] <- as.POSIXct(data$data[[i]]),
					unknown = data$data[[i]] <- type.convert(data$data[[i]])
         		),
         		error=function(e) {
					log_warn("Error converting ", cData$names[i], ": ", e$message)
				}
			)
		}
		tryCatch(
			if (use.dots) {
				forceAndCall(1, FUN, data$data, ...)
			}
			else {
				forceAndCall(1, FUN, data$data)
			},
			error=function(e) {
				log_error(e$message)
				log_error(odbcGetErrMsg(con))
				return(FALSE)
			}
		)
		nresults <- nresults + length(data$data[[1]])
	}
	return(TRUE)
}


############################
############################
### must test in sql -- is "NOT BETWEEN" valid? -- tend to think not
### also what about > and <?  can we do those?


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
				queryString.exact <- paste(field, " = '", vals[vals.exact], "'", sep="")
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

