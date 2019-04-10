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
	function(x, query=NULL, ...) {
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
		upsetDBviz(results, ...)
		return(results)
	}
)

#original code acquires prepopulated data frame "data" before function begins
#modify to initialize "data" with DB connection during function call
#also modify to add statement assigning results of sql query to another variable -- 
# at conclusion of function, two variables will persist (one holding binary data, one holding original sql results) -- 
# two persiting variables will  represent the same data (correlate by index)


# DEFINE upsetDBviz() FUNCTION to visualize upset plot of columns
# -- in this case, tailored to summarize completeness of data
upsetDBviz <- function(data, pdf=NULL, width=NA, height=NA, nsets=NA, mode=NULL, ...) {
	ncols <- dim(data)[2]
	if (ncols <= 0) { 
		warning("cannot visualize upset plot for empty data")
		return()
	}
	mode <- match.arg(mode, choices=c("presence"))
	if (mode == "presence") {
		for (i in 1:ncols) {
			data[,i] <- as.numeric(data[,i])
			na.data <- is.na(data[,i])
			data[which(!na.data), i] <- 1
			data[which(na.data), i] <- 0
		}
	}

#test for boolean data and convert TRUE to 1 and FALSE to 0 if data is boolean 
#I don't know if SQL boolean data will come into R as logical or character.  Assuming logical.
#If it were the FALSE values that were important to know about, FALSE should be assigned 1 and TRUE 0 (no change for is.na).
#checks all values for data type? -- only needs to check one non-null entry per column

	if (class(data[,i]) == "logical") {
		data[which(data[,i] == TRUE), i] <- 1
		data[which(data[,i] == FALSE), i] <- 0
		data[which(is.na(data[,i])),i] <- 0
	}

#possible to write above assignments using an OR to put both 0 assignments in one statement?

#test for character data
#options: could convert to binary for simple presence or absence of values (0 if NULL, 1 if any data exists in specified element)
#options: could check each element for any of a list of particular strings (0 if NULL or not on list, 1 if value on list)

#below is option to convert based on presence or absence (the actual test of data type might not be necessary)
	if (class(data[,i]) == "character") {
		data[which(!is.na(data[,i])),i] <- 1
   		data[which(is.na(data[,i])),i] <- 0
	}

#below is option to convert based on finding certain strings
	if (class(data[,i]) == "character") {
		stringSearch <- c("one", "two", "three")
		data[(which(data[,i] %in% stringSearch)),i] <- 1
#above may not have correct parentheses?
		data[!(which(data[,i] %in% stringSearch)),i]] <- 0
		data[which(is.na(data[,i])),i] <- 0
	}

#below is the original code with an added test for numeric data
	if (class(data[,i]) == "numeric") {
		data[which(!is.na(data[,i])),i] <- 1
  		data[which(is.na(data[,i])),i] <- 0
	}

#thinking and intent behind above loop:
#use the original for loop to assess one column at a time and take action based on each column's data type



#attempt to modify call to upset() to include a variable for specifying which columns to visualize in the plot
#user will have to assign a vector of values to the variable
#assumption: okay to include index parameters with data frame when using upset function

	colsToVis <- c(3:12)
#variable initialized to values in Appendix 8

	if (is.na(width)) { width <- max(8, floor(ncols/5)) }
	if (is.na(height)) { height <- max(8, floor(ncols/2)) }
	if (is.na(nsets)) { nsets <- ncols }
	if (!is.null(pdf)) {
    	pdf(file=pdf, width=width, height=height)

#adding variable for parameters
		upset(data[,colsToVis], nsets=nsets, ...)
 

		dev.off()
	}
	else {

#adding variable for parameters
		upset(data[,colsToVis], nsets=nsets, ...)
	}
}
