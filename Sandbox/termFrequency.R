###############
# termFrequency.R script describes term frequency counting method for database exploration
#
# Author/contact:  Cecelia.Madison@va.gov
###############

# load/install RODBC package
if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	message("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
}

# load/install tm package
if (!requireNamespace("tm", partial = TRUE, quietly = TRUE)) {
	message("installing missing package 'tm'")
	suppressMessages(install.packages("tm", quiet = TRUE))
}
if (!isNamespaceLoaded("tm")) {
	suppressPackageStartupMessages(require("tm"))
}

# define generic tfDB() function
tfDB <- function (x, ...) {
	UseMethod("tfDB", x)
}

# tfDB() method for texts as matrix input and terms as character input
### This method assumes/requires that the first column of the matrix is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "matrix"),
	function(x, ...) {
		tfDB(as.data.frame(x), ...)
	}
)


# tfDB() method for texts as dataframe input and terms as character input
### This method assumes/requires that the first column of the dataframe is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "data.frame"),
	function(x, terms=NULL, ...) {
		names(x) <- c("doc_id", "text")
		x <- x[,c("doc_id", "text")]
		textsCorpus <- SimpleCorpus(DataframeSource(x), control = list(language = "en"))
		if (is.null(terms)) {
			textsDTM <- DocumentTermMatrix(textsCorpus)
		}
		else {
			textsDTM <- DocumentTermMatrix(textsCorpus, control = list(
					dictionary = terms, 
					wordLengths=range(nchar(terms), na.rm=TRUE)
				)
			)			
		}
		return(list(textsCorpus=textsCorpus, textsDTM=textsDTM))
	}
)

# tfDB() method for texts as DB query results input and terms as character input
### This method assumes/requires that the first column of the results is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "RODBC"),
	function(x, terms=NULL, query = NULL, table=NULL, use.columns=NULL, identifiers=NULL, ...) {
		# test database connection and clear error log
		tryCatch(
			odbcClearError(x),
			error=function(e) {
				warning(e)
				return()
			}
		)
		if (length(identifiers) < 1) {
			warning("no matching identifier columns to query in table ", table)
			return()
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
			if (odbcQuery(x, query) < 0) {
				warning("error evaluating query '", query, "' using provided DB connection")
				warning(odbcGetErrMsg(x))
				return()
			}
			results <- getSqlResults(x, as.is=TRUE)
			if (dim(results)[1] < 1) {
				warning("query '", query, "' returned no actionable results")
				return(results)
			}
			if (dim(results)[2] < 2) {
				warning("query '", query, "' returned fewer than 2 columns")
				return(results)
			}
			if (!is.null(identifiers)) {
				doc_id <- which(names(results) %in% identifiers)]
				if (length(doc_id) < 1) {
					warning("no matching document identifier column(s)")
					return()
				} else if (length(doc_id) > 1) {
					warning("multiple matches to document identifier column(s), using first entry only: ", doc_id[1])
					doc_id <- doc_id[1]
				}
				names(results)[doc_id] <- "doc_id"
			}
			else {
				names(results)[1] <- "doc_id"
			}
			if (!is.null(use.columns)) {
				text <- which(names(results) %in% use.columns)]
				if (length(text) < 1) {
					warning("no matching text column(s)")
					return()
				} else if (length(text) > 1) {
					warning("multiple matches to document text column(s), using first entry only: ", text[1])
					text <- text[1]
				}
				names(results)[text] <- "text"
			}
			else {
				names(results)[2] <- "text"
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
			table_alias <- unlist(strsplit(gsub("([][])","",table),"[.]"))
			table_alias <- table_alias[length(table_alias)]
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
				warning("no matching text columns to query in table ", table)
				return()
			}
			# process 'identifiers' arg to match within table constraints
			### processing code here to get identifiers matching table columns!
			if (odbcQuery(x,
				paste(
					"SELECT ", 
					if (length(identifiers) > 1) {
						paste("CONCAT_WS(' ',", paste(identifiers, sep="", collapse=","), ") AS doc_id,", sep="")
					} else {
						paste(identifiers, " AS doc_id,", sep="")
					},
					if (length(use.columns) > 1) {
						paste("CONCAT_WS(' ',", paste(use.columns, sep="", collapse=","), ") AS text", sep="")
					} else {
						paste(use.columns, " AS text", sep="")
					},
					" FROM ", table,
					sep=""
				)
			) < 0) {
				warning("error querying table '", table, "' using provided DB connection")
				warning(odbcGetErrMsg(x))
				return()
			}
			results <- getSqlResults(x, as.is=TRUE)
		}
		else {
			warning("must specify either argument 'query' or 'table'")
			return()
		}
		results <- SimpleCorpus(DataframeSource(results), control = list(language = "en"))
		if (is.null(terms)) {
			textsDTM <- DocumentTermMatrix(results)
		}
		else {
			textsDTM <- DocumentTermMatrix(results, control = list(
					dictionary = terms, 
					wordLengths=range(nchar(terms), na.rm=TRUE)
				)
			)			
		}
		return(list(textsCorpus=results, textsDTM=textsDTM))
	}
)

