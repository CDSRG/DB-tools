# load/install RODBC package
if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
}

# load/install tm package
if (!requireNamespace("tm", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'tm'")
	install.packages("tm", quiet = TRUE)
}
if (!isNamespaceLoaded("tm")) {
	suppressPackageStartupMessages(require("tm"))
}

# define generic tfDB() function
tfDB <- function (x, y = NULL, ...) {
	UseMethod("tfDB", x, y)
}

# tfDB() method for texts as matrix input and terms as character input
### This method assumes/requires that the first column of the matrix is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "matrix", y = "character"),
	function(x, y, ...) {
		tfDB(as.data.frame(x), y, ...)
	}
)

# tfDB() method for texts as matrix input and terms as NULL input
### This method assumes/requires that the first column of the matrix is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "matrix", y = "NULL"),
	function(x, y, ...) {
		tfDB(as.data.frame(x), NULL, ...)
	}
)

# tfDB() method for texts as dataframe input and terms as character input
### This method assumes/requires that the first column of the dataframe is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "data.frame", y = "character"),
	function(x, y, ...) {
		names(x) <- c("doc_id", "text")
		x <- x[,c("doc_id", "text")]
		textsCorpus <- SimpleCorpus(DataframeSource(x), control = list(language = "en"))
		textsDTM <- DocumentTermMatrix(textsCorpus, list(dictionary = y))
		return(list(textsCorpus=textsCorpus, textsDTM=textsDTM))
	}
)

# tfDB() method for texts as dataframe input and terms as NULL input
### This method assumes/requires that the first column of the dataframe is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "data.frame", y = "NULL"),
	function(x, y, ...) {
		names(x) <- c("doc_id", "text")
		x <- x[,c("doc_id", "text")]
		textsCorpus <- SimpleCorpus(DataframeSource(x), control = list(language = "en"))
		textsDTM <- DocumentTermMatrix(textsCorpus)
		return(list(textsCorpus=textsCorpus, textsDTM=textsDTM))
	}
)

# tfDB() method for texts as DB query results input and terms as character input
### This method assumes/requires that the first column of the results is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "RODBC", y = "character"),
	function(x, y, query = NULL, textColumn=NULL, table=NULL, docID=NULL, ...) {
		results <- sqlQuery(x, query)
		names(results) <- c("doc_id", "text")
		x <- x[,c("doc_id", "text")]
		textsCorpus <- SimpleCorpus(DataframeSource(x), control = list(language = "en"))
		textsDTM <- DocumentTermMatrix(textsCorpus, list(dictionary = y))
		return(list(textsCorpus=textsCorpus, textsDTM=textsDTM))
	}
)

# tfDB() method for texts as DB query results input and terms as NULL input
### This method assumes/requires that the first column of the results is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "RODBC", y = "NULL"),
	function(x, y, query = NULL, textColumn=NULL, table=NULL, docID=NULL, ...) {
		results <- sqlQuery(x, query)
		names(results) <- c("doc_id", "text")
		x <- x[,c("doc_id", "text")]
		textsCorpus <- SimpleCorpus(DataframeSource(x), control = list(language = "en"))
		textsDTM <- DocumentTermMatrix(textsCorpus)
		return(list(textsCorpus=textsCorpus, textsDTM=textsDTM))
	}
)
