###############
# termFrequency.R script describes term frequency counting method for database exploration
#
# Author/contact:  Cecelia.Madison@va.gov
###############

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
			textsDTM <- DocumentTermMatrix(textsCorpus, list(dictionary = terms))			
		}
		return(list(textsCorpus=textsCorpus, textsDTM=textsDTM))
	}
)

# tfDB() method for texts as DB query results input and terms as character input
### This method assumes/requires that the first column of the results is the document identifier and the second column is the document text!
setMethod("tfDB", signature(x = "RODBC"),
	function(x, terms=NULL, query = NULL, textColumn=NULL, table=NULL, docID=NULL, ...) {
		results <- sqlQuery(x, query)
		names(results) <- c("doc_id", "text")
		x <- x[,c("doc_id", "text")]
		textsCorpus <- SimpleCorpus(DataframeSource(x), control = list(language = "en"))
		if (is.null(terms)) {
			textsDTM <- DocumentTermMatrix(textsCorpus)
		}
		else {
			textsDTM <- DocumentTermMatrix(textsCorpus, list(dictionary = terms))			
		}
		return(list(textsCorpus=textsCorpus, textsDTM=textsDTM))
	}
)

