#get term dictionary
features <- read.csv("~/Desktop/text_features_edited.csv")
features <- features[!as.logical(features[,"exclude"]),]


#build terms dictionaries
terms <- list()
compound.terms <- list()
for (i in 1:(dim(features)[1])) {
	terms.i <- strsplit(as.character(features[i, 1])," ")
	compound.terms <- c(compound.terms, terms.i)
	terms.i <- unlist(terms.i)
	for (j in terms.i) {
		terms[j] <- NA
	}
}

counts <- function(text) {
	text <- unlist(strsplit(text, " "))
	term.counts <- data.frame(rep(0, dim(features)[1]), row.names=features[,1])
	if (length(text) < 1) {
		return(term.counts)
	}
	positions <- 1:length(text)
	for (i in names(terms)) {
		pos.i <- which(text==i)
		if (length(pos.i) < 1) next
		terms[i] <- list(positions[pos.i])
	}
	for (i in 1:length(compound.terms)) {
		if (length(compound.terms[[i]]) == 1) {
			term.counts[i, 1] <- length(which(!is.na(unlist(terms[compound.terms[[i]]]))))
		}
		else {
			a <- as.numeric(unlist(terms[compound.terms[[i]][1]]))
			b <- as.numeric(unlist(terms[compound.terms[[i]][2]]))
			if(length(a)>0 & length(b)>0) {
				match()
				term.counts[i, 1] <- length(which(!is.na(match(b,a+1))))
			}
		}
	}
	return(term.counts)
} 
