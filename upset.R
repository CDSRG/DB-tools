# DEFINE table_column_upset() FUNCTION to visualize upset plot of columns
# -- in this case, tailored to summarize completeness of data
table_column_upset <- function(data, pdf=NULL, width=NA, height=NA, nsets=NA, ...) {
  data <- as.data.frame(data)
  ncols <- dim(data)[2]
  if (ncols <= 0) { return() }
  for (i in 1:ncols) {

#test for boolean data and convert "true" to 1 and "false" to 0 if data is boolean 
#I don't know if SQL boolean data will come into R as logical or character.  Assuming logical.
#If it were the FALSE values that were important to know about, FALSE should be assigned 1 and TRUE 0 (no change for is.na).

if (is.logical(data[,i])) == TRUE {
	data[which(data[,i] == TRUE), i] <- 1
	data[which(data[,i] == FALSE, i] <- 0
	data[which(is.na(data[,i])),i] <- 0
}


#below=original code to convert numeric data to binary values
    data[which(!is.na(data[,i])),i] <- 1
    data[which(is.na(data[,i])),i] <- 0
  }
  if (is.na(width)) { width <- max(8, floor(ncols/5)) }
  if (is.na(height)) { height <- max(8, floor(ncols/2)) }
  if (is.na(nsets)) { nsets <- ncols }
  if (!is.null(pdf)) {
    pdf(file=pdf, width=width, height=height)
    upset(data, nsets=nsets, ...)
    dev.off()
  }
  else {
    upset(data, nsets=nsets, ...)
  }
}
