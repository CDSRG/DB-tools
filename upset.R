# DEFINE table_column_upset() FUNCTION to visualize upset plot of columns
# -- in this case, tailored to summarize completeness of data
table_column_upset <- function(data, pdf=NULL, width=NA, height=NA, nsets=NA,
...) {
data <- as.data.frame(data)
ncols <- dim(data)[2]
if (ncols <= 0) { return() }
for (i in 1:ncols) {
data[,i] <- as.numeric(data[,i])
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
