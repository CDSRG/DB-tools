###	exploreDB
###	The purpose of this tool is to create a thorough overview that describes a dataset 
###	(including assessments of missingness, robustness, association, distribution, and dispersal), 
###	intended to be conducted upon receiving a dataset in preparation for meaningful deep analysis.

###################################################################

###	Ensure needed packages are installed (if not, install them).

if(!("RODBC" %in% installed.packages()[,1]))
  install.packages(c("RODBC"), dependencies = TRUE)

if(!("reshape2" %in% installed.packages()[,1]))
  install.packages(c("reshape2"), dependencies = TRUE)

if(!("ggplot2" %in% installed.packages()[,1]))
  install.packages(c("ggplot2"), dependencies = TRUE)

if(!("UpSetR" %in% installed.packages()[,1]))
  install.packages(c("UpSetR"), dependencies = TRUE)

if(!("GGally" %in% installed.packages()[,1]))
  install.packages(c("GGally"), dependencies = TRUE)

if(!("ggcorrplot" %in% installed.packages()[,1]))
  install.packages(c("ggcorrplot"), dependencies = TRUE)

###	Load needed packages into workspace.

require(RODBC)
require(reshape2)
require(ggplot2)
require(UpSetR)
require(GGally)
require(ggcorrplot)

###################################################################

###	Establish variables for connecting to and querying a database.

connectString <- ""
queryString <- ""

###	Make connection and retrieve query results.

con <- odbcDriverConnect(connection = connectString)
results <- as.data.frame(sqlQuery(con, queryString)

###################################################################

###	Create missingness visualization.  See missingness.R file for more information.

missingDBviz <- function(x, ...){ 
	ggplot(melt(is.na(results), na.rm = FALSE, value.name = "value"),
	aes(x = Var2, y = Var1)) +
	geom_raster(aes(fill = value)) +
	scale_fill_grey(name = "", labels = c("Present","Missing")) +
	theme_minimal() + 
	labs(x = "Variables in Dataset", y = "Rows / observations")
}

###################################################################

###	Create missingness/robustness visualization.  See upset.R file for more information.

upsetDBviz <- function(data, pdf=NULL, width=NA, height=NA, nsets=NA, mode=NULL, use.columns=NULL, ...) {
	ncols <- dim(data)[2]
	if (ncols <= 0) { 
		warning("cannot visualize upset plot for empty data")
		return()
	}
	if (is.null(use.columns)) {
		cols <- 1:ncols
	}
	else {
		cols <- use.columns	
	}
	mode <- match.arg(mode, choices=c("presence"))
	if (mode == "presence") {
		for (i in cols) {
			data[,i] <- as.numeric(data[,i])
			na.data <- is.na(data[,i])
			data[which(!na.data), i] <- 1
			data[which(na.data), i] <- 0
		}
	}
	if (is.na(width)) { width <- max(8, floor(ncols/5)) }
	if (is.na(height)) { height <- max(8, floor(ncols/2)) }
	if (is.na(nsets)) { nsets <- ncols }
	if (!is.null(pdf)) {
    	pdf(file=pdf, width=width, height=height)
		upset(data[,cols], nsets=nsets, ...)
 		dev.off()
	}
	else {
		upset(data[,cols], nsets=nsets, ...)
	}
}

###################################################################

###	Create correlation visualization.  See ggpairsTinkering.R file for more information.

ggpairs(results)

# This is the most basic version with no customization.