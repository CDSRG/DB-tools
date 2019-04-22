### https://www.r-bloggers.com/ggplot-your-missing-data-2/
### see also https://www.r-exercises.com/2017/11/29/spatial-data-analysis-introduction-to-raster-processing-part-1/

if(!("reshape2" %in% installed.packages()[,1]))
  install.packages(c("reshape2"), dependencies = TRUE)

if(!("ggplot2" %in% installed.packages()[,1]))
  install.packages(c("ggplot2"), dependencies = TRUE)

require(reshape2)

require(ggplot2)


missingDB <- function(x, ...){  

### below is a more controlled version
### MUST PASS TO IT: 
	### table as data frame
	### vector of column names that are identifying variables for the table
	### vector of column names that are measured variables for the table 
### x <- melt.data.frame(x, id.vars = c(VECTOR OF ID VARIABLES), measure.vars = c(VECTOR OF MEASURED VARIABLES), na.rm = FALSE, value.name = "value", variable.name = "variable", factorsAsStrings = TRUE)

x <- melt(is.na(DATA FRAME), na.rm = FALSE, value.name = "value")

### could pop the melting bit into the ggplot call with x

ggplot(x,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() +  
 theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
   labs(x = "Variables in Dataset",
         y = "Rows / observations")
}



############### tidy version
missingDBviz <- function(x, ...){ 
	ggplot(melt(is.na(results), na.rm = FALSE, value.name = "value"),
	aes(x = Var2, y = Var1)) +
	geom_raster(aes(fill = value)) +
	scale_fill_grey(name = "", labels = c("Present","Missing")) +
	theme_minimal() + 
	labs(x = "Variables in Dataset", y = "Rows / observations")
}