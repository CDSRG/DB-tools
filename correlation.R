#data from Principles of Biostatistics, page 407 (table 17.3)
#data represents percentage of births attended by trained health care personnel and maternal mortality rates for 20 countries

#create vectors to use in analyses
attendedBirth <- c(5,100,98,84,100,99,70,50,26,6,100,37,35,96,55,90,96,99,99,95)
maternalMortality <- c(600,3,67,170,6,15,120,170,300,830,10,800,500,60,100,10,5,5,8,120)

#create data frame
birthMortality <- data.frame(attendedBirth, maternalMortality)

#later: add functionality to accept input other than these two vectors

#provide summary stats

#calculate means
#later: add variables to control parameters
meanBirths <- mean(attendedBirth,weights=NULL,trim=0,na.rm=TRUE)
meanMortality <- mean(maternalMortality,weights=NULL,trim=0,na.rm=TRUE)

#calculate medians
#later: add variables to control parameters
medianBirths <- median(attendedBirth,weights=NULL,trim=0,na.rm=TRUE)
medianMortality <- median(maternalMortality,weights=NULL,trim=0,na.rm=TRUE)

#boxplot
#later: add parameters for labels! and and more
#In this instance (the hardcoded dataset), it doesn't make sense 
#to do the boxplots for both vectors in one image.  However, it might
#make sense in other cases.
#later: add functionality to control how many sets of data might be
#plotted together or separately (assume large dataset comes in threough
#a SQL connection in a table with many rows and columns -- some columns may
#be better suited to considering together
#later: also provide values for ranges and quantiles
boxplot(attendedBirth)
boxplot(maternalMortality)
boxplot(birthMortality)

#simple scatter plot
#can provide two vectors or 2-column vector frame
#later:add functionality to handle other input and give parameters
#later:add best fit line
plot(attendedBirth,maternalMortality)

#better scatter plot
#if more than two columns, will provide a matrix of scatters
require(lattice)
require(ggplot2)
pairs(birthMortality)

#another better scatter plot uses ggcorplot (see file)

#correlation matrix
#for more info, see http://www.sthda.com/english/wiki/ggally-r-package-extension-to-ggplot2-for-correlation-matrix-and-survival-plots-r-software-and-data-visualization
install.packages("GGally")
library("GGally")
ggcorr(birthMortality)
#can customize with parameters
#obviously better for displaying multiple comparisons than just the one coded here
ggpairs(birthMortality)
#above is very nice set of graphs with scatter, line, correlation
#use ggsurv() for Kaplan-Meier survival curves

#correlation by pearson
pearsonCorr <- cor.test(x=attendedBirth, y=maternalMortality, method = 'pearson')
pearsonT <- pearsonCorr$statistic
pearsonP <- pearsonCorr$p.value
pearsonCI <- pearsonCorr$conf.int

#correlation by spearman
#later: add methods for user to choose correlation method?
spearmanCorr <- cor.test(x=attendedBirth, y=maternalMortality, method = 'spearman')
spearmanT <- spearmanCorr$statistic
spearmanP <- spearmanCorr$p.value

#add assessment of missingness
#add assessment of outliers/unusual values
#eventually format to give all (or desired part) info in single
#document of descriptive stats about given table/data



