#Tinkering with ggpairs

library(ggplot2)
library(GGally)
library(ggcorrplot)

data(iris)

#compute a correlation matrix
corr <- cor(iris[,1:4])
#compute a correlation p-values matrix
p.mat <- cor_pmat(iris[,1:4])

#attempt to put a user-defined function into ggpairs

#develop function to insert
#great assistance from the examples in ggcorrplot documentation

#visualize the correlation matrix
ggcorrplot(corr)
#reorder the matrix
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
#take just lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "white")
#add correlation coefficients and pvalues, leaving blank if not significant
ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE, type= "lower", lab = TRUE, insig = "blank")


#prepare ggpairs framework to take the user-defined function
customPairs <- ggpairs(iris, upper = "blank")
#note: "blank" removed the categorical graphs at the edge too
#maybe we can get them back in later?
#assign user-designed function to variable
customPlot <- ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE, type= "lower", lab = TRUE, insig = "blank")
#place plot into ggpairs matrix
customPairs[2,4] <- customPlot
#call
customPairs


