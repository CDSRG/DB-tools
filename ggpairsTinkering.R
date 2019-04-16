#Tinkering with ggpairs

library(ggplot2)
library(GGally)

data(iris)

#one example of basic use of ggpairs()

ggpairs(iris, upper = list(continuous = "points", combo = "box",discrete = "facetbar", na = "na"), 
lower = list(continuous = "cor", combo = "facethist", discrete = "facetbar", na = "na"), 
diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"))



#attempt to put a user-designed function into ggpairs

#develop function to insert
#great assistance from the examples in ggcorrplot documentation
library(ggcorrplot)
#compute a correlation matrix
corr <- cor(iris[,1:4])
#compute a correlation p-values matrix
p.mat <- cor_pmat(iris[,1:4])
#visualize the correlation matrix
ggcorrplot(corr)
#reorder the matrix
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
#take just lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "white")
#add correlation coefficients and pvalues, leaving blank if not significant
ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE, type= "lower", lab = TRUE, insig = "blank")
