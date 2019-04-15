#Tinkering with ggpairs

library(ggplot2)
library(GGally)

data(iris)

#one example of basic use of ggpairs()

ggpairs(iris, upper = list(continuous = "points", combo = "box",discrete = "facetbar", na = "na"), 
lower = list(continuous = "cor", combo = "facethist", discrete = "facetbar", na = "na"), 
diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"))



#attempt to put a user-designed function into ggpairs

testPlot <- function(data, mapping, ...){
	ggplot(iris, aes(x,y)) + geom_point(size=2,shape=23)
	}

ggpairs(iris, upper = list(continuous = testPlot, combo = "box",discrete = "facetbar", na = "na"), 
lower = list(continuous = "cor", combo = "facethist", discrete = "facetbar", na = "na"), 
diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"))

#This works, in that it puts the "testPlot" graphs into the pairs matrix, but all the graphs are the same.
#It isn't calculating a new graph for each pair.
#Somehow, I must need to tell it to do all the pairs.  Loop?  Something more efficient?