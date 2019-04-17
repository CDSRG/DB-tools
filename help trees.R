# if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
# BiocManager::install("HELP", version = "3.8")

require("HELP")

data(iris)
plotPairs(as.matrix(iris[,1:4]))

data(mtcars)
plotPairs(as.matrix(mtcars))
