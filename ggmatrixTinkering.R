require(ggplot2)
require(GGally)
require(ggcorrplot)

### first plot
plot1 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point() + geom_quantile()

### second plot
plot2 <- ggcorrplot(cor(iris[,1:4]), p.mat = cor_pmat(iris[,1:4]), hc.order = TRUE, type= "lower", lab = TRUE, insig = "blank")

### third plot
plot3 <- ggplot(iris, aes(x=Sepal.Length)) + geom_histogram(aes(color=Species),fill="white",position="dodge")

### fourth plot
plot4 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point(aes(color=Species, shape=Species)) + geom_smooth(aes(color=Species, shape=Species), method=lm, se=FALSE, fullRange=TRUE)

### put plots in a list
plotList <- list(plot1, plot2, plot3, plot4)

### make matrix of plots from list
plotMatrix <- ggmatrix(
  plotList,
  2, 2,
  xAxisLabels = NULL, yAxisLabels = NULL,
  byrow = TRUE,
  showStrips = TRUE,
  showAxisPlotLabels = TRUE, showXAxisPlotLabels = TRUE, showYAxisPlotLabels = TRUE,
  labeller = NULL, switch = NULL
)

### call matrix to display
plotMatrix
