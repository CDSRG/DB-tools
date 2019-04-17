require(ggplot2)
require(GGally)
require(ggcorrplot)

### first plot
plot1 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point() + geom_quantile()

### legend for first plot
plotLegend1 <- ggally_text("Scatterplot with quantile lines")

### second plot
plot2 <- ggcorrplot(cor(iris[,1:4]), p.mat = cor_pmat(iris[,1:4]), hc.order = TRUE, type= "lower", lab = TRUE, insig = "blank")

### legend for second plot
plotLegend2 <- ggally_text("Correlation matrix with p-values")

### third plot
plot3 <- ggplot(iris, aes(x=Sepal.Length)) + geom_histogram(aes(color=Species),fill="white",position="dodge")

### legend for third plot
plotLegend3 <- ggally_text("Histogram")

### fourth plot
plot4 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point(aes(color=Species, shape=Species)) + geom_smooth(aes(color=Species, shape=Species), method=lm, se=FALSE, fullRange=TRUE)

### legend for fourth plot
plotLegend4 <- ggally_text("Scatterplot by group")

### put plots in a list
plotList <- list(plot1, plot2, plotLegend1, plotLegend2, plot3, plot4, plotLegend3, plotLegend4)

### make matrix of plots from list
plotMatrix <- ggmatrix(
  plotList,
  4, 2,
  xAxisLabels = NULL, yAxisLabels = NULL,
  byrow = TRUE,
  showStrips = TRUE,
  showAxisPlotLabels = FALSE, showXAxisPlotLabels = FALSE, showYAxisPlotLabels = FALSE,
  title="test", xlab="x label", ylab="y label"
)

### call matrix to display
plotMatrix
