##DEFINE GENERIC FUNCTION plotPairs()
setGeneric("plotPairs", 
	function(x, ...) {
		standardGeneric("plotPairs")
	}
)

##DEFINE METHOD TO HANDLE CLASS: "missing"
setMethod("plotPairs", 
	signature=c("missing"), 
	function(x, ...) {
		stop("argument 'x' is missing with no default")
	}
)

##DEFINE METHOD TO HANDLE CLASS: "ExpressionSet"
setMethod("plotPairs", 
	signature=c("ExpressionSet"), 
	function (x, element="exprs", ...) {
		if (!validObject(x)) {
			stop("argument 'x' not a valid ExpressionSet object")
		}
		callGeneric(assayDataElement(x, element), ...)
	}
)

##DEFINE MAIN plotPairs() METHOD TO HANDLE CLASS: "matrix"
setMethod("plotPairs", 
	signature=c("matrix"), 
	function(x, samples=NULL, scale=TRUE, groups=TRUE, dist.method="euclidean", hclust.method="ward.D", k=NULL, cor.method="pearson", ...) {
		par.initial <- par()
		x <- getSamples(as.matrix(x), samples, ...)
		N <- dim(x)[2]
		if (N < 2) {
			stop("need to compare at least 2 samples at a time")
		}
		data.cluster <- hclust(dist(t(x), method=dist.method, ...), method=hclust.method)
		data.order <- data.cluster$order
		data.range <- range(data.cluster$height)
		data.cutoff <- floor(mean(data.cluster$height))
		data.groups <- cutree(data.cluster, h=data.cutoff, k=k)
		group.colors <- rainbow(n=max(data.groups), start=0, end=2/3)
		####################################################
		## DEFINE HELPER FUNCTION: plot.pairs.upper()
		plot.pairs.upper <- function(x=NULL, y=NULL) {
			usr <- par()$usr
			data.points <- sample(1:length(x), size=1000, replace=TRUE)
			data.points <- data.points[order(data.points)]
			points(x[data.points], y[data.points], type="p", pch=20, cex=0.2, col="blue")
			xy.cor <- ceiling(10000*cor(x, y, method=cor.method))/10000
			l <- lowess(x, y)
			lines(l$x[data.points], l$y[data.points], col="red")
			text(usr[1], 0.9*(usr[4]-usr[3])+usr[3], labels=paste(" R=", xy.cor, sep=""), cex=1.2, adj=0)
		}
		####################################################
		layout(matrix(data=1, nrow=1, ncol=1))
		par(pin=rep(N*1.5,2))
		pairs(x[, data.order], lower.panel=NULL, upper.panel=plot.pairs.upper, ...)
		fin <- par()$fin
		layout(matrix(data=1, nrow=1, ncol=1))
		par(mfg=c(1, 1), mar=c(0, 0, 0, 0), mgp=c(1, 0, 0), pin=c(fin[1]-1.05, fin[2]-1.05))
		if (groups) {
			xlab <- paste(N, " samples, ", max(data.groups), " groups", sep="")
		}
		else {
			xlab <- paste(N, " samples", sep="")
		}
		if (scale) {
			ylab <- "Distance"
		}
		else {
			ylab <- ""	
		}
		plot(c(1:N), c(1:N), axes=FALSE, xlab=xlab, ylab=ylab, cex=0)
		usr <- par()$usr
		x.adjust <- 0.5*(usr[2]-usr[1])/N
		y.adjust <- 0.5*(usr[4]-usr[3])/N
		leafs.x <- x.adjust+x.adjust*2*(1:N-1)+usr[1]
		leafs.y <- usr[4]-y.adjust-y.adjust*2*(1:N-1)
		tree.root <- c(1, 1)
		m <- (leafs.y-tree.root[2])/(leafs.x-tree.root[1])
		b <- tree.root[2]-m*tree.root[1]
		dist.x <- sqrt(x.adjust**2+(leafs.y-m*(leafs.x-x.adjust)-b)**2)
		dist.y <- sqrt((leafs.x-(leafs.y-y.adjust-b)/m)**2+y.adjust**2)
		leafs <- matrix(nrow=length(dist.x), ncol=2)
		for (i in 1:length(dist.x)) {
			if (dist.x[i]<dist.y[i]) {
				leafs[i, 1] <- (leafs.x-x.adjust)[i]
				leafs[i, 2] <- (m*(leafs.x-x.adjust)+b)[i]
			}
			else {
				leafs[i, 1] <- ((leafs.y-y.adjust-b)/m)[i]
				leafs[i, 2] <- (leafs.y-y.adjust)[i]
			}
		}
		for (i in 1:length(data.order)) {
			if (groups) {
				group <- data.groups[data.order[i]]
				points(leafs[i, 1], leafs[i, 2],cex=2, pch=22, col=group.colors[group], bg=group.colors[group])
			}
			points(leafs[i, 1], leafs[i, 2], cex=1, pch=19)
			x.new <- ((usr[4]-2*y.adjust) - b[i])/(m[i]+1)
			y.new <- (usr[4]-2*y.adjust)-x.new
			points(c(x.new, leafs[i, 1]), c(y.new, leafs[i, 2]), type="l", lty="dashed")
			leafs[i, 1] <- x.new
			leafs[i, 2] <- y.new
		}
		root.b <- tree.root[1]+tree.root[2]
		if (scale) {
			for (i in 0:(N-1)) {
				abline(b=-1, a=i*(usr[4]-2*y.adjust-root.b)/(N-1)+root.b, lty="dotted", col="gray", lwd=2)
				text(usr[1], i*(usr[4]-2*y.adjust-root.b)/(N-1)+root.b-usr[1], label=ceiling(data.range[2]-i*(data.range[2]-data.range[1])/(N-1)), adj=0, cex=1, col="gray")
				abline(b=-1, a=i*(usr[4]-2*y.adjust-root.b)/(N-1)+root.b, lty="dotted", col="gray", lwd=2)
			}
			if (data.range[2]-data.range[1] != 0) {
				i <- (data.range[2]-data.cutoff)/(data.range[2]-data.range[1])
			}
			else {
				i <- 0	
			}
			if (is.null(k)) {
				abline(b=-1, a=i*(usr[4]-2*y.adjust-root.b)+root.b, lty="dotted", col="red", lwd=2)
				text(usr[1], i*(usr[4]-2*y.adjust-root.b)+root.b-usr[1], label=data.cutoff, adj=0, cex=1, col="red")
			}
		}
		####################################################
		## DEFINE HELPER FUNCTION: join.leafs()
		join.leafs <- function(leafs=NULL, root=NULL, scale=NULL) {
			x <- y <- 0
			num.leafs <- dim(leafs)[1]
			for (i in 1:num.leafs) {
				x <- x+leafs[i, 1]-root[1]
				y <- y+leafs[i, 2]-root[2]
			}
			x <- x/num.leafs-0.5*(num.leafs-2)
			y <- y/num.leafs-0.5*(num.leafs-2)
			if (is.null(scale)) {
				scale <- 1/num.leafs
			}
			x <- scale*x+root[1]
			y <- scale*y+root[2]
			list(x=x, y=y)
		}
		####################################################
		
		####################################################
		## DEFINE HELPER FUNCTION: draw.tree()
		draw.tree <- function(tree=NULL, root=tree.root, range=NULL) {
			if (is.null(tree) | length(tree)<1) {
				return()	
			}
			height.prev <- attr(tree, which="height")
			if (is.null(range)) {
				range <- c(0, height.prev)	
			}
			is.group <- (all(data.groups[unlist(tree)] == mean(data.groups[unlist(tree)])) & groups)
			if (length(tree) == 1) {
				leaf.xy <- leafs[which(data.order == tree[1]), ]
				if (is.group) {
					points(c(leaf.xy[1], root[1]), c(leaf.xy[2], root[2]), type="l", col=group.colors[data.groups[tree[1]]], lwd=2)
				}
				points(c(leaf.xy[1], root[1]), c(leaf.xy[2], root[2]), type="l", lwd=1)
				return()
			}
			if (!scale) {
				branches <- which(data.order %in% unlist(tree))
				leaf.xy <- join.leafs(leafs=as.matrix(cbind(c(leafs[branches, 1]), c(leafs[branches, 2]))), root=root)
			}
			for (i in 1:length(tree)) {
				sub.tree <- tree[[i]]
				height <- attr(sub.tree, which="height")
				distance <- ((usr[4]-2*y.adjust-root.b))*(height.prev-max(range[1], height))/(range[2]-range[1])
				branches <- which(data.order %in% unlist(sub.tree))
				m1 <- mean(leafs[branches, 2]-tree.root[2])/mean(leafs[branches, 1]-tree.root[1])
				b1 <- mean(leafs[branches, 2]) - m1*mean(leafs[branches, 1])
				m2 <- -1
				b2 <- root[2] - m2*root[1]
				x.new <- (b2 - b1)/(m1-m2)
				y.new <- m1*x.new+b1
				b2 <- b2+distance
				root.x.new <- (b2 - b1)/(m1-m2)
				root.y.new <- m1*root.x.new+b1
				if (scale) {
					if (is.group) {
						points(c(x.new, root.x.new), c(y.new, root.y.new), type="l", col=group.colors[data.groups[unlist(tree)[i]]], lwd=2)
						points(c(x.new, root[1]), c(y.new, root[2]), type="l", col=group.colors[data.groups[unlist(tree)[i]]], lwd=2)
					}
					points(c(x.new, root[1]), c(y.new, root[2]), type="l")
					points(c(x.new, root.x.new), c(y.new, root.y.new), type="l")
					draw.tree(tree=sub.tree, root=c(root.x.new, root.y.new), range=range)
				}
				else {
					if (is.group) {
						points(c(leaf.xy$x, root[1]), c(leaf.xy$y, root[2]), type="l", col=group.colors[data.groups[unlist(tree)[i]]], lwd=2)
					}
					points(c(leaf.xy$x, root[1]), c(leaf.xy$y, root[2]), type="l", lwd=1)
					draw.tree(tree=sub.tree, root=c(leaf.xy$x, leaf.xy$y), range=range)
				}
			}
		}
		####################################################
		
		draw.tree(tree=as.dendrogram(data.cluster), range=data.range)
		par(par.initial[which(!names(par()) %in% c("cin", "cra", "csi", "cxy", "din", "gamma", "page"))])
	}
)
