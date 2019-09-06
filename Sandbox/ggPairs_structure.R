##### structure/blackbox of ggpairsTinkering script

> attributes(customPairs)
$names
 [1] "data"                "plots"               "title"               "xlab"                "ylab"                "showStrips"          "xAxisLabels"         "yAxisLabels"        
 [9] "showXAxisPlotLabels" "showYAxisPlotLabels" "labeller"            "switch"              "xProportions"        "yProportions"        "progress"            "legend"             
[17] "gg"                  "nrow"                "ncol"                "byrow"              

$class
[1] "gg"       "ggmatrix"

> customPairs$nrow
[1] 5
> customPairs$ncol
[1] 5
> nrow(customPairs)
NULL

> customPairs$plots
##### this section is preparing the plot for the upper left corner (distribution of sepal.length)
[[1]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8d778cb0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_densityDiag"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ..., rescale = FALSE) 
{
    mapping <- mapping_color_to_fill(mapping)
    p <- ggplot(data, mapping) + scale_y_continuous()
    if (identical(rescale, TRUE)) {
        p <- p + stat_density(aes(y = ..scaled.. * diff(range(x, 
            na.rm = TRUE)) + min(x, na.rm = TRUE)), position = "identity", 
            geom = "line", ...)
    }
    else {
        p <- p + geom_density(...)
    }
    p
}
<bytecode: 0x7f9e8d7747e0>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"

##### this section prepares the blank section in the second position of the top row
[[2]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8d7a0658>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Width`
* `y` -> `Sepal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### this section refers to the part of the matrix grid where I inserted the scatterplot with quantile lines
[[3]]
Smoothing formula not specified. Using: y ~ x


##### this is another blank section
[[4]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8e078f20>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Width`
* `y` -> `Sepal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"

##### this is the blank section from the top row where the boxplot would have gone
[[5]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8e9a6298>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Species`
* `y` -> `Sepal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### scatterplot second row first column
[[6]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8e9ce3b0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_points"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    p <- ggplot(data = data, mapping = mapping) + geom_point(...)
    p
}
<bytecode: 0x7f9e8e9c9a10>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Length`
* `y` -> `Sepal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### frequency for sepal.width
[[7]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ea01af0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_densityDiag"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ..., rescale = FALSE) 
{
    mapping <- mapping_color_to_fill(mapping)
    p <- ggplot(data, mapping) + scale_y_continuous()
    if (identical(rescale, TRUE)) {
        p <- p + stat_density(aes(y = ..scaled.. * diff(range(x, 
            na.rm = TRUE)) + min(x, na.rm = TRUE)), position = "identity", 
            geom = "line", ...)
    }
    else {
        p <- p + geom_density(...)
    }
    p
}
<bytecode: 0x7f9e8d7747e0>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### second row, third column, blank
[[8]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ea3aeb0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Length`
* `y` -> `Sepal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### inserted correlation matrix visualization
[[9]]


#####second row, last column, blank
[[10]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8eaa26f8>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Species`
* `y` -> `Sepal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### third row, first column, scatter
[[11]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8751c500>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_points"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    p <- ggplot(data = data, mapping = mapping) + geom_point(...)
    p
}
<bytecode: 0x7f9e8e9c9a10>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Length`
* `y` -> `Petal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### third row, second column, scatter
[[12]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8eacece0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_points"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    p <- ggplot(data = data, mapping = mapping) + geom_point(...)
    p
}
<bytecode: 0x7f9e8e9c9a10>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Width`
* `y` -> `Petal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


##### third row, center column, frequency dist
[[13]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8eb28ce0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_densityDiag"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ..., rescale = FALSE) 
{
    mapping <- mapping_color_to_fill(mapping)
    p <- ggplot(data, mapping) + scale_y_continuous()
    if (identical(rescale, TRUE)) {
        p <- p + stat_density(aes(y = ..scaled.. * diff(range(x, 
            na.rm = TRUE)) + min(x, na.rm = TRUE)), position = "identity", 
            geom = "line", ...)
    }
    else {
        p <- p + geom_density(...)
    }
    p
}
<bytecode: 0x7f9e8d7747e0>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"

#####third row, fourth column, blank
[[14]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8eb7aa08>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Width`
* `y` -> `Petal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####third row, fifth column, blank
[[15]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ec12260>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Species`
* `y` -> `Petal.Length`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####fourth row, first column
[[16]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ec59bb0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_points"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    p <- ggplot(data = data, mapping = mapping) + geom_point(...)
    p
}
<bytecode: 0x7f9e8e9c9a10>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Length`
* `y` -> `Petal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####fourth row, second column
[[17]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ed33230>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_points"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    p <- ggplot(data = data, mapping = mapping) + geom_point(...)
    p
}
<bytecode: 0x7f9e8e9c9a10>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Width`
* `y` -> `Petal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####fourth row, third column
[[18]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ed5c570>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_points"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    p <- ggplot(data = data, mapping = mapping) + geom_point(...)
    p
}
<bytecode: 0x7f9e8e9c9a10>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Length`
* `y` -> `Petal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####fourth row, fourth column
[[19]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8753e880>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_densityDiag"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ..., rescale = FALSE) 
{
    mapping <- mapping_color_to_fill(mapping)
    p <- ggplot(data, mapping) + scale_y_continuous()
    if (identical(rescale, TRUE)) {
        p <- p + stat_density(aes(y = ..scaled.. * diff(range(x, 
            na.rm = TRUE)) + min(x, na.rm = TRUE)), position = "identity", 
            geom = "line", ...)
    }
    else {
        p <- p + geom_density(...)
    }
    p
}
<bytecode: 0x7f9e8d7747e0>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####fourth row, fifth column, blank
[[20]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ed98a40>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_blank"
attr(,"params")
list()
attr(,"fn")
function (...) 
{
    aes(...)
    a <- data.frame(X = 1:2, Y = 1:2)
    p <- ggplot(data = a, aes_string(x = "X", y = "Y")) + geom_point(colour = "transparent") + 
        theme(axis.line = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), 
            legend.background = element_blank(), legend.key = element_blank(), 
            legend.text = element_blank(), legend.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_blank(), 
            strip.background = element_blank(), strip.text.x = element_blank(), 
            strip.text.y = element_blank())
    class(p) <- c(class(p), "ggmatrix_blank")
    p
}
<bytecode: 0x7f9e8d79ee70>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Species`
* `y` -> `Petal.Width`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####fifth row, first column, histogram by species
[[21]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8edeb540>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_facethist"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    mapping <- mapping_color_to_fill(mapping)
    horizontal <- is_horizontal(data, mapping)
    if (!horizontal) {
        mapping <- mapping_swap_x_y(mapping)
    }
    xVal <- mapping_string(mapping$x)
    yVal <- mapping_string(mapping$y)
    mapping$y <- NULL
    p <- ggplot(data = data, mapping)
    p <- p + stat_bin(...)
    if (horizontal) {
        p <- p + facet_grid(paste(yVal, " ~ .", sep = "")) + 
            theme(panel.spacing = unit(0.1, "lines"))
    }
    else {
        p <- p + facet_grid(paste(". ~", yVal, sep = "")) + theme(panel.spacing = unit(0.1, 
            "lines")) + coord_flip()
    }
    p <- p + labs(x = xVal, y = yVal)
    p
}
<bytecode: 0x7f9e8edd92d8>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Length`
* `y` -> `Species`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


#####fifth row, second column
[[22]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ee141f0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_facethist"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    mapping <- mapping_color_to_fill(mapping)
    horizontal <- is_horizontal(data, mapping)
    if (!horizontal) {
        mapping <- mapping_swap_x_y(mapping)
    }
    xVal <- mapping_string(mapping$x)
    yVal <- mapping_string(mapping$y)
    mapping$y <- NULL
    p <- ggplot(data = data, mapping)
    p <- p + stat_bin(...)
    if (horizontal) {
        p <- p + facet_grid(paste(yVal, " ~ .", sep = "")) + 
            theme(panel.spacing = unit(0.1, "lines"))
    }
    else {
        p <- p + facet_grid(paste(". ~", yVal, sep = "")) + theme(panel.spacing = unit(0.1, 
            "lines")) + coord_flip()
    }
    p <- p + labs(x = xVal, y = yVal)
    p
}
<bytecode: 0x7f9e8edd92d8>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Sepal.Width`
* `y` -> `Species`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"

#####fifth row, third column
[[23]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8ee75ee0>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_facethist"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    mapping <- mapping_color_to_fill(mapping)
    horizontal <- is_horizontal(data, mapping)
    if (!horizontal) {
        mapping <- mapping_swap_x_y(mapping)
    }
    xVal <- mapping_string(mapping$x)
    yVal <- mapping_string(mapping$y)
    mapping$y <- NULL
    p <- ggplot(data = data, mapping)
    p <- p + stat_bin(...)
    if (horizontal) {
        p <- p + facet_grid(paste(yVal, " ~ .", sep = "")) + 
            theme(panel.spacing = unit(0.1, "lines"))
    }
    else {
        p <- p + facet_grid(paste(". ~", yVal, sep = "")) + theme(panel.spacing = unit(0.1, 
            "lines")) + coord_flip()
    }
    p <- p + labs(x = xVal, y = yVal)
    p
}
<bytecode: 0x7f9e8edd92d8>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Length`
* `y` -> `Species`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


####fifth row, fourth column
[[24]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8eeac298>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_facethist"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ...) 
{
    mapping <- mapping_color_to_fill(mapping)
    horizontal <- is_horizontal(data, mapping)
    if (!horizontal) {
        mapping <- mapping_swap_x_y(mapping)
    }
    xVal <- mapping_string(mapping$x)
    yVal <- mapping_string(mapping$y)
    mapping$y <- NULL
    p <- ggplot(data = data, mapping)
    p <- p + stat_bin(...)
    if (horizontal) {
        p <- p + facet_grid(paste(yVal, " ~ .", sep = "")) + 
            theme(panel.spacing = unit(0.1, "lines"))
    }
    else {
        p <- p + facet_grid(paste(". ~", yVal, sep = "")) + theme(panel.spacing = unit(0.1, 
            "lines")) + coord_flip()
    }
    p <- p + labs(x = xVal, y = yVal)
    p
}
<bytecode: 0x7f9e8edd92d8>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Petal.Width`
* `y` -> `Species`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"


####bottom right corner ----bar graph
[[25]]
$fn
function (data, mapping, ...) 
{
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
}
<bytecode: 0x7f9e8de203b8>
<environment: 0x7f9e8eeec340>
attr(,"class")
[1] "ggmatrix_fn_with_params"
attr(,"name")
[1] "ggally_barDiag"
attr(,"params")
list()
attr(,"fn")
function (data, mapping, ..., rescale = FALSE) 
{
    mapping <- mapping_color_to_fill(mapping)
    mapping$y <- NULL
    x_data <- eval_data_col(data, mapping$x)
    numer <- ("continuous" == plotting_data_type(x_data))
    p <- ggplot(data = data, mapping)
    if (is_date(x_data)) {
        p <- p + geom_histogram(...)
    }
    else if (numer) {
        if (identical(rescale, TRUE)) {
            p <- p + geom_histogram(aes(y = ..density../max(..density..) * 
                diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE)), 
                ...) + coord_cartesian(ylim = range(eval_data_col(data, 
                mapping$x), na.rm = TRUE))
        }
        else {
            p <- p + geom_histogram(...)
        }
    }
    else {
        p <- p + geom_bar(...)
    }
    p
}
<bytecode: 0x7f9e8eee80a0>
<environment: namespace:GGally>

$mapping
Aesthetic mapping: 
* `x` -> `Species`

$dataPos
[1] 1

$gg
NULL

attr(,"class")
[1] "ggmatrix_plot_obj"