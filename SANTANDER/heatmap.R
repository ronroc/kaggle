install.packages("d3heatmap"); 

require(d3heatmap)

d3heatmap(mtcars, scale = "column", colors = "Spectral")
