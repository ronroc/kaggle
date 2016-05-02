doPlots <- function(data.in, fun, ii, ncol=3) {

    pp <- list()

    for (i in ii) {

    p <- fun(data.in=data.in, i=i)
    
    pp <- c(pp, list(p))
  }
  
    do.call("grid.arrange", c(pp, ncol=ncol))

    }

# plot histogram of categorical variables----------------------------------------------------------------

require(gridExtra)

require(ggplot2)

plotHist <- function(data.in, i) {
  
  data <- data.frame(x=data.in[,i])
  
  p <- ggplot(data=data, aes(x=factor(x))) + 
    
       geom_histogram() + 
    
       xlab(colnames(data.in)[i]) + 
    
       theme_light() + 
    
       theme(axis.text.x=element_text(size=8))
  
  return (p)

  }


# densities of continous functions --------------------------------------------------------------------

plotDensity <- function(data.in, i) {
  
  data <- data.frame(x=data.in[,i], Response=data.in$Response)
  
  p <- ggplot(data) + #geom_density(aes(x=x, colour=factor(Response))) + 
    
       geom_line(aes(x=x), stat="density", size=1, alpha=1.0) +
    
       xlab(colnames(data.in)[i]) + 
    
       theme_light()
  
  return (p)
}


# box plots of continous features depending on response-----------------------------------------------

plotBox <- function(data.in, i) {

    data <- data.frame(y=data.in[,i], Response=data.in$Response)
  
    p <- ggplot(data, aes(x=factor(Response), y=y)) + 
      
         geom_boxplot() + 
      
         ylab(colnames(data.in)[i]) + 
      
         theme_light()
  
    return (p)
}