
library(plyr)

i <- 0

is_discrete <- function(heights){
  return(sum(heights > 1) > 0.8 * length(heights))
}

# This is not exactly like in Python version 
# It takes random sample of the color cycle
get_next_color <- function(){
  i <<- i + 1
  if (i > 4) i = 1
  
  color <- palette()[i]
  return(t(col2rgb(color)) / 255)
}

vlines <- function(x, y, color, alpha, normalize, add){
  if (normalize){
    ymax <- round(max(y), 1)
  } else 
    ymax <- round_any(max(y), 10, f = ceiling)
  
  if (!add){
    frame()
    plot.window(xlim=c(min(x), max(x)), 
                ylim=c(min(y), (ymax + (ymax / length(x)) / 2)))
    axis(1, at=round(x))
    axis(2, at=seq(0, ymax, 
                   by = signif(ymax, 1) / length(x)))
    box()
  }

  print("right here")
  print(x)
  for (i in 1:length(x)){
    segments(x[i], 0, x[i], y[i], col = rgb(color, alpha = alpha))
  }
  
  print("after")
}
