
#library(plyr)

i <- 0

is_discrete <- function(heights){
  return(sum(heights > 1) > 0.8 * length(heights))
}

# This is not exactly like in Python version
# It takes random sample of the color cycle
get_next_color <- function(){
  i <<- i + 1
  if (i > 5)
    i <<- 1

  color <- palette()[i]
  return(t(col2rgb(color)) / 255)
}

vlines <- function(x, y, color, alpha, normalize, add){
  if (normalize){
    ymax <- round(ceiling(max(y) * 100) / 100, 2)
  } else
    ymax <- plyr::round_any(max(y), 10, f = ceiling)

  if (!add){
    frame()
    plot.window(xlim=c(min(x), max(x)),
                ylim=c(0, ymax))
    # (ymax + (ymax / length(x)) / 2)
    axis(1, at=round(x))

    axis(2, at=signif(round(seq(0, ymax, length.out = 10), 2), 2)
         , las = 1)

    box()
  }

  #print("right here")
  #print(x)
  for (i in 1:length(x)){
    segments(x[i], 0, x[i], y[i], col = rgb(color, alpha = alpha))
  }

  #print("after")
}
