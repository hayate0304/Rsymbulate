

is_discrete <- function(heights){
  return(sum(heights > 1) > 0.8 * length(heights))
}

# This is not exactly like in Python version
# It takes random sample of the color cycle
#' @export
get_next_color <- function(){
  index <- 3L

  #print(index)
  function(){
    #print(index)
    index <<- index + 1
    #print(index)
    if (index > 5)
      index <<- 1
    return(palette()[index])
  }

}

