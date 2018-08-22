
InfiniteSequence <- function(fun){
  me <- list(
    fun = fun
  )
  
  class(me) <- append(me, "InfiniteSequence")
  return(me)
}

getitem <- function(self, n){
  UseMethod("getitem")
}

getitem.InfiniteSequence <- function(self, n){
  if (n == as.integer(n)){
    return(self$fun(n))
  } else{
    stop("Index to a sequence must be an integer.")
  }
}