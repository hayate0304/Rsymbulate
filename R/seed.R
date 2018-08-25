

#' @export
get_seed <- function(){
  seed <- as.integer(Sys.time())

  function(){
    seed <<- seed + 1
    if (seed > .Machine$integer.max) seed <<- 0
    return(seed)
  }
}
