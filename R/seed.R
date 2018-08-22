
seed = as.integer(Sys.time())

get_seed <- function(){
  seed <<- seed + 1
  
  if (seed > .Machine$integer.max) seed = 0
  return(seed)
}