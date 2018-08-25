
is_scalar <- function(x) is.numeric(x) && length(x) == 1L

is_vector <- function(x) is.numeric(x) && identical(dim(x), NULL) && length(x) != 1L

get_dimesion <- function(x){
  if (length(dim(x$results)) == 0){
    return(0L)
  } else
    return(dim(x$results)[2])
}
