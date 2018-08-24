
is_scalar <- function(x) is.numeric(x) && length(x) == 1L

is_vector <- function(x) is.numeric(x) && identical(dim(x), NULL) && length(x) != 1L

get_dimesion <- function(x){
  return(length(dim(x$results)))
}
