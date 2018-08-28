

operation_factory <- function(op){
  op_fun <- function(x){
    if (inherits(x, "RV")){
      return(Apply(x, op))
    } else
      return(op(x))
  }
  
  return(op_fun)
}

#' @export
Sqrt <- operation_factory(sqrt)
#' @export
Exp <- operation_factory(exp)
#' @export
Sin <- operation_factory(sin)
#' @export
Cos <- operation_factory(cos)
#' @export
Tan <- operation_factory(tan)
#' @export
Factorial <- operation_factory(factorial)
#' @export
Log <- function(x, base = exp(1)){
  if (inherits(x, "RVResults")){
    tryCatch(return(RVResults(log(x, base))),
             error = function(c) "I can't take the log of these values.")
  } else{
    print("Here")
    tryCatch(return(operation_factory(function(y) log(y, base))(x)),
             error = function(c) "I can't take the log of these values.")
  }
}
