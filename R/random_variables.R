

#' Defines a random variable.
#' 
#'  A random variable is a function which maps an outcome of
#'  a probability space to a number.  Simulating a random
#'  variable is a two-step process: first, a draw is taken
#'  from the underlying probability space; then, the function
#'  is applied to that draw to obtain the realized value of
#'  the random variable.
#'  
#' @param probSpace (ProbabilitySpace): the underlying
#' probability space of the random variable.
#' @param fun (function, optional):  a function that maps draws from the 
#' probability space to numbers.
#' 
#' @examples
#' # a single draw is a sequence of 0s and 1s, e.g., (0, 0, 1, 0, 1).
#' P = BoxModel(c(0, 1), size=5)
#' # X counts the number of 1s in the draw, e.g., 5
#' X = RV(P, sum)
#' 
#' # the function is the identity, so Y has a Normal(0, 1) distribution
#' Y = RV(Normal(0, 1)
#' 
#' # a single draw from BivariateNormal is a tuple of two numbers
#' P = BivariateNormal()
#' # Z is the smaller of the two numbers
#' Z = RV(P, min)
#' 
#' @return A RV
#' @export
RV <- function(probSpace, fun = function(x) x){
  me <- list(probSpace = probSpace,
             fun = fun)

  class(me) <- append(class(me), "RV")
  return(me)
}

#' A function that takes no arguments and returns a single
#'   realization of the random variable.
#'
#' @examples
#'   X = RV(Normal(0, 1))
#'   X %>% draw() might return -0.9, for example.
#' @export
draw.RV <- function(self){
  return(self$fun(draw(self$probSpace)))
}
  
#'   Simulate n draws from probability space described by the random
#'   variable.
#'
#' @param n (int): How many draws to make.
#' @return Results: A vector or matrix containing the simulation results.
#' @export
sim.RV <- function(self, n){

  if (length(draw(self)) == 1){
    return(RVResults((replicate(n, draw(self)))))
  } else
    return(RVResults(t(replicate(n, draw(self)))))
}

#' @export
call <- function(self, input) UseMethod("call")
#' @export
call.default <- function(self, input) stop("Could not perform the function")

#' @export
call.RV <- function(self, input){
  cat("Warning: Calling an RV as a function simply applies the function that defines
the RV to the input, regardless of whether the input is a valid outcome in
the underlying probability space.\n")

  dummy_draw = draw(self$probSpace)

  # paste("Dummy: ", dummy_draw)
  # R doesn't have scalar type
  if (is.atomic(input)){
    if (!(is.atomic(dummy_draw) && identical(dim(input), dim(dummy_draw)))){
      stop(paste("The underlying probability space returns a Heterogeneous
                 or different dimension. A(n) ", class(input), " with dim ",
                 length(dim(input)), " was given."))
    } else if (length(input) != length(dummy_draw))
      stop("Input has wrong length")

    if (all(sapply(input, is.numeric) == sapply(dummy_draw, is.numeric))){
      return(self$fun(input))
    } else
      stop(paste("Expect a(n) ", typeof(dummy_draw), ". Was given
                 a(n) ", typeof(input), "."))


  } else if (is.list(input)){
    if (!(is.list(dummy_draw) && identical(dim(input), dim(dummy_draw))))
      stop(paste("The underlying probability space returns a Homogeneous
                 or different dimension. A(n) ", class(input), " with dim ",
                 length(dim(input)), " was given."))

    if (identical(class(dummy_draw), class(input))){
      return(self$fun(input))
    } else
      stop(paste("Expect a(n) ", typeof(dummy_draw), ". Was given
                 a(n) ", typeof(input), "."))

  }
}

check_same_probSpace.RV <- function(self, other){
  if (is_scalar(other)){
    invisible()
  } else
    check_same(self$probSpace, other$probSpace)
}

#' @export
apply <- function(self, ...) UseMethod("apply")
#' @export
apply.default <- base::apply
# add a ... argument to apply.default to allow passing of package checks:
formals(apply.default) <- c(formals(apply.default), alist(... = ))

#' Transform a random variable by a function.
#'
#' @param func: function to apply to the random variable
#'   
#' @examples
#' X = RV(Exponential(1))
#' Y = X.apply(log)
#'
#' Note: For most standard functions, you can apply the function to
#' the random variable directly. For example, in the example above,
#' Y = log(X) would have been equivalent and more readable.
#'
#' User defined functions can also be applied.
#'
#' @examples 
#' g <- function(x)
#'   return log(x ** 2)
#' Y = X %>% apply(g)
#' @export
apply.RV <- function(self, func, ...){
  f_new <- function(outcome)
    return(func(self$fun(outcome), ...))

  return(RV(self$probSpace, f_new))
}

#-----------------------------------------
# To be implemented later: iter, getitem
#-----------------------------------------

#----------------------------------------------
# For transforming 
#----------------------------------------------
#' @export
abs.RV <- function(self)
  return(apply.RV(self, abs))

#' @export
sqrt.RV <- function(self)
  return(apply(self, sqrt))

#' @export
exp.RV <- function(self)
  return(apply(self, exp))

#' @export
sin.RV <- function(self)
  return(apply(self, sin))

#' @export
cos.RV <- function(self)
  return(apply(self, cos))

#' @export
tan.RV <- function(self)
  return(apply(self, tan))

#' @export
factorial <- function(self) UseMethod("factorial")
#' @export
factorial.default <- base::factorial
# add a ... argument to factorial.default to allow passing of package checks:
formals(factorial.default) <- c(formals(factorial.default), alist(... = ))
#' @export
factorial.RV <- function(self)
  return(apply(self, factorial))

#' @export
log.RV <- function(self, base = exp(1))
  return(apply(self, log, base))

#------------------------------------------------------------
# Operations
#------------------------------------------------------------
#' @export
`%+%` <- function(self, other) UseMethod("%+%")
#' @export
`%+%.default` <- function(self, other) stop("Could not perform the operation")
#' @export
`%+%.RV` <- function(self, other){
  check_same_probSpace(self, other)
  if (is_scalar(other)){
    return(apply.RV(self, function(x) self$fun(x) + other))
  } else if (inherits(other, "RV")){
    func <- function(x){
      self$fun(x) + other$fun(draw(other$probSpace))
    }
    return(apply.RV(self, func))
  } else
    warning("NotImplemented")
}

#' @export
`%+%.numeric` <- function(scalar, obj){
  if (inherits(obj, "RV")){
    return(apply.RV(obj, function(x) `+`(scalar, obj$fun(x))))
  } else if (inherits(obj, "Results"))
    return(scalar + obj$results)
}

#' @export
`%-%` <- function(self, other) UseMethod("%-%")
#' @export
`%-%.default` <- function(self, other) stop("Could not perform the operation")

#' @export
`%-%.numeric` <- function(scalar, obj){
  if (inherits(obj, "RV")){
    return(apply.RV(obj, function(x) `-`(scalar, obj$fun(x))))
  } else if (inherits(obj, "Results"))
    return(scalar - obj$results)
}

#' @export
`%-%.RV` <- function(self, other){
  check_same_probSpace(self, other)
  # print("Here")
  if (inherits(self, "RV")){
    if (is_scalar(other)){
      return(apply.RV(self, function(x) self$fun(x) - other))
    } else if (inherits(other, "RV")){
      func <- function(x){
        self$fun(x) - other$fun(draw(other$probSpace))
      }
      return(apply.RV(self, func))
    } else
      warning("NotImplemented")
  } 
}

#' @export
`%/%` <- function(self, other) UseMethod("%/%")
#' @export
`%/%.default` <- function(self, other) stop("Could not perform the operation")

#' @export
`%/%.numeric` <- function(scalar, obj){
  if (inherits(obj, "RV")){
    return(apply.RV(obj, function(x) `/`(scalar, obj$fun(x))))
  } else if (inherits(obj, "Results"))
    return(scalar / obj$results)
}

#' @export
`%/%.RV` <- function(self, other){
  check_same_probSpace(self, other)
  
  if (inherits(self, "RV")){
    if (is_scalar(other)){
      return(apply.RV(self, function(x) self$fun(x) / other))
    } else if (inherits(other, "RV")){
      func <- function(x){
        self$fun(x) / other$fun(draw(other$probSpace))
      }
      return(apply.RV(self, func))
    } else
      warning("NotImplemented")
  } 
}

#' @export
`%*%.RV` <- function(self, other){
  check_same_probSpace(self, other)
  if (is_scalar(other)){
    return(apply.RV(self, function(x) self$fun(x) * other))
  } else if (inherits(other, "RV")){
    func <- function(x){
      self$fun(x) * other$fun(draw(other$probSpace))
    }
    return(apply.RV(self, func))
  } else
    warning("NotImplemented")
}

#' @export
`%*%.numeric` <- function(scalar, obj){
  if (inherits(obj, "RV")){
    return(apply.RV(obj, function(x) `*`(scalar, obj$fun(x))))
  } else if (inherits(obj, "Results"))
    return(scalar * obj$results)
}

#' @export
`%&%.RV` <- function(self, other){
  check_same_probSpace(self, other)

  if (inherits(other, "RV")){
    fun <- function(outcome) {
      a <- self$fun(outcome)
      b <- other$fun(outcome)
      return(c(a, b))
    }
    return(RV(self$probSpace, fun))
  } else
    stop("Joint distributions are only defined for RVs.")
}
