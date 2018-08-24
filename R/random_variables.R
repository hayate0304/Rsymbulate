
# source("probability_space.R")
# source("utils.R")

#' Defines a random variable.
#'  A random variable is a function which maps an outcome of
#'  a probability space to a number.  Simulating a random
#'  variable is a two-step process: first, a draw is taken
#'  from the underlying probability space; then, the function
#'  is applied to that draw to obtain the realized value of
#'  the random variable.
#' @param probSpace (ProbabilitySpace): the underlying
#' probability space of the random variable.
#' @param fun (function, optional)
#' @examples
#' A single draw is a sequence of 0s and 1s
#' , e.g., (0, 0, 1, 0, 1). P = BoxModel([0, 1], size=5)
#' @return A RV
#' @export
RV <- function(probSpace, fun = function(x) x){
  me <- list(probSpace = probSpace,
             fun = fun)

  class(me) <- append(class(me), "RV")
  return(me)
}

#' @export
draw.RV <- function(self)

  # A function that takes no arguments and returns a single
  #   realization of the random variable.
  #
  # Example:
  #   X = RV(Normal(0, 1))
  #   X.draw() might return -0.9, for example.
  return(self$fun(draw(self$probSpace)))

#' @export
sim.RV <- function(self, n){
  #   Simulate n draws from probability space described by the random
  #   variable.
  #
  # Args:
  #   n (int): How many draws to make.
  #
  # Returns:
  #   Results: A list-like object containing the simulation results.

  if (length(draw(self)) == 1){
    return(RVResults(replicate(n, draw(self))))
  } else
    return(RVResults(t(replicate(n, draw(self)))))
}

#' @export
call <- function(self, input) UseMethod("call")
#' @export
call.default <- function(self, input) return(NULL)

#' @export
call.RV <- function(self, input){
  cat("Warning: Calling an RV as a function simply applies the function that defines\n
the RV to the input, regardless of whether the input is a valid outcome in\n
the underlying probability space.\n")

  dummy_draw = draw(self$probSpace)
  #
  #paste("Dummy: ", dummy_draw)


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
Apply.RV <- function(self, func){
  # Transform a random variable by a function.
  #
  # Args:
  #   function: function to Apply to the random variable
  #
  # Example:
  #   X = RV(Exponential(1))
  # Y = X.Apply(log)
  #
  # Note: For most standard functions, you can Apply the function to
  # the random variable directly. For example, in the example above,
  # Y = log(X) would have been equivalent and more readable.
  #
  # User defined functions can also be applied.
  #
  # Example:
  #   def g(x):
  #   return log(x ** 2)
  # Y = X.Apply(g)

  # f_new <- function(outcome)
  #   return(func(self$fun(outcome)))

  return(RV(self$probSpace, func))
}

#-----------------------------------------
# To be implemented later: iter, getitem
#-----------------------------------------

# e.g., abs(X)
#' @export
abs.RV <- function(self){
  return(Apply.RV(self, function(x) abs(x)))
}

#
# operation_factory <- function(self, op) UseMethod("operation_factory")
#
# operation_factory.default <- function(self, op) return(NULL)

#' @export
`%+%` <- function(self, other) UseMethod("%+%")
#' @export
`%+%.default` <- function(self, other) return(NULL)
#' @export
`%+%.RV` <- function(self, other){
  check_same_probSpace(self, other)
  if (is_scalar(other)){
    return(Apply.RV(self, function(x) self$fun(x) + other))
  } else if (inherits(other, "RV")){
    func <- function(x){
      self$fun(x) + other$fun(draw(other$probSpace))
    }
    return(Apply.RV(self, func))
  } else
    warning("NotImplemented")
}

#' @export
`%+%` <- function(scalar, rv){
  #return(-1 * (`%-%.RV`(rv, scalar)))
  return(Apply.RV(rv, function(x) `+`(scalar, rv$fun(x))))
}

`%-%` <- function(self, other) UseMethod("%-%")
`%-%.default` <- function(self, other) return(NULL)

#' @export
`%-%` <- function(scalar, rv){
  #return(-1 * (`%-%.RV`(rv, scalar)))
  return(Apply.RV(rv, function(x) `-`(scalar, rv$fun(x))))
}

#' @export
`%-%.RV` <- function(self, other){
  check_same_probSpace(self, other)

  if (inherits(self, "RV"))
    if (is_scalar(other)){
      return(Apply.RV(self, function(x) self$fun(x) - other))
    } else if (inherits(other, "RV")){
      func <- function(x){
        self$fun(x) - other$fun(draw(other$probSpace))
      }
      return(Apply.RV(self, func))
    } else
      warning("NotImplemented")
}
