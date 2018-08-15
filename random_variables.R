
source("probability_space.R")
source("scalar.R")
# Defines a random variable.
#     
# A random variable is a function which maps an outcome of
# a probability space to a number.  Simulating a random 
# variable is a two-step process: first, a draw is taken 
# from the underlying probability space; then, the function 
# is applied to that draw to obtain the realized value of
# the random variable.
# 
# Args:
#   probSpace (ProbabilitySpace): the underlying probability space
#   of the random variable.
#   fun (function, optional): a function that maps draws from the 
#   probability space to numbers. (By default, the function is the 
#   identity function. For named distributions, a draw from the
#   underlying probability space is the value of the random
#   variable itself, which is why the identity function is the 
#   most frequently used.)
# 
# Attributes:
#   probSpace (ProbabilitySpace): the underlying probability space
#   of the random variable.
#   fun (function): a function that maps draws from the probability
#   space to numbers.
# 
# Examples:
# # a single draw is a sequence of 0s and 1s, e.g., (0, 0, 1, 0, 1)
# P = BoxModel([0, 1], size=5)
# # X counts the number of 1s in the draw, e.g., 5
# X = RV(P, sum)
# 
# # the function is the identity, so Y has a Normal(0, 1) distribution
# Y = RV(Normal(0, 1)
# 
# # a single draw from BivariateNormal is a tuple of two numbers
# P = BivariateNormal()
# # Z is the smaller of the two numbers
# Z = RV(P, min)

RV <- function(probSpace, fun = function(x) x){
  me <- list(probSpace = probSpace,
             fun = fun)
  
  class(me) <- append(class(me), "RV")
  return(me)
}

draw.RV <- function(self)
  
  # A function that takes no arguments and returns a single 
  #   realization of the random variable.
  # 
  # Example:
  #   X = RV(Normal(0, 1))
  #   X.draw() might return -0.9, for example.  
  
  return(self$fun(draw(self$probSpace)))

sim.RV <- function(self, n){
  #   Simulate n draws from probability space described by the random 
  #   variable.
  # 
  # Args:
  #   n (int): How many draws to make.
  # 
  # Returns:
  #   Results: A list-like object containing the simulation results.
  
  # To be implemented  
}

call <- function(self, input) UseMethod("call")

call.default <- function(self, input) return(NULL)

call.RV <- function(self, input){
  print("Warning: Calling an RV as a function simply applies the function that defines 
        the RV to the input, regardless of whether the input is a valid outcome in 
        the underlying probability space.")
  
  dummy_draw = draw(self$proSpace)
  
  # R doesn't have scalar type
  if (is.atomic(input)){
    if (!(is.atomic(dummy_draw) && identical(dim(input), dim(dummy_draw)))){
      stop(paste("The underlying probability space returns a Heterogeneous
                 or different dimension. A(n) ", class(input), " with dim ",
                 length(dim(input)), " was given."))
    } else if (length(input) != length(dummy_draw))
      stop("Input has wrong length")
    
    if (all(sapply(input, typeof) == sapply(dummy_draw, typeof))){
      return(self$fun(input))
    } else {
      stop(paste("Expect a(n) ", typeof(dummy_draw), ". Was given 
                 a(n) ", typeof(input), "."))
    }
  } else if (is.list(input)){
    if (!(is.list(dummy_draw) && identical(dim(input), dim(dummy_draw))))
      stop(paste("The underlying probability space returns a Homogeneous
                 or different dimension. A(n) ", class(input), " with dim ",
                 length(dim(input)), " was given."))
    
    if (identical(typeof(dummy_draw), typeof(input))){
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

apply <- function(self, func) UseMethod("apply")

apply.default <- function(self, func) return(NULL)

apply.RV <- function(self, func){
  # Transform a random variable by a function.
  # 
  # Args:
  #   function: function to apply to the random variable
  # 
  # Example:
  #   X = RV(Exponential(1))
  # Y = X.apply(log)
  # 
  # Note: For most standard functions, you can apply the function to
  # the random variable directly. For example, in the example above,
  # Y = log(X) would have been equivalent and more readable.
  # 
  # User defined functions can also be applied.
  # 
  # Example:
  #   def g(x):
  #   return log(x ** 2)
  # Y = X.apply(g)
  
  f_new <- function(outcome)
    return(func(self$fun(outcome)))
  return(call(self$probSpace, f_new))
}

#-----------------------------------------
# To be implemented later: iter, getitem
#-----------------------------------------


# e.g., abs(X)
abs.RV <- function(self){
  return(apply(self, function(x) abs(x)))
} 
  
operation_factory <- function(self, op) UseMethod("operation_factory")

operation_factory.default <- function(self, op) return(NULL)

operation_factory.RV <- function(self, op){
  # The code for most operations (+, -, *, /, ...) is the
  # same, except for the operation itself. The following 
  # factory function takes in the the operation and 
  # generates the code to perform that operation.
  
  op_fun <- function(self, other){
    check_same_probSpace(self, other)
    if (is_scalar(other)){
      return(apply(self, function(x) op(x, other)))
    } else if (inherits(other, "RV")){
      fun <- function(outcome){
        a = self$fun(outcome)
        b = other$fun(outcome)
      }
    }
    
  }
}

  









