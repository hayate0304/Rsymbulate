
source("probability_space.R")
source("utils.R")

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
  
  if (length(draw(self)) == 1){
    return(RVResults(replicate(n, draw(self))))
  } else
    return(RVResults(t(replicate(n, draw(self)))))
}

call <- function(self, input) UseMethod("call")

call.default <- function(self, input) return(NULL)

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

# apply <- function(self, func) UseMethod("apply")
# 
# apply.default <- function(self, func) return(NULL)

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
  
  # f_new <- function(outcome)
  #   return(func(self$fun(outcome)))
  
  return(RV(self$probSpace, func))
}

#-----------------------------------------
# To be implemented later: iter, getitem
#-----------------------------------------


# e.g., abs(X)
abs.RV <- function(self){
  return(apply.RV(self, function(x) abs(x)))
} 
  
operation_factory <- function(self, op) UseMethod("operation_factory")

operation_factory.default <- function(self, op) return(NULL)

# operation_factory.RV <- function(self, op){
#   # The code for most operations (+, -, *, /, ...) is the
#   # same, except for the operation itself. The following 
#   # factory function takes in the the operation and 
#   # generates the code to perform that operation.
#   
#   op_fun <- function(self, other){
#     check_same_probSpace(self, other)
#     if (is_scalar(other)){
#       return(apply(self, function(x) op(x, other)))
#     } else if (inherits(other, "RV")){
#       fun <- function(outcome){
#         a = self$fun(outcome)
#         b = other$fun(outcome)
#         
#         ## Can be reduce to 1 if-else
#         if (is_vector(a) && is_vector(b) && length(a) == length(b)){
#           return(op(a, b))
#         } else if (is_scalar(a) && is_scalar(b)){
#           return(op(a, b))
#         } else 
#           stop(paste("Could not perform operation on the outcomes ",
#                      toString(a), " and ", toString(b)))
#       }
#     } else 
#       warning("Not Implemented")
#     
#   }
#   return(op_fun)
# }

`%+%` <- function(self, other) UseMethod("%+%")

`%+%.default` <- function(self, other) return(NULL)

`%+%.RV` <- function(self, other){
  check_same_probSpace(self, other)
  if (is_scalar(other)){
    return(apply.RV(self, function(x) x + other))
  } else if (inherits(other, "RV")){
    func <- function(x){
      x + other$fun(draw(other$probSpace))
    }
    return(apply.RV(self, func))
  } else 
    warning("NotImplemented")
}

`%-%` <- function(self, other) UseMethod("%-%")

`%-%.default` <- function(self, other) return(NULL)

`%-%.RV` <- function(self, other){
  check_same_probSpace(self, other)
  if (is_scalar(other)){
    return(apply.RV(self, function(x) x - other))
  } else if (inherits(other, "RV")){
    func <- function(x){
      x - other$fun(draw(other$probSpace))
    }
    return(apply.RV(self, func))
  } else 
    warning("NotImplemented")
}
  









