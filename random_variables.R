
source("probability_space.R")

"""Defines a random variable.
    
A random variable is a function which maps an outcome of
a probability space to a number.  Simulating a random 
variable is a two-step process: first, a draw is taken 
from the underlying probability space; then, the function 
is applied to that draw to obtain the realized value of
the random variable.

Args:
  probSpace (ProbabilitySpace): the underlying probability space
  of the random variable.
  fun (function, optional): a function that maps draws from the 
  probability space to numbers. (By default, the function is the 
  identity function. For named distributions, a draw from the
  underlying probability space is the value of the random
  variable itself, which is why the identity function is the 
  most frequently used.)

Attributes:
  probSpace (ProbabilitySpace): the underlying probability space
  of the random variable.
  fun (function): a function that maps draws from the probability
  space to numbers.

Examples:
# a single draw is a sequence of 0s and 1s, e.g., (0, 0, 1, 0, 1)
P = BoxModel([0, 1], size=5)
# X counts the number of 1s in the draw, e.g., 5
X = RV(P, sum)

# the function is the identity, so Y has a Normal(0, 1) distribution
Y = RV(Normal(0, 1)

# a single draw from BivariateNormal is a tuple of two numbers
P = BivariateNormal()
# Z is the smaller of the two numbers
Z = RV(P, min)
"""
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


