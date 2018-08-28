
# source("probability_space.R")
# library(ggplot2)
# library(rlist)

Distribution <- function(params, discrete = TRUE){
  me <- list()

  class(me) <- rlist::list.append(class(me), "Distribution", "ProbabilitySpace")
  return(me)
}

#' @export
Binomial <- function(n, p){
  # Defines a probability space for a binomial
  # distribution.
  #
  # Attributes:
  #   n (int): number of trials
  # p (float): probability (number between 0 and 1)
  # that each trial results in a "success" (i.e., 1)

  if (n >= 0 && is.numeric(n) && round(n) == n){
    self.n = n
  } else
    stop("n must be a non-negative integer")

  if (p >= 0 && p <= 1){
    self.p = p
  } else
    stop("p must be between 0 and 1")

  me <- list(n = self.n,
             p = self.p)
  class(me) <- rlist::list.append(class(me), "Binomial", "Distribution",
                                  "ProbabilitySpace")
  return(me)
}

#' @export
Draw.Binomial <- function(self)
  return(rbinom(1, self$n, self$p))

#' @export
DiscreteUniform <- function(self, a = 0, b = 1){
  # Defines a probability space for a discrete uniform distribution.
  #
  # Attributes:
  #   a (int): lower bound for possible values
  # b (int): upper bound for possible values

  if (a >= b)
    stop("b cannot be less than or equal to a")

  me <- list(a = a,
             b = b,
             xlim = c(a,b))
  class(me) <- rlist::list.append(class(me), "DiscreteUniform", "Distribution",
                                  "ProbabilitySpace")
  return(me)
}

#' @export
Draw.DiscreteUniform <- function(self)
  return(sample(self$a : self$b, 1))

#' @export
Uniform <- function(a=0.0, b=1.0){
  if (a > b)
    stop("b cannot be less than a")

  me <- list(a = a,
             b = b,
             xlim = c(a,b))
  class(me) <- rlist::list.append(class(me), "Uniform", "Distribution",
                                  "ProbabilitySpace")
  return(me)
}

#' @export
Draw.Uniform <- function(self)
  return(runif(1, self$a, self$b))

#' @export
Normal <- function(mean=0.0, sd=1.0, var=NULL){
  if (identical(var, NULL)){
    if (sd > 0){
      scale = sd
    } else if (sd == 0){
      stop("NotImplementedError")
    } else
      stop("sd cannot be less than 0")
  } else {
    if (var > 0){
      scale = sqrt(var)
    } else if (var == 0){
      stop("NotImplementedError")
    } else
      stop("var cannot be less than 0")
  }

  me <- list(mean = mean,
             scale = scale)
  class(me) <- rlist::list.append(class(me), "Normal", "Distribution",
                                  "ProbabilitySpace")
  return(me)
}

#' @export
Draw.Normal <- function(self)
  return(rnorm(1, self$mean, self$scale))

