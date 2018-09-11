
#' @import ggplot2

wrap <- function(fun, args){
  p <- paste0("do.call(", fun, args, ")")
  return(parse(text = p))
}

Distribution <- function(params, dist, discrete = TRUE){
  f <- paste0("d", dist)
  pdf <- function(x){
    x <- list(x)
    do.call(f, c(x, params))
  }

  ppf <- paste0("q", dist)
  xlim = c(do.call(ppf, c(0.001, params)), do.call(ppf, c(0.999, params)))

  attribute <- list(params = params,
                    pdf = pdf,
                    xlim = xlim,
                    discrete = discrete)

  class(attribute) <- c(class(attribute), "Distribution", "ProbabilitySpace")
  return(attribute)
}

#' @export
plot.Distribution <- function(self, type = NULL, alpha = 0.4,
                              xlim = NULL, new = NULL, ...){
  xlower = self$xlim[1]
  xupper = self$xlim[2]

  if (self$discrete){
    xlower <- as.integer(xlower)
    xupper <- as.integer(xupper)
    xvals <- xlower:xupper
  } else
    xvals <- seq(xlower, xupper, length.out = 100)

  yvals <- self$pdf(xvals)

  color <- get_next_color()
  col <- color()
  y <- ""
  if (identical(new, NULL)){
    g <- ggplot()
  } else{
    g <- new
    col <- "seagreen"
    alpha = 1
    y <- new$labels$y
  }

  if (self$discrete){
    g <- g +
      geom_point(aes(x=xvals, y=yvals, alpha = alpha, ...), size = 2.5, color = col) +
      geom_line(aes(x=xvals, y=yvals, alpha = alpha, ...), color = col)
  } else{
    g <- g +
      geom_line(aes(x=xvals, y=yvals, alpha = alpha, ...), color = col, size = 1.2,
                na.rm = TRUE)

  }

  g <- g + xlim(self$xlim) + theme(legend.position="none") +
    labs(y = y, x = "")

  return(suppressMessages(g))
}

#----------------------------------------------------------
## Discrete Distributions
#----------------------------------------------------------

#' Defines a probability space for a Bernoulli
#' distribution.
#'
#' @param p (double) probability (number between 0 and 1)
#' of a "success" (i.e., 1)
#' @export
Bernoulli <- function(p){
  if (!(p >= 0 && p <= 1))
    stop("p must be between 0 and 1")

  params <- list(1, p)

  attribute <- Distribution(params, "binom", TRUE)
  attribute$xlim = c(0, 1)  # Bernoulli distributions are not defined for x < 0 and x > 1
  attribute[["p"]] = p

  class(attribute) <- append("Bernoulli", class(attribute))
  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Bernoulli distribution.
#' @export
draw.Bernoulli <- function(self)
  return(rbinom(1, 1, self$p))

#' Defines a probability space for a binomial distribution.
#'
#' @param n (int): number of trials
#' @param p (float): probability (number between 0 and 1)
#' that each trial results in a "success" (i.e., 1)
#' @export
Binomial <- function(n, p){
  # is.integer(n) doesn't work here since R requires user to input 10L as integer
  if (n >= 0 && is.numeric(n) && round(n) == n){
    n = n
  } else
    stop("n must be a non-negative integer")

  if (p >= 0 && p <= 1){
    p = p
  } else
    stop("p must be between 0 and 1")

  params <- list(n, p)

  attribute <- Distribution(params, "binom", TRUE)
  attribute$xlim = c(0, n)  # Binomial distributions are not defined for x < 0 and x > n
  attribute[["n"]]= n
  attribute[["p"]] = p

  class(attribute) <- append("Binomial", class(attribute))
  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Binomial distribution.
#' @export
draw.Binomial <- function(self)
  return(rbinom(1, self$n, self$p))

#' Defines a probability space for a hypergeometric
#' distribution (which represents the number of
#' ones in n draws without replacement from a box
#' containing zeros and ones)
#'
#' @param n (int): number of draws (without replacement)
#' from the box
#' @param N0 (int): number of 0s in the box
#' @param N1 (int): number of 1s in the box
#' @export
Hypergeometric <- function(n, N0, N1){
  # is.integer won't work here since R requires "L" specificly for integer
  if (!(n > 0 && is.numeric(n) && round(n) == n))
    stop("n must be a positive integer")

  if (!(N0 > 0 && is.numeric(N0) && round(N0) == N0))
    stop("N0 must be a positive integer")

  if (!(N1 > 0 && is.numeric(N1) && round(N1) == N1))
    stop("N1 must be a positive integer")

  params = list(N1, N0, n)

  attribute <- Distribution(params, "hyper", TRUE)
  attribute$xlim = c(0, n)  # Hypergeometric distributions are not defined for x < 0 and x > n
  attribute[["n"]]= n
  attribute[["N0"]]= N0
  attribute[["N1"]]= N1

  class(attribute) <- append("Hypergeometric", class(attribute))
  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Hypergeometric distribution.
#' @export
draw.Hypergeometric <- function(self)
  return(rhyper(1, self$N1, self$N0, self$n))

#' Defines a probability space for a Poisson distribution.
#'
#' @param lam (float): rate parameter for the Poisson distribution
#' @export
Poisson <- function(lam){
  if (lam < 0)
    stop("Lambda (lam) must be greater than 0")

  params <- list(lam)

  attribute <- Distribution(params, "pois", TRUE)
  attribute$xlim = c(0, attribute$xlim[2])
  attribute[["lam"]]= lam

  class(attribute) <- append("Poisson", class(attribute))
  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Poisson distribution.
#' @export
draw.Poisson <- function(self)
  return(rpois(1, self$lam))

#' Defines a probability space for a discrete uniform distribution.
#'
#' @param a (int): lower bound for possible values
#' @param b (int): upper bound for possible values
#' @export
DiscreteUniform <- function(a = 0, b = 1){
  if (a >= b)
    stop("b cannot be less than or equal to a")

  attribute <- list(a = a,
             b = b)

  class(attribute) <- c(class(attribute), "DiscreteUniform", "Distribution", "ProbabilitySpace")
  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Discrete Uniform distribution.
#' @export
draw.DiscreteUniform <- function(self)
  return(sample(self$a : self$b, 1))

#' Defines a probability space for an uniform distribution.
#'
#' @param a (double): lower bound for possible values
#' @param b (double): upper bound for possible values
#' @export
Uniform <- function(a=0.0, b=1.0){
  if (a > b)
    stop("b cannot be less than a")

  params <- list(a, b)

  attribute <- Distribution(params, "unif", F)

  class(attribute) <- append("Uniform", class(attribute), after = 1)
  attribute$xlim = c(a, b)
  attribute[["a"]]= a
  attribute[["b"]]= b

  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Uniform distribution.
#' @export
draw.Uniform <- function(self)
  return(runif(1, self$a, self$b))

#' Defines a probability space for a normal distribution.
#'
#' @param mean (double): mean parameter of the normal distribution
#' @param sd (double): standard deviation parameter of the normal
#' distribution (if specified, var parameter will be ignored)
#' @param var (double): variance parameter of the normal distribution
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

  params <- list(mean, scale)

  attribute <- Distribution(params, "norm", F)
  attribute[["mean"]]= mean
  attribute[["scale"]]= scale

  class(attribute) <- append("Normal", class(attribute))
  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Normal distribution.
#' @export
draw.Normal <- function(self)
  return(rnorm(1, self$mean, self$scale))

#' Defines a probability space for an exponential distribution.
#' Only one of scale or rate should be set. (The scale is the
#' inverse of the rate.)
#'
#' @param scale double; scale parameter for gamma distribution
#' (often symbolized beta = 1 / lambda)
#' @param rate double; rate parameter for gamma distribution
#' (often symbolized lambda)
#' @export
Exponential <- function(rate = 1.0, scale = NULL){
  if (identical(scale, NULL)){
    if (rate < 0)
      stop("rate must be positive")
  } else {
    if (scale < 0)
      stop("scale must be positive")
  }

  if (identical(scale, NULL)){
    scale = rate
  } else
    scale = 1 / scale
  params <- list(scale)

  attribute <- Distribution(params, "exp", FALSE)
  attribute$xlim = c(0, attribute$xlim[2])
  attribute[["rate"]]= rate

  class(attribute) <- append("Exponential", class(attribute))
  return(attribute)
}

#' A function that takes no arguments and
#' returns a single draw from the Exponential distribution.
#' @export
draw.Exponential <- function(self){
  if (identical(self$scale, NULL)){
    return(rexp(1, self$rate))
  } else
    return(rexp(1, 1 / self$scale))
}
