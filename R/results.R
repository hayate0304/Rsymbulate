# Data structures for storing the results of a simulation.
#
# This module provides data structures for storing the
# results of a simulation, either outcomes from a
# probability space or realizations of a random variable /
# random process.
#

#' @import ggplot2
#---------------------------------------------------------
#             Results Class
#---------------------------------------------------------
#' @export
Results <- function(results){
  me <- list(results = results)
  class(me) <- c(class(me), "Results")

  return(me)
}

#' Apply a function to each outcome of a simulation.
#'
#' @param fun: A function to apply to each outcome.
#' @return Results: A Results object of the same length,
#' where each outcome is the result of applying
#' the function to each outcome from the original
#' Results object.
#' @export
apply.Results <- function(self, fun, ...){
  if (get_dimesion(self) == 0){
    results <- sapply(self$results, fun, ...)
  } else
    results <- apply(self$results, 1, fun, ...)

  # Python code uses type(self) for typecasting, not sure how to do that in R so if-else is used
  if (inherits(self, "RVResults")){
    return(RVResults(results))
  } else
    return(Results(results))
}

#' @export
print.Results <- function(self)
  print(self$results)

#' @export
get <- function(self, i) UseMethod("get")
#' @export
get.default <- function(self, i) stop("Could not perform the function")
#' @export
get.Results <- function(self, i){
  if (is.matrix(self$results)){
    return(self$results[, i])
  } else
    return(self$results[i])
}

#' @export
length.Results <- function(self){
  if (is.matrix(self$results)){
    return(dim(self$results)[1])
  } else
    return(length(self$results))
}

#' @export
tabulate <- function(self, ...) UseMethod("tabulate")
#' @export
tabulate.default <- base::tabulate

# Add a ... argument to tabulate.default to allow passing of package checks:
formals(tabulate.default) <- c(formals(tabulate.default), alist(... = ))

#' Counts up how much of each outcome there were.
#'
#' @param normalize (bool): If True, return the relative
#' frequency. Otherwise, return the counts.
#' Defaults to False.
#' @return Data frame: A data frame with each of the observed
#' outcomes and their frequencies.
#' @export
tabulate.Results <- function(self, normalize = FALSE){
  df <- plyr::count(as.data.frame(self$results))

  if (ncol(df) == 2)
    names(df) <- c("Outcome", "Value")
  if (normalize) {
    #print(df)
    if (ncol(df) == 2){
      df$Value <- round(df$Value / sum(df$Value), 4)
      #print(df$Value)
    } else {
      df$Value <- round(df$freq / sum(df$freq), 4)
      df$freq <- NULL
    }
  }

  return(df)
}

#------------------------------------------------------------------------
# filter family
#------------------------------------------------------------------------

#' @export
filter <- function(self, ...) UseMethod("filter")
#' @export
filter.default <- stats::filter

# Add a ... argument to filter.default to allow passing of package checks:
formals(filter.default) <- c(formals(filter.default), alist(... = ))

#' filters the results of a simulation and
#' returns only those outcomes that satisfy
#' a given criterion.
#'
#' @param fun (outcome -> bool): A function that
#' takes in an outcome and returns a
#' True / False. Only the outcomes that
#' return True will be kept; the others
#' will be filtered out.
#'
#' @return Results: Another Results object containing
#' only those outcomes for which the function
#' returned True.
#' @export
filter.Results <- function(self, fun){
  # Python code uses type(self) for typecasting, not sure how to do that in R so if-else is used
  if (inherits(self, "RVResults")){
    return(RVResults(self$results[fun(self$results)]))
  } else
    return(Results(self$results[fun(self$results)]))
}

#' @export
filter_eq <- function(self, value) UseMethod("filter_eq")
#' @export
filter_eq.default <- function(self, value) stop("Could not perform the function")
#' @export
filter_eq.Results <- function(self, value)
  return(filter(self, function(x) x == value))

#' @export
filter_neq <- function(self, value) UseMethod("filter_neq")
#' @export
filter_neq.default <- function(self, value) stop("Could not perform the function")
#' @export
filter_neq.Results <- function(self, value)
  return(filter(self, function(x) x != value))

#' @export
filter_lt <- function(self, value) UseMethod("filter_lt")
#' @export
filter_lt.default <- function(self, value) stop("Could not perform the function")
#' @export
filter_lt.Results <- function(self, value)
  return(filter(self, function(x) x < value))

#' @export
filter_leq <- function(self, value) UseMethod("filter_leq")
#' @export
filter_leq.default <- function(self, value) stop("Could not perform the function")
#' @export
filter_leq.Results <- function(self, value)
  return(filter(self, function(x) x <= value))

#' @export
filter_gt <- function(self, value) UseMethod("filter_gt")
#' @export
filter_gt.default <- function(self, value) stop("Could not perform the function")
#' @export
filter_gt.Results <- function(self, value)
  return(filter(self, function(x) x > value))

#' @export
filter_geq <- function(self, value) UseMethod("filter_geq")
#' @export
filter_geq.default <- function(self, value) stop("Could not perform the function")
#' @export
filter_geq.Results <- function(self, value)
  return(filter(self, function(x) x >= value))

#------------------------------------------------------
# count family
# The following functions return an integer indicating
# how many outcomes passed a given criterion.
#------------------------------------------------------
#' @export
count <- function(self, value) UseMethod("count")
#' @export
count.default <- function(self, value) stop("Could not perform the function")

#' counts the number of outcomes that satisfied a given criterion.
#'
#' @param fun (outcome -> bool): A function that
#' takes in an outcome and returns a
#' True / False. Only the outcomes that
#' return True will be counted.
#'
#' @return int: The number of outcomes for which
#' the function returned True.
#' @export
count.Results <- function(self, fun = function(x) TRUE){
  return(length(filter(self, fun)$result))
}

#' @export
count_eq <- function(self, value) UseMethod("count_eq")
#' @export
count_eq.default <- function(self, value) stop("Could not perform the function")
#' @export
count_eq.Results <- function(self, value){
  return(length(filter_eq(self, value)$result))
}

#' @export
count_neq <- function(self, value) UseMethod("count_neq")
#' @export
count_neq.default <- function(self, value) stop("Could not perform the function")
#' @export
count_neq.Results <- function(self, value){
  return(length(filter_neq(self, value)$result))
}

#' @export
count_lt <- function(self, value) UseMethod("count_lt")
#' @export
count_lt.default <- function(self, value) stop("Could not perform the function")
#' @export
count_lt.Results <- function(self, value){
  return(length(filter_lt(self, value)$result))
}

#' @export
count_leq <- function(self, value) UseMethod("count_leq")
#' @export
count_leq.default <- function(self, value) stop("Could not perform the function")
#' @export
count_leq.Results <- function(self, value){
  return(length(filter_leq(self, value)$result))
}

#' @export
count_gt <- function(self, value) UseMethod("count_gt")
#' @export
count_gt.default <- function(self, value) stop("Could not perform the function")
#' @export
count_gt.Results <- function(self, value){
  return(length(filter_gt(self, value)$result))
}

#' @export
count_geq <- function(self, value) UseMethod("count_geq")
#' @export
count_geq.default <- function(self, value) stop("Could not perform the function")
#' @export
count_geq.Results <- function(self, value){
  return(length(filter_geq(self, value)$result))
}

#' @export
plot <- function(self, ...) UseMethod("plot")
#' @export
plot.default <- graphics::plot

# Add a ... argument to plot.default to allow passing of package checks:
formals(plot.default) <- c(formals(plot.default), alist(... = ))

#' @export
plot.Results <- function(self)
  stop(paste("Only simulations of random variables (RV) ",
             "can be plotted, but you simulated from a ",
             "probability space. You must first define a RV ",
             "on your probability space and simulate it. ",
             "Then call plot() on those simulations."))

#--------------------------------------------------------------------------
# Operators
#--------------------------------------------------------------------------
#' @export
`%-%.Results` <- function(self, scalar){
  return(self$results - scalar)
}

#' @export
`%+%.Results` <- function(self, scalar){
  return(self$results + scalar)
}

#' @export
`%/%.Results` <- function(self, scalar){
  return(self$results / scalar)
}

#' @export
`%*%.Results` <- function(self, scalar){
  return(self$results * scalar)
}

#--------------------------------------------------------------------------
# Stats functions
#--------------------------------------------------------------------------
#' @export
mean.Results <- function(self)
  stop(paste("You can only call mean() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call mean() on those simulations."))

#' @export
var <- function(self, ...) UseMethod("var")
#' @export
var.default <- stats::var

# Add a ... argument to var.default to allow passing of package checks:
formals(var.default) <- c(formals(var.default), alist(... = ))

#' @export
var.Results <- function(self)
  stop(paste("You can only call var() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call var() on those simulations."))

#' @export
sd <- function(self) UseMethod("sd")
#' @export
sd.default <- stats::sd

# Add a ... argument to sd.default to allow passing of package checks:
formals(sd.default) <- c(formals(sd.default), alist(... = ))

#' @export
sd.Results <- function(self)
  stop(paste("You can only call sd() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call sd() on those simulations."))

#' @export
cov <- function(self) UseMethod("cov")
#' @export
cov.default <- stats::cov

# Add a ... argument to sd.default to allow passing of package checks:
formals(cov.default) <- c(formals(cov.default), alist(... = ))

#' @export
cov.Results <- function(self)
  stop(paste("You can only call cov() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call cov() on those simulations."))

#' @export
cor <- function(self) UseMethod("cor")
#' @export
cor.default <- stats::cor

# Add a ... argument to sd.default to allow passing of package checks:
formals(cor.default) <- c(formals(cor.default), alist(... = ))

#' @export
cor.Results <- function(self)
  stop(paste("You can only call cor() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call cor() on those simulations."))

#---------------------------------------------------------
#             RVResults Class
#---------------------------------------------------------
#' @export
RVResults <- function(results){
  me <- list(results = results)

  class(me) <- c(class(me), "RVResults", "Results")
  return(me)
}

#' @export
plot.RVResults <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
                            jitter=FALSE, bins=NULL){

  dim <- get_dimesion(self)

  # If RVResults is vector
  if (dim == 0){
    tb <- tabulate(self)
    #print(tb)
    heights <- tb$Value
    #print(heights)
    discrete <- is_discrete(heights)
    #print(paste("Dis: ", discrete))

    if (identical(type, NULL)){
      if (discrete){
        type <- c(type, "impulse")
      } else
        type <- c(type, "hist")
    }
    if (identical(alpha, NULL))
      alpha <- 0.45
    if (identical(bins, NULL))
      bins <- 30

    color <- get_next_color()
    ylab <- "count"
    tb <- tabulate(self, normalize = normalize)

    #--------------------------------------
    ## if density in type to be implemented
    #--------------------------------------

    if (is.element("hist", type) || is.element("bar", type)){
      if (normalize)
        ylab = "Density"

      if (identical(type, c("hist", "impulse"))){
        ggplot2::ggplot(tb, aes(Outcome, Value)) +
          ggplot2::geom_bar(stat="identity", fill = color(), , alpha = alpha) +
          labs(y=ylab, x="")
      } else {
        if (normalize){
          hist <- ggplot2::geom_histogram(bins = bins, fill = color(), ggplot2::aes(y=..density..),
                                          alpha = alpha)
        }else{
          hist <- ggplot2::geom_histogram(bins = bins, fill = color(), alpha = alpha)
        }

        ggplot2::ggplot() + ggplot2::aes(self$results) +
          hist +
          labs(y=ylab, x="")
      }
    }
    else if (is.element("impulse", type)){
      x <- as.double(tb$Outcome)
      y <- round(tb$Value, 4)

      #print("Here")
      #print(y)

      if (identical(alpha, NULL))
        alpha <- 0.7
      if (normalize)
        y <- y / sum(y)
      if (jitter){
        a <- 0.05 * (max(x) - min(x))
        noise <- runif(1, -a, a)
        x <- x + noise
      }

      #print(x)
      #print(y)

      if (normalize)
        ylab = "Relative Frequency"

      # plot the impulses
      # graphics::plot(x, y, type="h", col = rgb(color,alpha = alpha),
      #                ylab = ylab, xlab = "")
      ggplot2::ggplot(tb, aes(x=Outcome, xend=Outcome, y=0, yend=Value)) +
        ggplot2::geom_segment(color=color(), alpha = alpha) +
        ylab(ylab)
    }
  } else if (dim == 2){
    x <- self$results[,1]
    y <- self$results[,2]
#
#     print(x)
#     print(y)
    x_height <- as.vector(table(self$results[,1]))
    y_height <- as.vector(table(self$results[,2]))
    discrete_x <- is_discrete(x_height)
    discrete_y <- is_discrete(y_height)

    if (identical(type, NULL))
      type <- c(type, "scatter")
    if (identical(alpha, NULL))
      alpha = 0.4
    if (identical(bins, NULL))
      if (is.element("tile", type)){
        bins = 10
      } else
        bins = 30

    if (is.element("marginal", type)){
      warning("To be implemeted")
    } else
      color <- get_next_color()

    if (is.element("scatter", type)){
      if (jitter){
        x <- x + rnorm(length(x), 0, .01 * (max(x) - min(x)))
        y <- y + rnorm(length(y), 0, .01 * (max(y) - min(y)))
      }

      ggplot2::ggplot(, aes(x=x, y=y)) +
        ggplot2::geom_point(size=2, color = color(), alpha = alpha) +
        labs(y="", x="")
    }
  }
}

#--------------------------------------------------------------------------
# Stats functions
#--------------------------------------------------------------------------

#' @export
mean.RVResults <- function(self){
 if (get_dimesion(self) == 0){
   return(mean(self$results))
 } else if (get_dimesion(self) > 0){
   return(apply(self$results, 1, mean))
 } else
   stop("I don't know how to take the mean of these values.")
}

#' @export
var.RVResults <- function(self){
  if (get_dimesion(self) == 0){
    return(var(self$results))
  } else if (get_dimesion(self) > 0){
    return(apply(self$results, 1, var))
  } else
    stop("I don't know how to take the variance of these values.")
}

#' @export
sd.RVResults <- function(self){
  if (get_dimesion(self) == 0){
    return(sd(self$results))
  } else if (get_dimesion(self) > 0){
    return(apply(self$results, 1, sd))
  } else
    stop("I don't know how to take the standard deviation of these values.")
}

#' @export
cov.RVResults <- function(self){
  if (get_dimesion(self) > 0){
    return(cov(self$results))
  } else
    stop("Covariance requires that the simulation results have consistent dimension.")
}

#' @export
cor.RVResults <- function(self){
  if (get_dimesion(self) > 0){
    return(cor(self$results))
  } else
    stop("Correlation requires that the simulation results have consistent dimension.")
}
