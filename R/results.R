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
  class(me) <- append(class(me), "Results")

  return(me)

}

#' @export
Apply <- function(self, fun, ...) UseMethod("Apply")
#' @export
Apply.default <- function(self, fun, ...) stop("Could not perform the function")

#' Apply a function to each outcome of a simulation.
#' 
#' @param fun: A function to apply to each outcome.
#' @return Results: A Results object of the same length,
#' where each outcome is the result of applying
#' the function to each outcome from the original
#' Results object.
#' @export
Apply.Results <- function(self, fun, ...){
  if (get_dimesion(self) == 0){
    return(Results(sapply(self$results, fun, ...)))
  } else
    return(Results(apply(self$results, 1, fun, ...)))
}

#' @export
Get <- function(self, i) UseMethod("Get")
#' @export
Get.default <- function(self, i) stop("Could not perform the function")
#' @export
Get.Results <- function(self, i){
  if (is.matrix(self$results)){
    return(self$results[, i])
  } else 
    return(self$results[i])
}
  
#' @export
Len <- function(self) UseMethod("Len")
#' @export
Len.default <- function(self) stop("Could not perform the function")
#' @export
Len.Results <- function(self){
  if (is.matrix(self$results)){
    return(dim(self$results)[1])
  } else 
    return(length(self$results))
}
  
#' @export
Tabulate <- function(self, normalize = FALSE) UseMethod("Tabulate")
#' @export
Tabulate.default <- function(self, normalize = FALSE) stop("Could not perform the function")

#' Counts up how much of each outcome there were.
#'
#' @param normalize (bool): If True, return the relative
#' frequency. Otherwise, return the counts.
#' Defaults to False.
#' @return Data frame: A data frame with each of the observed
#' outcomes and their frequencies.
#' @export
Tabulate.Results <- function(self, normalize = FALSE){
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
# Filter family
#------------------------------------------------------------------------

#' @export
Filter <- function(self, fun) UseMethod("Filter")
#' @export
Filter.default <- function(self, fun) stop("Could not perform the function")

#' Filters the results of a simulation and
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
Filter.Results <- function(self, fun){
  return(Results(self$results[fun(self$results)]))
}

#' @export
Filter_eq <- function(self, value) UseMethod("Filter_eq")
#' @export
Filter_eq.default <- function(self, value) stop("Could not perform the function")
#' @export
Filter_eq.Results <- function(self, value)
  return(Results(self$results[self$results == value]))

#' @export
Filter_neq <- function(self, value) UseMethod("Filter_neq")
#' @export
Filter_neq.default <- function(self, value) stop("Could not perform the function")
#' @export
Filter_neq.Results <- function(self, value)
  return(Results(self$results[self$results != value]))

#' @export
Filter_lt <- function(self, value) UseMethod("Filter_lt")
#' @export
Filter_lt.default <- function(self, value) stop("Could not perform the function")
#' @export
Filter_lt.Results <- function(self, value)
  return(Results(self$results[self$results < value]))

#' @export
Filter_leq <- function(self, value) UseMethod("Filter_leq")
#' @export
Filter_leq.default <- function(self, value) stop("Could not perform the function")
#' @export
Filter_leq.Results <- function(self, value)
  return(Results(self$results[self$results <= value]))

#' @export
Filter_gt <- function(self, value) UseMethod("Filter_gt")
#' @export
Filter_gt.default <- function(self, value) stop("Could not perform the function")
#' @export
Filter_gt.Results <- function(self, value)
  return(Results(self$results[self$results > value]))

#' @export
Filter_geq <- function(self, value) UseMethod("Filter_geq")
#' @export
Filter_geq.default <- function(self, value) stop("Could not perform the function")
#' @export
Filter_geq.Results <- function(self, value)
  return(Results(self$results[self$results >= value]))

#------------------------------------------------------
# Count family
# The following functions return an integer indicating
# how many outcomes passed a given criterion.
#------------------------------------------------------
#' @export
Count <- function(self, value) UseMethod("Count")
#' @export
Count.default <- function(self, value) stop("Could not perform the function")

#' Counts the number of outcomes that satisfied a given criterion.
#'
#' @param fun (outcome -> bool): A function that
#' takes in an outcome and returns a
#' True / False. Only the outcomes that
#' return True will be counted.
#'
#' @return int: The number of outcomes for which
#' the function returned True.
#' @export
Count.Results <- function(self, fun = function(x) TRUE){
  return(length(Filter(self, fun)$result))
}

#' @export
Count_eq <- function(self, value) UseMethod("Count_eq")
#' @export
Count_eq.default <- function(self, value) stop("Could not perform the function")
#' @export
Count_eq.Results <- function(self, value){
  return(length(Filter_eq(self, value)$result))
}

#' @export
Count_neq <- function(self, value) UseMethod("Count_neq")
#' @export
Count_neq.default <- function(self, value) stop("Could not perform the function")
#' @export
Count_neq.Results <- function(self, value){
  return(length(Filter_neq(self, value)$result))
}

#' @export
Count_lt <- function(self, value) UseMethod("Count_lt")
#' @export
Count_lt.default <- function(self, value) stop("Could not perform the function")
#' @export
Count_lt.Results <- function(self, value){
  return(length(Filter_lt(self, value)$result))
}

#' @export
Count_leq <- function(self, value) UseMethod("Count_leq")
#' @export
Count_leq.default <- function(self, value) stop("Could not perform the function")
#' @export
Count_leq.Results <- function(self, value){
  return(length(Filter_leq(self, value)$result))
}

#' @export
Count_gt <- function(self, value) UseMethod("Count_gt")
#' @export
Count_gt.default <- function(self, value) stop("Could not perform the function")
#' @export
Count_gt.Results <- function(self, value){
  return(length(Filter_gt(self, value)$result))
}

#' @export
Count_geq <- function(self, value) UseMethod("Count_geq")
#' @export
Count_geq.default <- function(self, value) stop("Could not perform the function")
#' @export
Count_geq.Results <- function(self, value){
  return(length(Filter_geq(self, value)$result))
}

#' @export
Plot.Results <- function(self)
  stop(paste("Only simulations of random variables (RV) ",
             "can be plotted, but you simulated from a ",
             "probability space. You must first define a RV ",
             "on your probability space and simulate it. ",
             "Then call Plot() on those simulations."))

#' @export
Mean <- function(self) UseMethod("Mean")
#' @export
Mean <- function(self) stop("Could not perform the function")
#' @export
Mean.Results <- function(self)
  stop(paste("You can only call Mean() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call Mean() on those simulations."))

#' @export
Var <- function(self) UseMethod("Var")
#' @export
Var.default <- function(self) stop("Could not perform the function")
#' @export
Var.Results <- function(self)
  stop(paste("You can only call Var() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call Var() on those simulations."))

#' @export
SD <- function(self) UseMethod("SD")
#' @export
SD.default <- function(self) stop("Could not perform the function")
#' @export
SD.Results <- function(self)
  stop(paste("You can only call SD() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call SD() on those simulations."))


#---------------------------------------------------------
#             RVResults Class
#---------------------------------------------------------
#' @export
RVResults <- function(results){
  me <- list(results = results)

  class(me) <- rlist::list.append(class(me), "RVResults", "Results")
  return(me)
}

#' @export
Plot <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
                 jitter=FALSE, bins=NULL, add = FALSE)
  UseMethod("Plot")
#' @export
Plot.default <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
                         jitter=FALSE, bins=NULL, add = FALSE)
  stop("Not implemented")

#' @export
Plot.RVResults <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
                            jitter=FALSE, bins=NULL, add = FALSE){

  dim <- get_dimesion(self)

  # If RVResults is vector
  if (dim == 0){
    tb <- Tabulate(self)
    heights <- tb$Value
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
    ylab <- "Count"
    tb <- Tabulate(self, normalize = normalize)

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
          hist <- ggplot2::geom_histogram(bins = bins, fill = color(), ggplot2::aes(y=..density..), alpha = alpha)
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

      # Plot the impulses
      # graphics::Plot(x, y, type="h", col = rgb(color,alpha = alpha),
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

#' @export
Mean.RVResults <- function(self){
 if (get_dimesion(self) == 0){
   return(mean(self$results))
 } else if (get_dimesion(x) > 0){
   return(apply(self$results, 1, mean))
 } else
   stop("I don't know how to take the mean of these values.")
}

#' @export
Var.RVResults <- function(self){
  if (get_dimesion(self) == 0){
    return(var(self$results))
  } else if (get_dimesion(x) > 0){
    return(apply(self$results, 1, var))
  } else
    stop("I don't know how to take the variance of these values.")
}

#' @export
SD.RVResults <- function(self){
  if (get_dimesion(self) == 0){
    return(sd(self$results))
  } else if (get_dimesion(x) > 0){
    return(apply(self$results, 1, sd))
  } else
    stop("I don't know how to take the standard deviation of these values.")
}

