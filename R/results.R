#
# source("utils.R")
# source("plot.R")

#---------------------------------------------------------
#             Results Class
#---------------------------------------------------------
#' @export
Results <- function(results){
  me <- list(results = results)
  class(me) <- append(class(me), "Results")

  return(me)

  # me <- results
  # class(me) <- append(class(me), "Results")
  # return(me)
}

# To print the object if standing alone without assignment
# print.Results <- function(self){
#   print(self$results)
#   #print(self)
# }

#' @export
Apply <- function(self, fun, ...) UseMethod("Apply")
#' @export
Apply.default <- function(self, fun, ...) return(NULL)
#' @export
Apply.Results <- function(self, fun, ...){
  if (get_dimesion(self) == 0){
    return(Results(sapply(self$results, fun, ...)))
  } else
    return(Results(apply(self$results, 1, fun, ...)))
}

#' @export
tabulate <- function(self, normalize = FALSE) UseMethod("tabulate")
#' @export
tabulate.default <- function(self, normalize = FALSE) return(NULL)

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

#------------------
# filter family
#------------------

#' @export
filter.Results <- function(self, fun){
  return(Results(self$results[fun(self$results)]))
}

filter_eq <- function(self, value) UseMethod("filter_eq")
filter_eq.default <- function(self, value) return(NULL)
#' @export
filter_eq.Results <- function(self, value)
  return(Results(self$results[self$results == value]))


filter_neq <- function(self, value) UseMethod("filter_neq")
filter_neq.default <- function(self, value) return(NULL)
#' @export
filter_neq.Results <- function(self, value)
  return(Results(self$results[self$results != value]))


filter_lt <- function(self, value) UseMethod("filter_lt")
filter_lt.default <- function(self, value) return(NULL)
#' @export
filter_lt.Results <- function(self, value)
  return(Results(self$results[self$results < value]))


filter_leq <- function(self, value) UseMethod("filter_leq")
filter_leq.default <- function(self, value) return(NULL)
#' @export
filter_leq.Results <- function(self, value)
  return(Results(self$results[self$results <= value]))


filter_gt <- function(self, value) UseMethod("filter_gt")
filter_gt.default <- function(self, value) return(NULL)
#' @export
filter_gt.Results <- function(self, value)
  return(Results(self$results[self$results > value]))


filter_geq <- function(self, value) UseMethod("filter_geq")
filter_geq.default <- function(self, value) return(NULL)
#' @export
filter_geq.Results <- function(self, value)
  return(Results(self$results[self$results >= value]))

#------------------
# count family
#------------------
count_eq <- function(self, value) UseMethod("count_eq")
count_eq.default <- function(self, value) return(NULL)
#' @export
count_eq.Results <- function(self, value){
  return(length(filter_eq(self, value)$result))
}

count_neq <- function(self, value) UseMethod("count_neq")
count_neq.default <- function(self, value) return(NULL)
#' @export
count_neq.Results <- function(self, value){
  return(length(filter_neq(self, value)$result))
}

count_lt <- function(self, value) UseMethod("count_lt")
count_lt.default <- function(self, value) return(NULL)
#' @export
count_lt.Results <- function(self, value){
  return(length(filter_lt(self, value)$result))
}

count_leq <- function(self, value) UseMethod("count_leq")
count_leq.default <- function(self, value) return(NULL)
#' @export
count_leq.Results <- function(self, value){
  return(length(filter_leq(self, value)$result))
}

count_gt <- function(self, value) UseMethod("count_gt")
count_gt.default <- function(self, value) return(NULL)
#' @export
count_gt.Results <- function(self, value){
  return(length(filter_gt(self, value)$result))
}

count_geq <- function(self, value) UseMethod("count_geq")
count_geq.default <- function(self, value) return(NULL)
#' @export
count_geq.Results <- function(self, value){
  return(length(filter_geq(self, value)$result))
}

#' @export
plot.Results <- function(self)
  stop(paste("Only simulations of random variables (RV) ",
             "can be plotted, but you simulated from a ",
             "probability space. You must first define a RV ",
             "on your probability space and simulate it. ",
             "Then call plot() on those simulations."))
#' @export
mean.Results <- function(self)
  stop(paste("You can only call mean() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call mean() on those simulations."))

Var <- function(self) UseMethod("Var")
Var.default <- function(self) return(NULL)
#' @export
Var.Results <- function(self)
  stop(paste("You can only call Var() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call Var() on those simulations."))

std <- function(self) UseMethod("std")
std.default <- function(self) return(NULL)
#' @export
std.Results <- function(self)
  stop(paste("You can only call std() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call std() on those simulations."))


#---------------------------------------------------------
#             RVResults Class
#---------------------------------------------------------
#' @export
RVResults <- function(results){
  me <- list(results = results)

  class(me) <- rlist::list.append(class(me), "RVResults", "Results")
  return(me)
}

plot <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
                 jitter=FALSE, bins=NULL, add = FALSE)
  UseMethod("plot")

# plot.default <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
#                          jitter=FALSE, bins=NULL, add = FALSE)
#   return(NULL)

#' @export
plot.RVResults <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
                           jitter=FALSE, bins=NULL, add = FALSE){

  dim <- get_dimesion(self)

  # If RVResults is vector
  if (dim == 0){
    tb <- tabulate(self)
    heights <- tb$Value
    discrete <- is_discrete(heights)
    #paste("Dis: ", discrete)

    if (identical(type, NULL)){
      if (discrete){
        type <- "impulse"
      } else
        type <- "hist"
    }
    if (identical(alpha, NULL))
      alpha <- 0.5
    if (identical(bins, NULL))
      bins <- 30

    color <- get_next_color()
    ylab <- "Count"

    #--------------------------------------
    ## if density in type to be implemented
    #--------------------------------------

    if (is.element("hist", type) || is.element("bar", type)){
      if (normalize)
        ylab = "Density"

      hist(self$results, breaks = bins,
           col = rgb((t(col2rgb(color)) / 255), alpha = alpha),
           freq = !normalize, ylab = ylab, xlab = "", main = "")
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
      ggplot(tb, aes(x=Outcome, xend=Outcome, y=0, yend=Value)) +
        geom_segment(color=color, alpha = alpha) +
        ylab(ylab)
    }
  }
}

#' @export
mean.RVResults <- function(self){
  if (get_dimesion(x) == 0){
    return(mean(self$results))
  } else if (get_dimesion(x) > 0){
    return(apply(self$results, 1, mean))
  } else
    stop("I don't know how to take the mean of these values.")
}

#' @export
Var.RVResults <- function(self){
  if (get_dimesion(x) == 0){
    return(var(self$results))
  } else if (get_dimesion(x) > 0){
    return(apply(self$results, 1, var))
  } else
    stop("I don't know how to take the variance of these values.")
}

#' @export
std.RVResults <- function(self){
  if (get_dimesion(x) == 0){
    return(sd(self$results))
  } else if (get_dimesion(x) > 0){
    return(apply(self$results, 1, sd))
  } else
    stop("I don't know how to take the standard deviation of these values.")
}

