
setwd("C:/Users/lucki/Desktop/Frost Research/Rsymbulate")
source("utils.R")
source("plot.R")

#---------------------------------------------------------
#             Results Class
#---------------------------------------------------------
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

Apply <- function(self, fun) UseMethod("Apply")
Apply.default <- function(self, fun) return(NULL)

Apply.Results <- function(self, fun){
  return(Results(apply(self$results, 1, fun)))
}

tabulate <- function(self, normalize = FALSE) UseMethod("tabulate")
tabulate.default <- function(self, normalize = FALSE) return(NULL)

tabulate.Results <- function(self, normalize = FALSE){
  if (normalize) {
    return(round(table(self$results) / length(self$results), 5))
  } else
    return(round(table(self$results), 5))
  
}

#------------------
# filter family
#------------------
filter_eq <- function(self, value) UseMethod("filter_eq")
filter_eq.default <- function(self, value) return(NULL)
filter_eq.Results <- function(self, value)
  return(Results(self$results[self$results == value]))


filter_neq <- function(self, value) UseMethod("filter_neq")
filter_neq.default <- function(self, value) return(NULL)
filter_neq.Results <- function(self, value)
  return(Results(self$results[self$results != value]))


filter_lt <- function(self, value) UseMethod("filter_lt")
filter_lt.default <- function(self, value) return(NULL)
filter_lt.Results <- function(self, value)
  return(Results(self$results[self$results < value]))


filter_leq <- function(self, value) UseMethod("filter_leq")
filter_leq.default <- function(self, value) return(NULL)
filter_leq.Results <- function(self, value)
  return(Results(self$results[self$results <= value]))


filter_gt <- function(self, value) UseMethod("filter_gt")
filter_gt.default <- function(self, value) return(NULL)
filter_gt.Results <- function(self, value)
  return(Results(self$results[self$results > value]))


filter_geq <- function(self, value) UseMethod("filter_geq")
filter_geq.default <- function(self, value) return(NULL)
filter_geq.Results <- function(self, value)
  return(Results(self$results[self$results >= value]))

#------------------
# count family
#------------------
count_eq <- function(self, value) UseMethod("count_eq")
count_eq.default <- function(self, value) return(NULL)
count_eq.Results <- function(self, value){
  return(length(filter_eq(self, value)$result))
}

count_neq <- function(self, value) UseMethod("count_neq")
count_neq.default <- function(self, value) return(NULL)
count_neq.Results <- function(self, value){
  return(length(filter_neq(self, value)$result))
}

count_lt <- function(self, value) UseMethod("count_lt")
count_lt.default <- function(self, value) return(NULL)
count_lt.Results <- function(self, value){
  return(length(filter_lt(self, value)$result))
}

count_leq <- function(self, value) UseMethod("count_leq")
count_leq.default <- function(self, value) return(NULL)
count_leq.Results <- function(self, value){
  return(length(filter_leq(self, value)$result))
}

count_gt <- function(self, value) UseMethod("count_gt")
count_gt.default <- function(self, value) return(NULL)
count_gt.Results <- function(self, value){
  return(length(filter_gt(self, value)$result))
}

count_geq <- function(self, value) UseMethod("count_geq")
count_geq.default <- function(self, value) return(NULL)
count_geq.Results <- function(self, value){
  return(length(filter_geq(self, value)$result))
}

plot.Results <- function(self) 
  stop(paste("Only simulations of random variables (RV) ",
             "can be plotted, but you simulated from a ",
             "probability space. You must first define a RV ",
             "on your probability space and simulate it. ",
             "Then call plot() on those simulations."))

mean.Results <- function(self)
  stop(paste("You can only call mean() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call mean() on those simulations."))

Var <- function(self) UseMethod("Var")
Var.default <- function(self) return(NULL)
Var.Results <- function(self)
  stop(paste("You can only call Var() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call Var() on those simulations."))

std <- function(self) UseMethod("std")
std.default <- function(self) return(NULL)
std.Results <- function(self)
  stop(paste("You can only call std() on simulations of ",
             "random variables (RV), but you simulated from ",
             "a probability space. You must first define ",
             "a RV on your probability space and simulate it ",
             "Then call std() on those simulations."))


#---------------------------------------------------------
#             RVResults Class
#---------------------------------------------------------
RVResults <- function(results){
  me <- list(results = results)
  
  class(me) <- list.append(class(me), "RVResults", "Results")
  return(me)
}

plot.RVResults <- function(self, type=NULL, alpha=NULL, normalize=TRUE,
                            jitter=FALSE, bins=NULL, add = FALSE){
  
  dim <- get_dimesion(self)
  
  # If RVResults is vector
  if (dim == 0){
    heights <- as.vector(tabulate(self))
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
    
    #--------------------------------------
    ## if density in type to be implemented
    #--------------------------------------
    
    if (is.element("hist", type) || is.element("bar", type)){
      hist(self$results, breaks = bins, 
           col = rgb(color(), alpha = alpha),
           freq = !normalize)
      if (normalize){
        title(ylab = "Relative Frequency")
      } else 
        title(ylab = "Count")
    }
    else if (is.element("impulse", type)){
      x <- as.double(names(tabulate(self)))
      y <- as.vector(tabulate(self))
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
      #print(a)
      print(y)
      
      # plot the impulses
      vlines(x, y, color, alpha, normalize, add)
      if (normalize){
        title(ylab = "Relative Frequency")
      } else 
        title(ylab = "Count") 
        
    }
  }
}

mean.RVResults <- function(self){
 if (get_dimesion(x) == 0){
   return(mean(self$results))
 } else if (get_dimesion(x) > 0){
   return(apply(self$results, 1, mean))
 } else 
   stop("I don't know how to take the mean of these values.")
}

Var.RVResults <- function(self){
  if (get_dimesion(x) == 0){
    return(var(self$results))
  } else if (get_dimesion(x) > 0){
    return(apply(self$results, 1, var))
  } else 
    stop("I don't know how to take the variance of these values.")
}

std.RVResults <- function(self){
  if (get_dimesion(x) == 0){
    return(sd(self$results))
  } else if (get_dimesion(x) > 0){
    return(apply(self$results, 1, sd))
  } else 
    stop("I don't know how to take the standard deviation of these values.")
}

