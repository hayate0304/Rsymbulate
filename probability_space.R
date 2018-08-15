#setwd("C:/Users/lucki/Desktop/Frost Research/Rsymbulate")
setwd("D:/Desktop/Frost Research/Rsymbulate")
source("seed.R")
source("sequences.R")
library(rlist)
library(magrittr)


#---------------------------------------------------------------
# ProbabilitySpace class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ProbabilitySpace <- function(drawFunc){
  me <- list(
    draw = drawFunc
  )
  
  # Set name of the class
  class(me) <- append(class(me), "ProbabilitySpace")
  return(me)
}

draw <- function(self)  UseMethod("draw")

draw.default <- function(self){
  print("In default")
  return(NULL)
}

draw.ProbabilitySpace <- function(self){
#  print("In PS")
  return(self$draw())
}

sim <- function(self, n)  UseMethod("sim")

sim.default <- function(self, n)  return(NULL)

sim.ProbabilitySpace <- function(self, n){
  # vect <- numeric(length(n))
  # for (i in 1:n) vect[i] <- draw(self)
  return(t(replicate(n, draw(self))))
}

check_same <- function(self, other)  UseMethod("check_same")

check_same.default <- function(self, other)  return(NULL)

check_same.ProbabilitySpace <- function(self, other){
  if (inherits(other, "ArbitrarySpace")){
    invisible()
  } else if (!identical(self, other))
    stop("Events must be defined on same probability space.")
}

# `%*%` <- function(self, other){
#   UseMethod("%*%")
# }
# 
# `%*%.default` <- function(self, other){
#   return(NULL)
# }

`%*%.ProbabilitySpace` <- function(self, other){
  
  # Create function "dr" to pass to ProbabilitySpace. If I change this function's name 
  # to "draw" then R will be aborted since the function's name mess up with the method
  # "draw" in BoxModel.
  dr <- function(){
    return(c(draw(self), draw(other)))
  }
  return(ProbabilitySpace(dr))
}

`%**%` <- function(self, exponent)  UseMethod("%**%")

`%**%.default` <- function(self, exponent)  return(NULL)

`%**%.ProbabilitySpace` <- function(self, exponent){
  if (is.infinite(exponent)){
    dr <- function(){
      seed <- get_seed()
      
      x <- function(n){
        set.seed(seed)
        replicate(as.integer(n), draw(self))
        
        return(draw(self))
      }
      return(InfiniteSequence(x))
    }
  } else {
    dr <- function(){
      # Another way to store this is using list
      # tuple = list()
      # for (i in 1:exponent){
      #   tuple = list.append(tuple, draw(self))
      # }
      
      return(t(replicate(exponent, draw(self))))
    }
  }
  return(ProbabilitySpace(dr))
}

#---------------------------------------------------------------
# ArbitrarySpace class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Define inheritance classes
ArbitrarySpace <- function(){
  me <- list(
    draw = function() 1
  )

  # Add name for the class
  class(me) <- list.append(class(me), "ProbabilitySpace", "ArbitrarySpace")
  return(me)
}

#---------------------------------------------------------------
# BoxModel class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BoxModel <- function(box, size = 1, replace = TRUE, 
                     probs = NULL, order_matters = TRUE){

# size = NULL just to have similar behavior with Python
# but default is 1
  
  if (is.atomic(box)){
    self.box = box

  } else if (is.list(box)){
      self.box = unlist(lapply(
        seq_along(box), function(x) rep(names(box)[x], box[[x]])))
  } else 
      stop("Box must be specified either as a vector or a list.")
  
  me <- list(box = self.box,
             size = size,
             replace = replace,
             probs = probs,
             order_matters = order_matters)
             
  ## Add name for the class
  class(me) <- list.append(class(me), "BoxModel", "ProbabilitySpace")
  return(me)
}

# draw <- function(self){
#   UseMethod("draw")
# }
# 
# draw.default <- function(self){
#   return(NULL)
# }

draw.BoxModel <- function(self){

  draw_inds <- function(size){
    return(sample(length(self$box), size, self$replace, self$prob)) 
  }
  
  if (self$size == 1){
    return(self$box[draw_inds(1)])
  } else if (is.infinite(self$size)) {
#    warning("BoxModel with size = Inf will be implemented later")
    
    if (self$replace == FALSE)
      stop("Cannot draw an infinite number of tickets
           without replacement")

    seed <- get_seed()
    
    x <- function(n){
      set.seed(seed)
      replicate(as.integer(n), draw_inds(1))

      return(self$box[draw_inds(1)])
    }

    return(getitem(InfiniteSequence(x), n))
  } else {
    draws = self$box[draw_inds(self$size)]
    if (!self$order_matters)
      draws = sort(draws)
    
    return(draws)
  }
  
}

#---------------------------------------------------------------
# DeckOfCards class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DeckOfCards <- function(size = 1, replace = FALSE, order_matters = TRUE){
  deck <- paste(rep(c(2:10, "J", "Q", "K", "A"), 4),  #card values
                rep(c("Diamonds", "Hearts", "Clubs", "Spades"), each = 13)) #suits
  
  me <- list(box = deck,
             size = size,
             replace = replace,
             probs = NULL,
             order_matters = order_matters)
  
  class(me) <- list.append(class(me), "DeckOfCards", "BoxModel", "ProbabilitySpace")
  return(me)
}

#---------------------------------------------------------------
# Event class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Event <- function(probSpace, fun){
  me <- list(probSpace = probSpace,
             fun = fun)
  
  class(me) <- append(class(me), "Event")
  return(me)
}

check_same_probSpace <- function(self, other)
  UseMethod("check_same_probSpace")

check_same_probSpace.default <- function(self, other)
  return(NULL)

check_same_probSpace.Event <- function(self, other){
  check_same(self$probSpace, other$probSpace)
}

`%&%` <- function(self, other) UseMethod("%&%")
  
`%&%.default` <- function(self, other) return(NULL)

# define the event (A & B)
`%&%.Event` <- function(self, other){
  check_same_probSpace(self, other)
  
  if (inherits(other, "Event")){
    return(Event(self$probSpace, function(x) self$fun(x) & other$fun(x)))
  }
}

# define the event (A | B)
`%|%` <- function(self, other) UseMethod("%|%")

`%|%.default` <- function(self, other) return(NULL)

`%|%.Event` <- function(self, other){
  check_same_probSpace(self, other)
  
  if (inherits(other, "Event")){
    return(Event(self$probSpace, function(x) self$fun(x) | other$fun(x)))
  }
}

# define the event (-A)
`%!%` <- function(self) UseMethod("%|%")

`%!%.default` <- function(self) return(NULL)

`%!%.Event` <- function(self){
  return(Event(self$probSpace, function(x) !self$fun()))
}

# This prevents users from writing expressions like 2 < X < 5,
# which evaluate to ((2 < X) and (X < 5)).
# __bool__ : implement later when I got to test all above.

draw.Event <- function(self){
  return(self$fun(draw(self$probSpace)))
}

# sim <- function(self, n)
#   return