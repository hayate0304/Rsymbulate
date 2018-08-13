setwd("C:/Users/lucki/Desktop/Frost Research/Rsymbulate")
source("seed.R")
source("sequences.R")
library(rlist)

ProbabilitySpace <- function(drawFunc){
  me <- list(
    draw = drawFunc
  )
  
  # Set name of the class
  class(me) <- append(class(me), "ProbabilitySpace")
  return(me)
}

draw <- function(self){
  UseMethod("draw")
}

draw.default <- function(self){
  print("In default")
  return(NULL)
}

draw.ProbabilitySpace <- function(self){
#  print("In PS")
  return(self$draw())
}

sim <- function(self, n){
  UseMethod("sim")
}

sim.default <- function(self, n)
  return(NULL)


sim.ProbabilitySpace <- function(self, n){
  vect <- numeric(length(n))
  for (i in 1:n) vect[i] <- draw(self)
  return(vect)
}

check_same <- function(self, other){
  UseMethod("check_same")
}

check_same.default <- function(self, other)
  return(NULL)


check_same.ProbabilitySpace <- function(self, other){
  # if (inherits(other, "ArbitrarySpace")){
  #   return()
  # } else 
  if (!identical(self, other))
    stop("Events must be defined on same probability space.")
}

`*` <- function(self, other){
  UseMethod("*")
}

`*.default` <- function(self, other){
  return(NULL)
}

`*.ProbabilitySpace` <- function(self, other){
  
  # Create function "dr" to pass to ProbabilitySpace. If I change this function's name 
  # to "draw" then R will be aborted since the function's name mess up with the method
  # "draw" in BoxModel.
  dr <- function(self, other){
    return(c(draw(self), draw(other)))
  }
  return(ProbabilitySpace(dr))
}

`**` <- function(self, exponent){
  UseMethod("**")
}

`**` <- function(self, exponent){
  return(NULL)
}

`**` <- function(self, exponent){
  if (is.infinite(exponent)){
    dr <- function(){
      seed <- get_seed()
      
      x <- function(n){
        set.seed(seed)
        replicate(as.integer(n), draw_inds(1))
        
        return(self$box[draw_inds(1)])
      }
    }
  }
}

#--------------------------------------------------
### Define inheritance classes
ArbitrarySpace <- function(){
  me <- list(
    draw = function() 1
  )

  # Add name for the class
  class(me) <- list.append(class(me), "ProbabilitySpace", "ArbitrarySpace")
  return(me)
}

#--------------------------------------------------------
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

Event <- function(probSpace, fun){
  me <- list(probSpace = probSpace,
             fun = fun)
  
  class(me) <- append(class(me), "Event")
  return(me)
}

check_same_probSpace <- function(self, other){
  UseMethod("check_same_probSpace")
}

check_same_probSpace.default <- function(self, other){
  return(NULL)
}

check_same_probSpace.Event <- function(self, other){
  check_same(self$probSpace, other$probSpace)
}






