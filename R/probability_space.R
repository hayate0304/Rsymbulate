# source("seed.R")
# source("sequences.R")
# library(rlist)
# library(magrittr)

#---------------------------------------------------------------
# ProbabilitySpace class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Defines a probability space.
#'
#' @param Draw function
#' @return ProbabilitySpace
#' @export
ProbabilitySpace <- function(DrawFunc){
  me <- list(
    Draw = DrawFunc
  )

  # Set name of the class
  class(me) <- append(class(me), "ProbabilitySpace")
  return(me)
}

#' @export
Draw <- function(self)  UseMethod("Draw")
#' @export
Draw.default <- function(self)  stop("Could not perform the function on this object")

#' @export
Draw.ProbabilitySpace <- function(self){
#  print("In PS")
  return(self$Draw())
}

#' @export
Sim <- function(self, n)  UseMethod("Sim")
#' @export
Sim.default <- function(self, n)  return(NULL)

#' Simulate n Draws from probability space.
#'
#' @param n (int): How many Draws to make.
#' @return A vector containing the Simulation results.
#' @export
Sim.ProbabilitySpace <- function(self, n){
  if (length(Draw(self)) == 1){
    return(Results(replicate(n, Draw(self))))
  } else
    return(Results(t(replicate(n, Draw(self)))))
}

check_same <- function(self, other)  UseMethod("check_same")
check_same.default <- function(self, other)  return(NULL)
check_same.ProbabilitySpace <- function(self, other){
  if (inherits(other, "ArbitrarySpace")){
    invisible()
  } else if (!identical(self, other))
    stop("Events must be defined on same probability space.")
}


#' @export
`%*%` <- function(self, other) UseMethod("%*%")
#' @export
`%*%.default` <- function(self, other) return(NULL)

#' @export
`%*%.ProbabilitySpace` <- function(self, other){

  # Create function "dr" to pass to ProbabilitySpace. If I change this function's name
  # to "Draw" then R will be aborted since the function's name mess up with the method
  # "Draw" in BoxModel.
  dr <- function(){
    return(c(Draw(self), Draw(other)))
  }
  return(ProbabilitySpace(dr))
}

#' @export
`%^%` <- function(self, exponent)  UseMethod("%^%")
#' @export
`%^%.default` <- function(self, exponent)  return(NULL)

#' @export
`%^%.ProbabilitySpace` <- function(self, exponent){
  if (is.infinite(exponent)){
    dr <- function(){
      seed <- get_seed()

      x <- function(n){
        set.seed(seed)
        replicate(as.integer(n), Draw(self))

        return(Draw(self))
      }
      return(InfiniteSequence(x))
    }
  } else {

    dr <- function(){
      return(replicate(exponent, Draw(self)))
    }
    
  }
  return(ProbabilitySpace(dr))
}

#---------------------------------------------------------------
# ArbitrarySpace class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ArbitrarySpace <- function(){
  me <- list(
    Draw = function() 1
  )

  # Add name for the class
  class(me) <- rlist::list.append(class(me), "ProbabilitySpace", "ArbitrarySpace")
  return(me)
}

#---------------------------------------------------------------
# BoxModel class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Defines a probability space from a box model.
#'
#' @param box, size, replace, probs, order_matters
#' @return A BoxModel
#' @export
BoxModel <- function(box, size = 1, replace = TRUE,
                     probs = NULL, order_matters = TRUE){

  if (is.atomic(box)){
    self.box = box

  } else if (is.list(box)){
      self.box = unlist(lapply(
        seq_along(box), function(x) rep(names(box)[x], box[[x]])))
      #print(self.box)

      tryCatch(
        self.box <- as.integer(self.box),
        warning = function(c) invisible()
      )

      #print(self.box)
  } else
      stop("Box must be specified either as a vector or a list.")

  me <- list(box = self.box,
             size = size,
             replace = replace,
             probs = probs,
             order_matters = order_matters)

  ## Add name for the class
  class(me) <- rlist::list.append(class(me), "BoxModel", "ProbabilitySpace")
  return(me)
}

#' @export
Draw.BoxModel <- function(self){

  Draw_inds <- function(size){
    return(sample(length(self$box), size, self$replace, self$prob))
  }

  if (self$size == 1){
    return(self$box[Draw_inds(1)])
  } else if (is.infinite(self$size)) {
#    warning("BoxModel with size = Inf will be implemented later")

    if (self$replace == FALSE)
      stop("Cannot Draw an infinite number of tickets
           without replacement")

    seed <- get_seed()

    x <- function(n){
      set.seed(seed)
      replicate(as.integer(n), Draw_inds(1))

      return(self$box[Draw_inds(1)])
    }

    return(getitem(InfiniteSequence(x), n))
  } else {
    Draws = self$box[Draw_inds(self$size)]
    if (!self$order_matters)
      Draws = sort(Draws)

    return(Draws)
  }
}

#---------------------------------------------------------------
# DeckOfCards class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
DeckOfCards <- function(size = 1, replace = FALSE, order_matters = TRUE){
  deck <- paste(rep(c(2:10, "J", "Q", "K", "A"), 4),  #card values
                rep(c("Diamonds", "Hearts", "Clubs", "Spades"), each = 13)) #asuits

  me <- list(box = deck,
             size = size,
             replace = replace,
             probs = NULL,
             order_matters = order_matters)

  class(me) <- rlist::list.append(class(me), "DeckOfCards", "BoxModel", "ProbabilitySpace")
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

#' @export
`%&%` <- function(self, other) UseMethod("%&%")
#' @export
`%&%.default` <- function(self, other) stop("Could not perform the operation")

# define the event (A & B)
#' @export
`%&%.Event` <- function(self, other){
  check_same_probSpace(self, other)

  if (inherits(other, "Event")){
    return(Event(self$probSpace, function(x) self$fun(x) & other$fun(x)))
  }
}

# define the event (A | B)
#' @export
`%|%` <- function(self, other) UseMethod("%|%")
#' @export
`%|%.default` <- function(self, other) return(NULL)

#' @export
`%|%.Event` <- function(self, other){
  check_same_probSpace(self, other)

  if (inherits(other, "Event")){
    return(Event(self$probSpace, function(x) self$fun(x) | other$fun(x)))
  }
}

# define the event (-A)
#' @export
`%!%` <- function(self) UseMethod("%|%")
#' @export
`%!%.default` <- function(self) return(NULL)

#' @export
`%!%.Event` <- function(self){
  return(Event(self$probSpace, function(x) !self$fun()))
}

# This prevents users from writing expressions like 2 < X < 5,
# which evaluate to ((2 < X) and (X < 5)).
# __bool__ : implement later when I got to test all above.
#' @export
Draw.Event <- function(self){
  return(self$fun(Draw(self$probSpace)))
}
