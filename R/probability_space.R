#---------------------------------------------------------------
# ProbabilitySpace class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Defines a probability space.
#'
#' @param draw function: A function explaining how to draw one
#' outcome from the probability space.
#' @return ProbabilitySpace
#' @export
ProbabilitySpace <- function(DrawFunc, self = NULL, other = NULL){
  # Have self and other to store ProbabilitySpaces when doing operations
  attribute <- list(
    draw = DrawFunc,
    self = self,
    other = other
  )

  # Set name of the class
  class(attribute) <- append(class(attribute), "ProbabilitySpace")
  return(attribute)
}

#' @export
draw <- function(self)  UseMethod("draw")
#' @export
draw.default <- function(self)  stop("Could not perform the function on this object")

#' @export
draw.ProbabilitySpace <- function(self){
#  print("In PS")
  return(self$draw())
}

#' @export
sim <- function(self, n)  UseMethod("sim")
#' @export
sim.default <- function(self, n)  stop("Could not perform the function on this object")

#' Simulate n draws from probability space.
#'
#' @param n (int): How many draws to make.
#' @return A vector containing the Simulation results.
#' @export
sim.ProbabilitySpace <- function(self, n){
  # Vector
  if (length(draw(self)) == 1){
    return(Results(replicate(n, draw(self))))
  } else
    return(Results(t(replicate(n, draw(self))))) #Matrix
}

check_same <- function(self, other)  UseMethod("check_same")
check_same.default <- function(self, other)  stop("Could not perform the function")
check_same.ProbabilitySpace <- function(self, other){
  # print(self)
  # print(other)
  if (inherits(other, "ArbitrarySpace")){
    invisible()
  } else if (inherits(self, "S") && inherits(other, "S")){
    invisible()
  } else if (!identical(self, other))
    stop("Events must be defined on same probability space.")
}

#' @export
`*.ProbabilitySpace` <- function(self, other){
  # If I change this function's name to "draw" then R will be aborted since
  # the function's name mess up with other method "draw".
  dr <- function(){
    return(c(draw(self), draw(other)))
  }
  return(ProbabilitySpace(dr, self, other))
}

#' @export
`^.ProbabilitySpace` <- function(self, exponent){
  # 'infinite' is not tested thoroughly
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
      return(replicate(exponent, draw(self)))
    }
  }
  return(ProbabilitySpace(dr, self, exponent))
}

#---------------------------------------------------------------
# ArbitrarySpace class
#---------------------------------------------------------------
ArbitrarySpace <- function(){
  attribute <- list(
    draw = function() 1
  )

  # Add name for the class
  class(attribute) <- c(class(attribute), "ArbitrarySpace", "ProbabilitySpace")
  return(attribute)
}

#---------------------------------------------------------------
# Event class
#---------------------------------------------------------------
Event <- function(probSpace, fun){
  attribute <- list(probSpace = probSpace,
             fun = fun)

  class(attribute) <- append(class(attribute), "Event")
  return(attribute)
}

check_same_probSpace <- function(self, other)
  UseMethod("check_same_probSpace")

check_same_probSpace.default <- function(self, other)
  return(NULL)

check_same_probSpace.Event <- function(self, other){
  check_same(self$probSpace, other$probSpace)
}

#' @export
`&` <- function(self, other) UseMethod("&", self)

# define the event (A & B)
#' @export
`&.Event` <- function(self, other){
  check_same_probSpace(self, other)

  if (inherits(other, "Event")){
    return(Event(self$probSpace, function(x) self$fun(x) && other$fun(x)))
  }
}

# define the event (A | B)
# '|' is generic in R already but I need to specificly tell R that
# '|' dispatch on the first argument. Otherwise, it would dispatch
# both sides. And this would conflict with something like (X | (X == 3),
# when both the RV X and the Event (X == 3) has '|' methods.
#' @export
`|` <- function(self, other) UseMethod("|", self)

#' @export
`|.Event` <- function(self, other){
  check_same_probSpace(self, other)

  if (inherits(other, "Event")){
    return(Event(self$probSpace, function(x) self$fun(x) || other$fun(x)))
  }
}

# define the event (-A)
# '~' is not a unary operator in R. Thus, I replaced by "!"
#' @export
`!.Event` <- function(self){
  return(Event(self$probSpace, function(x) !self$fun(x)))
}

#' @export
draw.Event <- function(self){
  return(self$fun(draw(self$probSpace)))
}

#' @export
sim.Event <- function(self, n){
  if (length(draw(self)) == 1){
    return(Results(replicate(n, draw(self))))
  } else
    return(Results(t(replicate(n, draw(self)))))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BoxModel class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Defines a probability space from a box model.
#'
#' @param box (vector or list): The box to sample from.
#' The box can be specified either directly as a list
#' of objects or indirectly as a dict of objects and
#' their counts.
#' @param size (int): How many draws to make.
#' @param replace (bool): Sample with replacement or without?
#' @param probs (vector): Probabilities of sampling each ticket
#' (by default, all tickets are equally likely). Note
#' that this is ignored if box is specified as a list.
#' @param order_matters (bool): Should we count different
#' orderings of the same tickets as different outcomes?
#' Essentially, this determines whether the draws are
#' sorted before returning or not.
#' @return A BoxModel
#' @export
BoxModel <- function(box, size = 1, replace = TRUE,
                     probs = NULL, order_matters = TRUE){

  if (is.atomic(box)){
    self.box = box

    # List is used in replace of Python's dictionary
    # Look at my first tutorial for usage
  } else if (is.list(box)){
      self.box = unlist(lapply(
        seq_along(box), function(x) rep(names(box)[x], box[[x]])))

      tryCatch(
        self.box <- as.integer(self.box),
        warning = function(c) invisible()
      )
  } else
      stop("Box must be specified either as a vector or a list.")

  attribute <- list(box = self.box,
             size = size,
             replace = replace,
             probs = probs,
             order_matters = order_matters)

  ## Add name for the class
  class(attribute) <- c(class(attribute), "BoxModel", "ProbabilitySpace")
  return(attribute)
}

#' A function that takes no arguments and returns a value(s) from the
#' "box" argument of the BoxModel.
#'
#' Based on BoxModel inputs:
#' Number of values returned depends on the input of the "size"
#' argument.
#' Whether or not a value in the box can appear multiple times
#' depends on the "replace" argument.
#' If a list of probabilities is specified, values drawn will be drawn
#' with the specified probabilities.
#' @export
draw.BoxModel <- function(self){
  Draw_inds <- function(size){
    return(sample(length(self$box), size, self$replace, self$prob))
  }

  # 'infinitite' is not tested thoroughly
  if (self$size == 1){
    return(self$box[Draw_inds(1)])
  } else if (is.infinite(self$size)) {

    if (self$replace == FALSE)
      stop("Cannot draw an infinite number of tickets
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
#---------------------------------------------------------------
#' Defines the probability space for drawing from a deck of cards.
#'
#' @param size (int): How many draws to make.
#' @param replace (bool): Sample with replacement or without?
#' @export
DeckOfCards <- function(size = 1, replace = FALSE, order_matters = TRUE){
  deck <- paste(rep(c(2:10, "J", "Q", "K", "A"), 4),  #card values
                rep(c("Diamonds", "Hearts", "Clubs", "Spades"), each = 13)) #asuits

  attribute <- list(box = deck,
             size = size,
             replace = replace,
             probs = NULL,
             order_matters = order_matters)

  class(attribute) <- c(class(attribute), "DeckOfCards", "BoxModel", "ProbabilitySpace")
  return(attribute)
}

