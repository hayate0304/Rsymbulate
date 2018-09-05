
#'  @title Defines a random variable.
#'
#'  A random variable is a function which maps an outcome of
#'  a probability space to a number.  Simulating a random
#'  variable is a two-step process: first, a draw is taken
#'  from the underlying probability space; then, the function
#'  is applied to that draw to obtain the realized value of
#'  the random variable.
#'
#' @param probSpace (ProbabilitySpace): the underlying
#' probability space of the random variable.
#' @param fun (function, optional):  a function that maps draws from the
#' probability space to numbers.
#'
#' @examples
#' # a single draw is a sequence of 0s and 1s, e.g., (0, 0, 1, 0, 1).
#' P = BoxModel(c(0, 1), size=5)
#' # X counts the number of 1s in the draw, e.g., 5
#' X = RV(P, sum)
#'
#' # the function is the identity, so Y has a Normal(0, 1) distribution
#' Y = RV(Normal(0, 1)
#'
#' # a single draw from BivariateNormal is a tuple of two numbers
#' P = BivariateNormal()
#' # Z is the smaller of the two numbers
#' Z = RV(P, min)
#'
#' @return A RV
#' @export
RV <- function(probSpace, fun = function(x) x){
  temp_p <- probSpace

  if (length(temp_p) == 3){
    if (!is.numeric(temp_p$other)){
      # Count number of probSpace intilize a list easier
      i <- 2
      while (length(temp_p) == 3 && identical(c("list", "ProbabilitySpace"), class(temp_p))){
        if (length(temp_p$self) == 3){
          temp_p = temp_p$self
        } else
          break
        i <- i + 1
      }

      temp_p <- probSpace
      #print(paste("i here: ", i))
      vect_of_probSpace <- vector("list", i)
      vect_of_rv <- vector("list", i)

      #print("Starting inside")
      len_to_input <- i
      ## If there are multiples probSpace created by %*%
      ## This is the technique to get slicing.
      ## If first probSpace$self passed in has length 3 means it was created by %*%
      while (length(temp_p) == 3 && identical(c("list", "ProbabilitySpace"), class(temp_p))){
        # print("Inside 1")
        # print("Other")
        # print(temp_p$other)

        vect_of_probSpace[[len_to_input]] <- temp_p$other
        len_to_input <- len_to_input - 1

        # print("Self")
        # print(temp_p$self)
        if (length(temp_p$self) == 3){
          # Set temp_p = temp_p$self so that the loop can continue
          # if there are more than 3 probSpace such as: Normal(0, 1) ^ 5
          #print("Next loop")
          temp_p = temp_p$self
        } else
          break

      }

      vect_of_probSpace[[len_to_input]] <- temp_p$self
      #print(paste("Print 2: ", vect_of_probSpace[[len_to_input]]))
      #print(paste("Length: ", length(vect_of_probSpace)))

      for (i in 1:length(vect_of_probSpace)){
        vect_of_rv[[i]] <- list(probSpace = vect_of_probSpace[[i]],
                                fun = fun)
        class(vect_of_rv[[i]]) <- append(class(vect_of_rv[[i]]), "RV")
      }

      vect_of_rv[["probSpace"]] <- probSpace
      vect_of_rv[["fun"]] <- fun
      class(vect_of_rv) <- append(class(vect_of_rv), "RV")
      return(vect_of_rv)
    } else {
      vect_of_probSpace <- vector("list", temp_p$other)
      vect_of_rv <- vector("list", temp_p$other)

      for (i in 1:length(vect_of_probSpace)){
        vect_of_probSpace[[i]] <- temp_p$self
        vect_of_rv[[i]] <- list(probSpace = vect_of_probSpace[[i]],
                                fun = fun)
        class(vect_of_rv[[i]]) <- append(class(vect_of_rv[[i]]), "RV")
      }

      vect_of_rv[["probSpace"]] <- probSpace
      vect_of_rv[["fun"]] <- fun
      class(vect_of_rv) <- append(class(vect_of_rv), "RV")
      return(vect_of_rv)
    }

  } else {
    me <- list(probSpace = probSpace,
               fun = fun)

    class(me) <- append(class(me), "RV")
    return(me)
  }

}

#' A function that takes no arguments and returns a single
#'   realization of the random variable.
#'
#' @examples
#'   X = RV(Normal(0, 1))
#'   X %>% draw() might return -0.9, for example.
#' @export
draw.RV <- function(self){
  # self could be a list of probSpace or just 1 probSpace
  return(self$fun(draw(self$probSpace)))
}

#'   Simulate n draws from probability space described by the random
#'   variable.
#'
#' @param n (int): How many draws to make.
#' @return Results: A vector or matrix containing the simulation results.
#' @export
sim.RV <- function(self, n){

  if (length(draw(self)) == 1){
    return(RVResults((replicate(n, draw(self)))))
  } else
    return(RVResults(t(replicate(n, draw(self)))))
}

#' @export
call <- function(self, input) UseMethod("call")
#' @export
call.default <- function(self, input) stop("Could not perform the function")

#' @export
call.RV <- function(self, input){
  cat("Warning: Calling an RV as a function simply applies the function that defines
the RV to the input, regardless of whether the input is a valid outcome in
the underlying probability space.\n")

  dummy_draw = draw(self$probSpace)

  # paste("Dummy: ", dummy_draw)
  # R doesn't have scalar type
  if (is.atomic(input)){
    if (!(is.atomic(dummy_draw) && identical(dim(input), dim(dummy_draw)))){
      stop(paste("The underlying probability space returns a Heterogeneous
                 or different dimension. A(n) ", class(input), " with dim ",
                 length(dim(input)), " was given."))
    } else if (length(input) != length(dummy_draw))
      stop("Input has wrong length")

    if (all(sapply(input, is.numeric) == sapply(dummy_draw, is.numeric))){
      return(self$fun(input))
    } else
      stop(paste("Expect a(n) ", typeof(dummy_draw), ". Was given
                 a(n) ", typeof(input), "."))


  } else if (is.list(input)){
    if (!(is.list(dummy_draw) && identical(dim(input), dim(dummy_draw))))
      stop(paste("The underlying probability space returns a Homogeneous
                 or different dimension. A(n) ", class(input), " with dim ",
                 length(dim(input)), " was given."))

    if (identical(class(dummy_draw), class(input))){
      return(self$fun(input))
    } else
      stop(paste("Expect a(n) ", typeof(dummy_draw), ". Was given
                 a(n) ", typeof(input), "."))

  }
}

check_same_probSpace.RV <- function(self, other){
  if (is_scalar(other)){
    invisible()
  } else
    check_same(self$probSpace, other$probSpace)
}

#' @export
apply <- function(self, ...) UseMethod("apply")
#' @export
apply.default <- base::apply
# add a ... argument to apply.default to allow passing of package checks:
formals(apply.default) <- c(formals(apply.default), alist(... = ))

#' Transform a random variable by a function.
#'
#' @param func: function to apply to the random variable
#'
#' @examples
#' X = RV(Exponential(1))
#' Y = X.apply(log)
#'
#' Note: For most standard functions, you can apply the function to
#' the random variable directly. For example, in the example above,
#' Y = log(X) would have been equivalent and more readable.
#'
#' User defined functions can also be applied.
#'
#' @examples
#' g <- function(x)
#'   return log(x ** 2)
#' Y = X %>% apply(g)
#' @export
apply.RV <- function(self, func, ...){
  f_new <- function(outcome)
    return(func(self$fun(outcome), ...))

  return(RV(self$probSpace, f_new))
}

#-----------------------------------------
# To be implemented later: iter, getitem
#-----------------------------------------

#----------------------------------------------
# For transforming
#----------------------------------------------
#' @export
abs.RV <- function(self)
  return(apply.RV(self, abs))

#' @export
sqrt.RV <- function(self)
  return(apply(self, sqrt))

#' @export
exp.RV <- function(self)
  return(apply(self, exp))

#' @export
sin.RV <- function(self)
  return(apply(self, sin))

#' @export
cos.RV <- function(self)
  return(apply(self, cos))

#' @export
tan.RV <- function(self)
  return(apply(self, tan))

#' @export
factorial <- function(self) UseMethod("factorial")
#' @export
factorial.default <- base::factorial
# add a ... argument to factorial.default to allow passing of package checks:
formals(factorial.default) <- c(formals(factorial.default), alist(... = ))
#' @export
factorial.RV <- function(self)
  return(apply(self, factorial))

#' @export
log.RV <- function(self, base = exp(1))
  return(apply(self, log, base))

#------------------------------------------------------------
# Operations
#------------------------------------------------------------

operation_factory <- function(self, op){

  op_fun <- function(self, other){
    check_same_probSpace(self, other)
    if (is_scalar(other)){
      return(apply(self, function(x) op(x, other)))
    } else if (inherits(other, "RV")){
      fun <- function(outcome){
        a <- self$fun(outcome)
        #print(paste("Here a: ", a))
        b <- other$fun(outcome)
        if (length(a) == length(b)){
          return(op(a, b))
        } else
          stop("Could not perform operation.")
      }

      return(RV(self$probSpace, fun))
    } else
      return("NotImplemented")
  }
}

#' @export
`%+%` <- function(self, other) UseMethod("%+%")
#' @export
`%+%.default` <- function(self, other) stop("Could not perform the operation")

# e.g., X + Y or X + 3
#' @export
`%+%.RV` <- function(self, other){
  op_fun <- operation_factory(self, function(x, y) x + y)
  return(op_fun(self, other))
}
# e.g., 3 + X
#' @export
`%+%.numeric` <- function(scalar, obj){
  return(obj %+% scalar)
}

#' @export
`%-%` <- function(self, other) UseMethod("%-%")
#' @export
`%-%.default` <- function(self, other) stop("Could not perform the operation")

# e.g., 3 - X
#' @export
`%-%.numeric` <- function(scalar, obj){
  return(-1 %*% (obj %-% scalar))
}

# e.g., X + Y or X + 3
#' @export
`%-%.RV` <- function(self, other){
  op_fun <- operation_factory(self, function(x, y) x - y)
  return(op_fun(self, other))
}

#' @export
`%/%` <- function(self, other) UseMethod("%/%")
#' @export
`%/%.default` <- function(self, other) stop("Could not perform the operation")

# e.g., 2 / X
#' @export
`%/%.numeric` <- function(scalar, obj){
  op_fun <- operation_factory(self, function(x, y) y / x)
  return(op_fun(obj, scalar))
}

# e.g., X / Y or X / 2
#' @export
`%/%.RV` <- function(self, other){
  op_fun <- operation_factory(self, function(x, y) x / y)
  return(op_fun(self, other))
}

# e.g., X * Y or X * 2
#' @export
`%*%.RV` <- function(self, other){
  op_fun <- operation_factory(self, function(x, y) x * y)
  return(op_fun(self, other))
}

# e.g., 2 * X
#' @export
`%*%.numeric` <- function(scalar, obj){
  return(obj %*% scalar)
}

# e.g., X ^ Y or X ^ 2
#' @export
`%^%.RV` <- function(self, other){
  op_fun <- operation_factory(self, function(x, y) x ^ y)
  return(op_fun(self, other))
}

#e.g., 2 ^ X
#' @export
`%^%.numeric` <- function(scalar, obj){
  op_fun <- operation_factory(self, function(x, y) y ^ x)
  return(op_fun(self, other))
}

#' @export
`%&%.RV` <- function(self, other){
  check_same_probSpace(self, other)

  if (inherits(other, "RV")){
    fun <- function(outcome) {
      a <- self$fun(outcome)
      b <- other$fun(outcome)
      return(c(a, b))
    }
    return(RV(self$probSpace, fun))
  } else
    stop("Joint distributions are only defined for RVs.")
}

#------------------------------------------------------------
## The following operations all return Events
## (Events are used to define conditional distributions)
#------------------------------------------------------------

# e.g., X < 3
#' @export
`%<%` <- function(self, other) UseMethod("%<%")
#' @export
`%<%.default` <- function(self, other) stop("Could not perform the operation")
#' @export
`%<%.RV` <- function(self, other){
  if (is_scalar(other)){
    return(Event(self$probSpace,
                 function(x) self$fun(x) < other))
  } else if (inherits(other, "RV")){
    return(Event(self$probSpace,
                 function(x) self$fun(x) < other$fun(x)))
  } else
    stop("NotImplementedError")
}

# e.g., X <= 3
#' @export
`%<=%` <- function(self, other) UseMethod("%<=%")
#' @export
`%<=%.default` <- function(self, other) stop("Could not perform the operation")
#' @export
`%<=%.RV` <- function(self, other){
  if (is_scalar(other)){
    return(Event(self$probSpace,
                 function(x) self$fun(x) <= other))
  } else if (inherits(other, "RV")){
    return(Event(self$probSpace,
                 function(x) self$fun(x) <= other$fun(x)))
  } else
    stop("NotImplementedError")
}

# e.g., X > 3
#' @export
`%>>%` <- function(self, other) UseMethod("%>>%")
#' @export
`%>>%.default` <- function(self, other) stop("Could not perform the operation")
#' @export
`%>>%.RV` <- function(self, other){
  if (is_scalar(other)){
    return(Event(self$probSpace,
                 function(x) self$fun(x) > other))
  } else if (inherits(other, "RV")){
    return(Event(self$probSpace,
                 function(x) self$fun(x) > other$fun(x)))
  } else
    stop("NotImplementedError")
}

# e.g., X >= 3
#' @export
`%>=%` <- function(self, other) UseMethod("%>=%")
#' @export
`%>=%.default` <- function(self, other) stop("Could not perform the operation")
#' @export
`%>=%.RV` <- function(self, other){
  if (is_scalar(other)){
    return(Event(self$probSpace,
                 function(x) self$fun(x) >= other))
  } else if (inherits(other, "RV")){
    return(Event(self$probSpace,
                 function(x) self$fun(x) >= other$fun(x)))
  } else
    stop("NotImplementedError")
}

# e.g., X == 3
#' @export
`%==%` <- function(self, other) UseMethod("%==%")
#' @export
`%==%.default` <- function(self, other) stop("Could not perform the operation")
#' @export
`%==%.RV` <- function(self, other){
  if (is_scalar(other)){
    return(Event(self$probSpace,
                 function(x) self$fun(x) == other))
  } else if (inherits(other, "RV")){
    return(Event(self$probSpace,
                 function(x) self$fun(x) == other$fun(x)))
  } else
    stop("NotImplementedError")
}

# e.g., X != 3
#' @export
`%!=%` <- function(self, other) UseMethod("%!=%")
#' @export
`%!=%.default` <- function(self, other) stop("Could not perform the operation")
#' @export
`%!=%.RV` <- function(self, other){
  if (is_scalar(other)){
    return(Event(self$probSpace,
                 function(x) self$fun(x) != other))
  } else if (inherits(other, "RV")){
    return(Event(self$probSpace,
                 function(x) self$fun(x) != other$fun(x)))
  } else
    stop("NotImplementedError")
}

# Define conditional distribution of random variable.
# e.g., X | (X > 3)
#' @export
`%|%` <- function(self, condition_event) UseMethod("%|%")
#' @export
`%|%.default` <- function(self, condition_event) stop("Could not perform the operation")
#' @export
`%|%.RV` <- function(self, condition_event){
  check_same_probSpace(self, condition_event)

  if (inherits(condition_event, "Event")){
    return(RVConditional(self, condition_event))
  } else
    stop("NotImplementedError")
}

#' @title Defines a random variable conditional on an event.
#'
#' @description RVConditionals are typically produced when you condition a
#' RV on an Event object.
#'
#' @param random_variable (RV): the random variable whose conditional
#' distribution is desired
#' @param condition_event (Event): the event to condition on
#'
#' @examples
#' X, Y = RV(Binomial(10, 0.4) ** 2)
#' (X | (X + Y == 5)) %>% draw() # returns a value between 0 and 5.

RVConditional <- function(RV){
  attribute <- list(probSpace = RV$probSpace,
                    fun = RV$fun,
                    condition_event = condition_event)
  class(attribute) <- c(class(attribute), "RVConditional", "RV")
  return(attribute)
}

#' A function that takes no arguments and returns a value from
#' the conditional distribution of the random variable.
#'
#' @examples
#' X, Y = RV(Binomial(10, 0.4) ** 2)
#' (X | (X + Y == 5)).draw() might return a value of 4, for example.
draw.RVConditional <- function(self) {
  probSpace <- self$probSpace
  while (TRUE){
    outcome <- draw(probSpace)
    if ((self$condition_event)$fun(outcome))
      return(self$fun(outcome))
  }
}
