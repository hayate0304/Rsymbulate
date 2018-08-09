ProbabilitySpace <- function(drawFunc){
  me <- list(
    draw = drawFunc
  )
  
  # Set name of the class
  class(me) <- append(class(me), "ProbabilitySpace")
  return(me)
}

# draw <- function(elObjeto, newValue = 1){
#   print("Calling the base function")
#   UseMethod("draw")
# }
# 
# draw.default <- function(object, newValue){
#   return(object)
# }
# 
# draw.ProbabilitySpace <- function(elObjeto, newValue = 1){
#   print("In draw.Prob and setting the value")
#   elObjeto$draw <- newValue
#   return(elObjeto)
# }

sim <- function(self, value){
  UseMethod("sim")
}

sim.default <- function(self, value){
  return(NULL)
}

sim.ProbabilitySpace <- function(self, n){
  vect <- numeric(length(n))
  for (i in 1:n) vect[i] <- ProbabilitySpace$draw
  return(vect)
}

ArbitrarySpace <- function(drawFunc){
  me <- ProbabilitySpace(drawFunc)
  me$draw = function() 1
    
  # Add name for the class
  class(me) <- append(class(me), "ArbitrarySpace")
  return(me)
}

BoxModel <- function(){
  
}