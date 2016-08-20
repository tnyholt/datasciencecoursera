
# Courera Week 3 programming assignement 2
# Student: Tracey Nyholt
# The following function takes a matrix as an input and computes its inverse. Morever, 
# we use global variable declaration to cache the inverse to save on computation time
makeCacheMatrix <- function(inMat = matrix()) {
  #  set the default value of newInv to Null as we have not yet defined the newInv of our input
  newInv <- NULL
  # Create a method for the object of class makeCacheMatrix which will allow us to reset the value of 
  # inMat by calling objectName$set(newinMatrix)
  set <- function(tempMat) {
    # Set the global variable inMat to the new input tempMat (in above example, its called newInVetor)
    inMat <<- tempMat
    # if the inverse was set earlier, reset it to NULL globally as we have changed the input variable
    newInv <<- NULL
  }
  # get function which tells us what the current vector is
  get <- function() inMat
  # setInv allows us to set the global inverse value which is inInv manually
  setInv <- function(inInv) newInv <<- inInv
  # getInv allows us to read the value of the currently set global inverse variable
  getInv <- function() newInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# This function sets the inverse to the global variable newInv of a input object called inMat
# (of class defined in funtion above) if the inverse is currently not NULL, i.e. has been 
# computed and stored in a global variable, it will return the precomputed (i.e. cached)
# value, thus bypassing any computation

cacheSolve <- function(inMat) {
  # the function first imports the value stored in the object inMat
  newInv <- inMat$getInv()
  # If the value of the inverse is not NULL, it implies (by the virtue of the current design),
  # that the global value of newInv is the actual value we desire
  if(!is.null(newInv)) {
    # if the value is indeed stored, we simply fetch it
    message("getting cached data")
    return(newInv)
  }
  # otherwise, the code must extract the vector stored in the object
  data <- inMat$get()
  # and compute the inverse
  newInv <- solve(data)
  # then set the inverse using setInv function as a non-NULL value for global variable newInv
  inMat$setInv(newInv)
  # then return the said value of newInv
  newInv
}