## This function creates a list that contains 4 member functions- set,get,setInvMatrix, getInvMatrix
## As instructed it uses <<- assignment operator so that internal variables are not exposed to outside environment


makeCacheMatrix <- function(x = matrix()) {
  
  ##code starts here
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m
  list(set=set, get=get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
}

## Following function returns a matrix that inverse of X.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInvMatrix() ##get inversed matrix from object X
  
  ## check if inveresion result is there. if result is there then return the inversion
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## if inversion result not then we need to get the matrix object, solve it and set it. Then return the object.
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInvMatrix(m)
  m
}

##Testing my functions

my_matrx <- matrix((1:4),2,2)

cachdMatrix <- makeCacheMatrix(my_matrx)

cachdMatrix$get()

cacheSolve(cachdMatrix)