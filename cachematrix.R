## Assignment: Caching Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Two functions created.
## 1. makeCacheMatrix
## 2. cacheSolve



##This function creates a "matrix" object that can cache its inverse.
## contains 4 member functions- set,get,setInvMatrix, getInvMatrix
## As instructed it uses <<- assignment operator so that internal variables are not exposed to outside environment
## --------- makeCacheMatrix function starts here----------------------
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
## --------- makeCacheMatrix function ends here----------------------



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## --------- cacheSolve function starts here----------------------
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
## --------- cacheSolve function ends here----------------------



##----------------Testing my functions----------------------------------

my_matrx <- matrix((1:4),2,2)

cachdMatrix <- makeCacheMatrix(my_matrx)

cachdMatrix$get()

cacheSolve(cachdMatrix)