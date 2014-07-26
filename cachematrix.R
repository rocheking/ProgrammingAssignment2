## 1. Set a framework or object (set of functions)  
## to store a matrix and some metadata about it  

makeCacheMatrix <- function(x = matrix()) {
     invMat <- NULL
     ##get and set functions for the matrix
     set <- function(y) {
          x <<- y
          invMat <<- NULL
     }
     get <- function() x
     
     ##get and set functions for the inverse of the matrix
     setsolve <- function(solvedMatrix) invMat <<- solvedMatrix
     getsolve <- function() invMat
     
     ## return... just exposes the methods - e.g. use x$getsolve() to call
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## 2. A function that can feed metadata (in this case a inverse matrix)
## into the matrix object
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
    
     ## get the inverseMatrix stored on the cached matrix object
     invMat <- x$getsolve()
     
     ## if it's null then solve() otherwise return the cached inverse matrix
     if(!is.null(invMat)) {
          message("getting cached data")
          return(invMat)
     }
     
          data <- x$get()
          invMat <- solve(data) 
          x$setsolve(invMat)
          invMat
     
}

