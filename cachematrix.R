## Matrix inversion is usually a costly computation. This R script provides
## two functions to cache the inverse of a matrix. If the inverse of the 
## matrix is already calculated and cached, then it can be returned immediately
## without redundant costly computation.

## This function returns a special vector that contains 4 functions
## 1. set(): set the matrix 
## 2. get(): retrieve the matrix
## 3. setinv(): set the inverse of the matrix
## 4. getinv(): retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## calculate the inverse of a matrix that is a data member of the above
## defined function makeCacheMatrix. If the inverse of the matrix is 
## already calculated and cached, then it is returned immediately without
## redundant computation.
## Example of calling this function:
## ma=makeCacheMatrix(matrix(1:9,3,3))
## cacheSolve(ma)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv=x$getinv();
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
}
