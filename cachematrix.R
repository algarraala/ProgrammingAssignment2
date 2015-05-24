## The objective of thi assingment is to create two functions: the first one 
## creates an "cached" object containing the matrix and its inverse, the second
## one retrieves the inverse from the firstly created object (if it has been 
## calculated previously) or calculates it

## makeCacheMatrix creates a object (list) containing the raw matrix and its 
## inverse plus other two

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) mat <<- solve
      getinverse <- function() mat
      list(set = set, get = get, setinverse = setinverse,
           getinverse = getinverse)

}


## the cacheSolve functions retreives the element created by the previous function
## where the inverse matrix is saved. If this element does not exist, it calculates
## it and returns it anyways

cacheSolve <- function(x, ...) {
      mat <- x$getinverse()
      if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
      }
      else{
            data <- x$get()
            mat <- solve(data, ...)
            x$setinverse(mat) ## add inverse to the cache
            mat  ## Return a matrix that is the inverse of 'x'
      }
}
