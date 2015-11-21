## As the assignment instructions for the R Programming class in the Coursera
## Data Science Specializations put it, these two functions that cache the inverse
## of a matrix. This is to understand how, in a case with a more complex matrix,
## to save (cache) the calculated inverse for repeated use. 

## makeCacheMatrix allows for a varying matrix to be stored in the main function
## and subsequently restored Inv to NULL because the old inverse is not needed 
## anymore. 

makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      set <- function(y) {
        x <<- y
        Inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) Inv <<- inverse 
      getinverse <- function() Inv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## cacheSolve checks to see that the value "Inv" (previously stored in getinverse"), 
## exists and is not NULL. If it already exists, the specified message is given and 
## the value Inv is returned. If it is NULL, "mat" gets the matrix in makeCasheMatrix
## Inv gets the inverse of the matrix, and x$setinverse(Inv) stores it in the object
## created assigned with makeCacheMatrix

cacheSolve <- function(x, ...) {
      Inv <- x$getinverse()
      if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
      }
      mat <- x$get()
      Inv <- solve(mat, ...)
      x$setinverse(Inv)
      Inv
}
