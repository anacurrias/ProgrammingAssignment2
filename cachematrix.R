## Below are two functions that are used to create a special object that stores a matrix
## and cache's its inverse. It's assumed that the matrix supplied is always invertible.

## makeCacheMatrix function creates a special matrix, which is really a list
## containing a function to set the matrix, get the matrix, set the inverse
## of the matrix and get the inverse of such matrix.

makeCacheMatrix <- function(x = matrix()) {

	At <- NULL

	set <- function(B){
		x<<-B
		At<<- NULL
	}
	get <- function() x

	setinverse <-function (solve) At <<-solve
	getinverse <-function () At

	list (set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special matrix created
## before. First, it checks if the inverse has already been calculated. If so
## it gets the inverse from cache. Otherwise, inverse matrix is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	At <- x$getinverse()
      if(!is.null(At)) {
              message("getting cached data")
              return(At)
      }
      data <- x$get()
      At <- solve(data, ...)
      x$setinverse(At)
      At
}
