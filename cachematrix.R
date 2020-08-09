# This functions cache's the inverse of a matrix for future calculations

## This matrix creates a list in order to cache's the previous computations
## of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y){
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve  <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, 
		 setsolve = setsolve, 
		 getsolve = getsolve)
}


## This function computes the inverse of a matrix only if it's not
## stored in cache

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
