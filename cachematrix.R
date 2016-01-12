## makeCacheMatrix stores a matrix and caches its inverse 
## cacheSolve calculates the inverse of the matrix

## This function creates a special "matrix" object: a list containing 
## functions to get and set the values of the matrix and its inverse 


makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
                x <<- y
                s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
      getsolve <- function() s
	list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)	
}

## This function computes the inverse of the special "matrix" created 
## with makeCacheMatrix. If the inverse has already been calculated it
## gets the inverse from the cache and skips computation. Otherwise it 
## computes the inverse and sets the result in the cache


cacheSolve <- function(x=matrix(), ...) {
	s<- x$getsolve()
	if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
	matrix <- x$get()
	s <- solve(matrix, ...)
	x$setsolve(s)
      s
}
