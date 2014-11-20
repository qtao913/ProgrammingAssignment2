## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function constructs a "makeCacheMatrix" object
## The object contains two attributes: 1. the original matrix "x"; 2. the inverse of x, stored in "m"
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL			## cache "m" is set to NULL every time an new object is constructed
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() {
		x
	}
	setInverse <- function(inverse) {
		m <<- inverse
	}
	getInverse <- function() {
		m
	}
	
	## making a list of all internal methods for other functions to access them
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function
## return the value in cache if the inverse of "x" is calculated, or calculate the inverse and set the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Check whether the value is calculated previously
        if(!is.null(m)) {
	        	message("getting cached data")
	        	return(m)
        }
        ## When the inverse of x hasn't been calculated before, carry out the following steps
        data <- x$get() 		    ## get the matrix "x"
        m <- solve(data,...) 	    ## calculate the inverse of "x"
        x$setInverse(m)			    ## cache the inverse
        m 							## print out the inverse
}
