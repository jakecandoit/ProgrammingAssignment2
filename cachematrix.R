## Put comments here that give an overall description of what your
## functions do
## The program has two functions. The first function stores a matrix and the inverse. 
##The second function retrieves the inverse of the matrix cached by the first function.
##
## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	##initializes x
	invm <- NULL ##initializes m
        set <- function(y) {
                x <<- y
                invm <<- NULL
			print(y)
        }
        get <- function() x ##defines the getter for matrix x
        setmatrix <- function(matrix) invm <<- matrix  #defines the setter for the inverse matrix invm
        getmatrix <- function() invm #defines the getter for inverse matrix invm
        ##creates a list of the setters and getters created earlier
	list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  invm <- x$getmatrix() ##retrieves the inverse matrix
        ## checks if the inverse matrix is cached
	if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
	##if there is no cached inverse matrix, get the inverse matrix
        data <- x$get()
        invm <- solve(data, ...)
        x$setmatrix(invm)
        invm
}
