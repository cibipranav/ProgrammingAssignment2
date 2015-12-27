## The objective here is to to write a pair of functions 
## that cache the inverse of a matrix.

## Primary assumption: all matrices passed are square invertible matrices
 
## This function creates a special "matrix" object 
## that can cache its inverse.It is a function of functions.

makeCacheMatrix <- function(x = matrix()) {

 	  inv = matrix()
	  inv <- NULL

        set <- function(y = matrix()) { 	## Function to initialize the matrix
                x <<- y
                inv <<- NULL
        }
        
	  get <- function() x 			## Function to obtain source matrix 
       
	  setinv <- function(localinv = matrix()) inv <<- localinv
        
	  getinv <- function() inv
        
	  list(set = set, get = get,setinv = setinv,getinv = getinv)
							## Returns a list of matrices

}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {

	  inv <- x$getinv()			## Local inverse matrix

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        input_matrix <- x$get()
        
	  inv <- solve(input_matrix, ...)
        
	  x$setinv(inv)
        
	  inv
       						 ## Returns matrix that is the inverse of 'x'
}

