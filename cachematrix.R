## Below are two functions that are used to create a special object that stores a matrix and 
## cache's its inverse.

## This function creates a special "matrix" which is really a list containing a function to:
## set the value of the matrix (set_matrix)
## get the value of the matrix (get_matrix)
## set the value of the inverse (set_inverse)
## get the value of the inverse (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 				
	set_matrix <- function(y) {			
		x <<- y 					
		inverse <<- NULL
	}
	get_matrix <- function() x 				
	set_inverse <- function(solve) inverse <<- solve 	
	get_inverse <- function() inverse 			
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}

## This function calculates the inverse of the special "matrix" created above.
## it first checks to see if the inverse has already been calculated
## If yes, then it gets the inverse from the cache. 
## If no, it calculates the inverse and sets the value of the inverse in the cache 

cacheSolve <- function(x, ...) {				
	inverse <- x$get_inverse()				
	if(!is.null(inverse)) {					
		message("getting cached data")			
		return(inverse)
	}
	data <- x$get_matrix()					
	inverse <- solve(data, ...)				
	x$set_inverse(inverse)					
	inverse 						
}