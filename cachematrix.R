## Put comments here that give an overall description of what your
## functions do


##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	#set mattix
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	
	#get matrix
	get <- function() x
	
	#set inverse matrix
	setinverse <- function(invmatrx) m <<- invmatrx
	
	#get inverse matrix
	getinverse <- function() m
	
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	
	#get matrix and solve the inverse 
	data <- x$get()
	m <- solve(data, ...)
	
	#save inverse matrix as cache
	x$setinverse(m)
	
	#return solved inverse matrix
	m		
}
