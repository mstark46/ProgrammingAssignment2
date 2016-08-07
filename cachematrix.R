makeCacheMatrix <- function(x = matrix()){
	m <- NULL					#Initializes NULL matrix
	set <- function(y){			#Resets values for x and resets m to NULL
		x <<- y
		m <<- NULL
	}
	get <- function() x			#Retrieves values of x
	setInverse <- function(inverse) m <<- inverse	#Caches inverse matrix
	getInverse <- function() m				#Retrieves cache
	list(set = set, get = get,				#List of functions
		setInverse = setInverse,
		getInverse = getInverse)
	
}

cacheSolve <- function(x, ...){
	m <- x$getInverse()				#Retrieves cache
	if(!is.null(m)){					#If cache not empty returns value
		message("Getting cached data")
		return(m)					#If data hasn't been changed 
	}							#returns cached value
	data <- x$get()					#Retrieves matrix data
	m <- solve(data,...)				#Solves inverse of data
	x$setInverse(m)					#Sets new calculated inverse
	m
}