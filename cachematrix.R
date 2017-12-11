## create a special object that stores a numeric matrix
## and caches its inverse

## creates a list containing four functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse matrix
## 4. get the value of its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inversematrix){
		inv <<- inversematrix
	}
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv =getinv)
}

## compute the inverse of the special matrix created by the above function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
