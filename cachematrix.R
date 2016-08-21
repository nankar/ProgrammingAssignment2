## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invrse <- NULL
        set <- function(y) {
                            x <<- y
                            invrse <<- NULL
                          }
        get <- function() x
        setinverse <- function(inverse) invrse <<- inverse
        getinverse <- function() invrse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns inverse of the matrix provided as a input. 
## This function will check if matrix inverse has been computed earlier or not. 
## If yes, then it will cache data of already calucated inverse for the same matrix.
## If not, then will compute the inverse and sets the result in the cache 

# For this assignment, assumption is that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        invrse <- x$getinverse()
        if(!is.null(invrse)) {
                            message("Getting Cached Data.")
                            return(invrse)
                             }
        data <- x$get()
        invrse <- solve(data)
        x$setinverse(invrse)
        invrse
}
