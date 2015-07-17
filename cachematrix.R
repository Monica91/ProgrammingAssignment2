## makeCacheMatrix.R
## This function creates a special "matrix" object that can cache its
## inverse

## Execution example
## 1. source("cachematrix.R")
## 2. x<-makeCacheMatrix()
## 3. x$set(matrix(c(1, 2, -1, -1), 2,2))
## 4. x$get() 
## 5. inv <- cacheSolve(x) [The digits remain the same but the signs switch columns]
## 6. inv <- cacheSolve(x) [Message: getting cached data]

## Other execution example
## 1. source("cachematrix.R")
## 2. m <- matrix(c(1, 2, -1, -1), 2,2)
## 3. x<-makeCacheMatrix(m)
## 4. x$get() 
## 5. inv <- cacheSolve(x) [The digits remain the same but the signs switch columns]
## 6. inv <- cacheSolve(x) [Message: getting cached data]

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##-----------------------------------------------------------------##

## cacheSolve.R
## This function computes the inverse of the special "matrix" retur- 
## ned by makeCacheMatrix above. If the inverse has already been cal-  
## culated (and the matrix has not changed), then cacheSolve should  
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

##-----------------------------------------------------------------##

## cacheSolve.R
## This function computes the inverse of the special "matrix" retur- 
## ned by makeCacheMatrix above. If the inverse has already been cal-  
## culated (and the matrix has not changed), then cacheSolve should  
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

