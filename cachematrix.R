## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function stores the four functions and make a list of it. It's similar to stream in Scheme.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

## This get the value if not null or calculates and put into cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

test <- function() {
    a <- makeCacheMatrix(matrix(1:4, 2, 2))
    b <- cacheSolve(a)
    if (identical(b, rbind(c(-2, 1.5), c(1, -0.5)))) {
        print("Pass")
    }
    else print("Fail")
}

test()
