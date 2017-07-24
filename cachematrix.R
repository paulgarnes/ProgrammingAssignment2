## Programming Assignment 2 - Caching the Inverse of a Matrix

## Caching allows us to reclaim valuable computational time especially with 
## typically time-intensive procedures like Matrix inversion. That way, there
## is no need to compute the result for the same matrix over and over--provided,
## of course, that the matrix stays the same.It's like visiting the same web-
## page over and over. The page loads faster usually because it is pulling from
## the cache as opposed to the 

## The idea of this exercise is to develop a couple functions that not only
## store a matrix but also caches its result..

## The first function, makeCacheMatrix creates an object that can cache its 
## inverse according to the steps outlined in the assignment guide:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # store the cached inverse matrix
        inv <- NULL
        # 1. Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # 2. Get the value of the matrix
        get <- function() x
        # 3. Set the value of the inverse
        setInverse <- function(inverse) inv <<- inverse
        # 4. Get the value of the inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}


## This second function, cacheSolve computes the inverse of the special "matrix"
## from the makeCacheMatrix above. We'll know if this actually works if when the
## matrix has not changed, the function pulls from the cache instead of 
## computing the answer from scratch.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Utilizing cached data instead...")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
