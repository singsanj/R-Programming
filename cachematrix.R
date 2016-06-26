## Lexical Scoping Example demonstration, matrix inversion considered as expensive computation 
## at runtime for large dataset when running in a loop, code in this assignment is to 
## use the scoping feature in R to preserve the state of the static object 

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function is to compute and return the inverse of the matrix, the
## first computation performed to get the inverse of the matrix, on computation the 
## result is preserved via setinverse, subsequent calls utilize the get function to
## access the cached data.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
