## Functions to cache the inverse of a matrix. 

##  I think that makeCacheMatrix() defines the four functions that will be 
## in the list in the output.
## set is a function to set x and m from values in the parent environment. 
## x and m are set
## get is a function that actually loads the matrix that is passed by x
## setsolve is the function taht would calculate the inverse of the matrix
## getsolve is the function that loads the value of m from the parent environment 
## I think that what is returned then is a list of the four functions which have 
## been defined and set up in the parent environment: set, get, setsolve and getsolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve returns the inverse of the matrix that is passed into x
## cacheSolve first checks if the inverse of the matrix has already been 
## calculated and that should be in x$getsolve from the parent environment
## If there is something there, ie the value is in the cache, then there is nothing 
## except to display the result and breakout of cahceSolve.
## If there is nothing there 'NULL', the data is loaded with the matrix using get, 
## the inverse is actually calculated with solve() adn the result cached using setsolve. The 
## the result is displayed.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
