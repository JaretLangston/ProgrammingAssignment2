## The functions solve the inverse of a specified matrix and caches the result. If the cunctions are called 
## again for the same matrix, then the cached value is returned and the inverse does not require 
## re-calculation.

## makeCaceMatrix function accepts a matrix variable and will store a cached value 
## for the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ##Set function updates the matrix value and clears the cache
        set <- function(y) {
                ##Update the matrix value for x and set the cache value m to NULL
                ##NOTE: x and m are not defined in this function, but are defined in
                ## the parent function. So the use of <<- is needed to set the values.
                x <<- y
                m <<- NULL
        }
        ##Get function returns the current matrix value
        get <- function(){
                x
        } 
        
        ##SetInverse function updates the cache value
        setinverse <- function(inv){
                ##Update the inverse cache value
                ##NOTE: the variable m is not defined in this function, but is defined in the parent
                ## function. So the use of <<- is required to set the value of m.
                m <<- inv
        }
        
        ##GetInverse function returns the current cache value for the inverse
        getinverse <- function() {
                m
        }
        
        ##Setup the list to enable the internal functions to be called
        ##from outside the main function.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse values for a specified matrix variable.
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x is an instance of the makeCacheMatrix 

        ##Get the cached inverse value
        m <- x$getinverse()
        ##Test if the cached value was NULL
        if(!is.null(m)) {
                ##it was not NULL, return the value of m, the return statement will exit the function.
                message("getting cached inverse")
                return(m)
        }
        
        ##The value of m was NULL, so the function was not exited and the function execution will continue.
        message("Cached inverse does not exist, calculating the inverse...")
        ##Get the matrix data value from x
        data <- x$get()
        ##solve the inverse for x and store it in m
        m <- solve(data)
        
        ##update the inverse cache value in x
        x$setinverse(m)
        ##return the inverse result and exit the function.
        m
}
