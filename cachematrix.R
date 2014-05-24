## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions calculates the 

makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL
        ##Defines a function that when it is called it takes the variable x and equals it
        ##to the argument. Then sets i to null.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ##Get function returns the value of x
        get <- function() x
        #Setmean calculates the inverse and assignates its result to i variable
        setsolve <- function(solve) i <<- solve
        #Getmean returns the inverse
        getsolve <- function() i
        #Creates a list with all the previous functions and stores it
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##This function checks if the Matrix stored via makeCacheMatrix has already
##been run through cacheSolve (and has the same value(s)) if yes it gets the
##stored values if not it calculates the inverse

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
        ## Return a matrix that is the inverse of 'x'