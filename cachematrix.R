## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions that calculate the inverse of a
##matrix and also set the variables to see if the function has its inverse stored

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
        #If i is empty calculates the inverse, if not, gets the cached data
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #Here it gets the matrix
        data <- x$get()
        #Makes the inverse
        i <- solve(data, ...)
        #And stores it...
        x$setsolve(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
