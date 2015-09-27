



# This function creates an environment with local variables and several functions.
# The functions returned in the list will have access to variables within the environment
# encapsulated within makeChacheMatrix. This is a nifty trick for getting object like
# behavior with persistence.
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse){inv <<-inverse}
    
    getInverse <- function() inv
    
    list(set = set, get = get
         , setInverse = setInverse
         , getInverse = getInverse)
}


# Once the funtion above is called and the output assigned to a variable (say x),
# This function wil capitalize upon the persistence within makeCacheMatrix and 
# make use of the cached matrix inverse.

# Note, this function assumes all matrices supplied as inputs are invertible per course project
# instuctions.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("Getting Cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
}



