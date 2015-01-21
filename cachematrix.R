## User written R-functions to invert a matrix and
## store and retrieve it from cache for easy referrals later.

## makeCacheMatrix :

## Purpose: Function to make a matrix with
## attributes, that stores values (here inverse matrix) in
## cache and helps retrieves them from cache if the
## the same referral was already made
## Arguments: invertible matrix

makeCacheMatrix <- function(mat = matrix()) {
        #minv denotes the matrix inverse
        minv <- NULL
        set <- function(y) {
                mat <<- y
                minv <<- NULL
                #forget matrix inverse in cache when a new matrix is set
        }
        get <- function() mat

        setinv <- function(solve) {
        #inverse of a matrix can be got using solve function
        #To store inverse, store value from solve function
        minv <<- solve
        }

        getinv <- function() minv

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## cacheSolve Function is used to inverse matrix,
## and retrieves the inverse from cache if the
## the same referral was already made
## Arguments: invertible matrix(mandatory),
## specifications to solve functionoptional)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        minv <- x$getinv()
        if(!is.null(minv)) {
                #minv (matrix inverse) not null, exists.
                #Display from cache.
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv

}