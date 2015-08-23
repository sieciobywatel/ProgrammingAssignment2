## Create special a matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # stored inversed matrix
        set <- function(y) {
                x <<- y  # save y in this matrix
                inv <<- NULL # clear outdated inverse
        }
        get <- function() x  # return stored matrix
        set_solved <- function(solve) inv <<- solve # store inverse
        get_solved<- function() inv  # return stored inverse

        # return closures ("scoped functions") as a named list:
        list(set = set, get = get,
             set_solved = set_solved,
             get_solved = get_solved)
}

## Retrieve an inverse of CacheMatrix or compute if not available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_solved()
        if (!is.null(inv)) {
                # inverse was already cached
                message("getting cached data")
                return(inv)
        }

        # else we need to compute inverse:
        data <- x$get()  # get internal matrix
        inv <- solve(data, ...) # solve
        x$set_solved(inv) # store computed inverse in CacheMatrix
        inv
}
