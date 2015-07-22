## Functions provided in this file can be used to store matrix and its inverse matrix
## Inverse matrix is cached, so it can be read multiple times with no computations (after first call to cacheSolve)

## makeCacheMatrix returns list which can store matrix and cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(m) {
                x <<- m
                inversed <<- NULL
        }
        get <- function() x
        setinversed <- function(inv) inversed <<-inv
        getinversed <- function() inversed

        ## return vector of above functions
        list(
                set = set,
                get = get,
                setinversed = setinversed,
                getinversed = getinversed
        )
}


## Calculates inverse matrix of special cacheMatrix - when not neede, inverse matrix is not calculated directly

cacheSolve <- function(x, ...) {
        inv <- x$getinversed()
        if (is.null(inv)) {
                ## need to actually solve for inverse matrix
                inv <- solve(x$get(), ...)
                ## cache it
                x$setinversed(inv)
        }
        ## Return cached or calculated above inverse matrix
        inv
}
