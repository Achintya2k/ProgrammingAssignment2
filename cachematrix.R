## Writtten code for a pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Inverse
    inv <- NULL

    ## set the matrix
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }
    ## get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }
    ##  set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    ## inverse of the matrix
    getInverse <- function() {
        ## Return the inverse
        inv
    }
    ##list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Written code to compute the inverse of the special matrix returned by the
## above function.The following function should retrieve the inverse from the cache
## provided the inverse has already been calculated and the inverse remains 
## unchanged.
cacheSolve <- function(x, ...) {

    ## the inverse of 'x'
    m <- x$getInverse()
    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    ## matrix from object
    data <- x$get()
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    ## Set the inverse to the object
    x$setInverse(m)
    ## Return the matrix
    m
}
