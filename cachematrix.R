makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix returns a special matrix which can cache/store its
    ## inverse.
    ## The object returned by the function is a list of functions which let you
    ## access and modify the matrix and access and set its inverse.
    ## Description of the functions:
    ##  set     - stores matrix
    ##  get     - returns cached matrix
    ##  setinv  - stores inverse
    ##  getinv  - returns cached inverse of matrix

    inv <- NULL

    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv

    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    if(is.null(x$getinv()))
    {
        ## calculate the inverse of 'x' and cache it
        x_inv <- solve(x$get(), ...)
        x$setinv(x_inv)
    }

    ## return cached inverse
    x$getinv()
}
