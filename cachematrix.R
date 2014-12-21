## Description of these fairly simple and theoretical snippets
## 

## Per assignment, this function creates a matrix object that "caches" its inverse.
## In this approach, the whole makeCacheMatrix is a "setter" per the numeric matrix given

makeCacheMatrix <- function(x = matrix()) {
        xi <- NULL
        m <- x
        calc_xi <- function() xi <<- solve(m)
        get_xi <- function() xi
        list(get_xi = get_xi,
             calc_xi = calc_xi)
}

## The inverse of such matrix-object above (returned by makeCacheMatrix)
## In this approach, cacheSolve uses either the already calculated inverse (with getter get_xi)
## or with the getter that runs the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$get_xi()
        if(!is.null(inverse_x)) {
        		## Print to ensure we get it right
                message("getting cached data")
                return(inverse_x)
        }
        inverse_x <- x$calc_xi()
        x$get_xi()
}
