## These functions enable the efficient calculation of the inverse matrix of sqaure invertible matrix.
## Once the inverse matrix is calculated, it is cached and returned should the inverse be requested
## again.  If the value of the original matrix is changed then inverse is recalculated and the
## cache updated

## makeCacheMatrix - creates a special 'matrix' object that can cache its inverse
##  x  : a square matrix which is assumed to be invertible
makeCacheMatrix <- function(x = matrix()) {
        ## initialise the chached inverse matrix
        inv <- NULL
        
        ## set : store the matrix and clear the inverse
        ## y : a square invertible matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get : return the original matrix
        get <- function() x
        
        ## setinverse - store the inverse matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## getinverse - retrive the cached inverse matrix
        getinverse <- function() inv
        
        ## return a list of functions to manage the object
        list( set = set, 
              get = get, 
              setinverse = setinverse,
              getinverse = getinverse )                
}

## cacheSolve 
##      - use a special matrix object to retrieve the inverse of the matrix
##      - if the inverse has already been calcuklated then return the cached inverse        
## input x - special matrix object, assumed to contain a square inversible matrix
## return  - the inverse matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                   ## retrieve the cached inverse value
        if(!is.null(inv)) {                     ## return cached value if not NULL
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                         ## no cached value so retrieve the original matrix
        inv <- solve(data, ...)                 ## calculate the inverse
        x$setinverse(inv)                       ## cache the answer
        inv                                     ## and return it
}