## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the solve
## 4. get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
    ## Return an object List (getter and setter) that can cache the inverse of a matrix (in parameter)
    ## Parameter x has to be a Matrix
    
    m <- NULL
    
    # Assign to "set" a function which set the value of the matrix
    # passed as input parameter
    set <- function(y) {
        # If the parameter (y) is a matrix
        # cahe the matrix in 'x' (in a different environment)
        # and clear the inverted matrix already calculated
        if (is.matrix(y)){
            x <<- y
            m <<- NULL
        } else{
            message("Parameter of Set as to be a Matrix")
        }
    }
    
    # Assign to "get" a function returning the matrix of the current obect
    get <- function() x
    
    # Assign to 'setsolve' a function which caches the inverted matrix.
    # solve is the inverted matrix (parameter of the function) which is cached into m
    setsolve <- function(solve) m <<- solve
    
    # Assign to 'getsolve' a function returning "m" (inverted matrix)
    getsolve <- function() m
    
    # Return list with getter and setter of the object
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  
    ## Return a matrix that is the inverse of 'x'
    ## 'x' has to be a special matrix returned by 'makeCacheMatrix'.
    
    # If parameter (x) is a matrix and not a "special matrix", give an error message to user
    # and return NULL
    if(is.matrix(x)){
        message("parameter has to be a special CacheMatrix")
        return(NULL)
    }
    

    # If it is already in the cache the inverted matrix is returned and function stops
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Else invert and return
    data <- x$get()         # assign to "data" the matrix to solve
    m <- solve(data, ...)   # compute into to "m" the invert of the matrix
    x$setsolve(m)           # cache the "m" inverted matrix
    m                       # return the inverted matrix
}
