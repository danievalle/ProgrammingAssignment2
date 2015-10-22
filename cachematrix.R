## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the solve
## 4. get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
    ## Return an object List that can cache the inverse of a matrix (in parameter)
    ## Parameter x has to be a Matrix
    
    m <- NULL
    
    # assign to "set" a function which set the value of the matrix
    # the matrix is the parameter (y)
    set <- function(y) {
        # if the parameter (y) is a matrix
        # assign a values (y and null) to objects (x and m) in an environment
        # that is different from the current environment
        if (is.matrix(y)){
            # if "set" is called, matrix is replaced by parameter
            # and the inverted matrix already calculated is removed from the cache
            x <<- y
            m <<- NULL
        } else{
            message("Parameter of Set as to be a Matrix")
        }
    }
    
    # assign to "get" a function returning the matrix of the current obect
    get <- function() x
    
    # cache the inverted matrix.
    # solve is the inverted matrix (parameter of the function) which is cached into m
    setsolve <- function(solve) m <<- solve
    
    # a function returning "m" (inverted matrix)
    getsolve <- function() m
    
    # return list with getter and setter of the object
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
    
    # if parameter (x) is a matrix and not a "special matrix", give an error message to user
    # and return NULL
    if(is.matrix(x)){
        message("parameter has to be a special CacheMatrix") 
        # not requested in the function, so return NULL
        # x <- makeCacheMatrix(x)
        return(NULL)
    }
    
    # check if inverted matrix is cached
    # if it is already in the cache the inverted matrix is returned and function stops
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # assign to "data" the matrix to solve
    data <- x$get()
    
    # compute into to "m" the invert of the matrix
    m <- solve(data, ...)
    
    # cache the "m" inverted matrix
    x$setsolve(m)
    
    # return the inverted matrix
    m
}
