## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the solve
## 4. get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
    ## Return an object List that can cache the inverse of a matrix (in parameter)
    
    m <- NULL
    
    # assign to "set" a function which set the value of the matrix
    # the matrix is the parameter (y)
    set <- function(y) {
        # if the parameter (y) is a matrix
        # assign a values (y and null) to objects (x and m) in an environment that is different
        # from the current environment
        if (is.matrix(y)){
            # if "set" is called, matrix is replaced by parameter
            # and the inverted matric already calculated is removed from the cache
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
    
    # "m" is returned (inverted matrix)
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
    
    # if parameter (x) is a matrix, transform in special "cacheMatrix"
    if(is.matrix(x)){
        x <- makeCacheMatrix(x)
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
    # compute into to "m" the inverted of the matrix
    m <- solve(data, ...)
    
    # cache the "m" inverted matrix
    x$setsolve(m)
    # return the inverted matrix
    m
}

################## FOR TEST PURPOSE ##################
# 
# # Debug functions
# debug(makeCacheMatrix)
# debug(cacheSolve)
# 
# # Test functions
# x<- matrix(c(1,3,5,6,7,8,43,21,34),3,3)
# matrix.to.solve <- makeCacheMatrix(x)
# res1 <- cacheSolve(matrix.to.solve)
# res2 <- cacheSolve(matrix.to.solve)
# 
# # Test with bigger matrix
# y <- matrix(runif(1000000000, min = 0, max = 99999),1000,1000)
# matrix.to.solve.big <- makeCacheMatrix(y)
# res3 <- cacheSolve(matrix.to.solve.big)
# res4 <- cacheSolve(matrix.to.solve.big)
# 
# # Test to solve a "normal" matrix
# z <- matrix(c(1,39,4,2,7,8,43,21,34),3,3)
# res5 <- cacheSolve(z)
# 
# # Test to set
# zz <- matrix(c(1,3,5,6,12,101,43,21,34),3,3)
# matrix.to.solve$set(zz)
# res6 <- cacheSolve(matrix.to.solve)
# 
# zzz <- c(1,3,5,6,12,101,43,21,34)
# matrix.to.solve$set(zzz)
# 
# # Compare to "standard" solve
# b <- solve(x)
# c <- solve(y)
# d <- solve(z)
# e <- solve(zz)
