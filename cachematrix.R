## FUNCTIONS WHO CACHE A INVERSE OF A INVERTIBLE MATRIX
## SAMPLE EXECUTION ----------------------------
##
## > x <- makeCacheMatrix(matrix(c(4,2,7,6),2,2))
## > x$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > cacheSolve(x)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > x$set(matrix(c(1,0,5,2,1,6,3,4,0),3,3))
## > x$get()
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > cacheSolve(x)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > cacheSolve(x)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## ---------------------------------------------

## CACHE A INVERTIBLE MATRIX
## parameter x must be a invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        ##Initializing s
        s <- NULL
        ##Defining function set for set matrix cache
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ##Defining function get to get matrix who was set before
        get <- function() x
            
        ##Defining functions setsolve and getsolve used by 
        ## cacheSolve to calculate and return the inverse of matrix x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, 
                setsolve = setsolve, 
                getsolve = getsolve)
}


## RETURN INVERSE OF A CACHED INVERTIBLE MATRIX
## parameter x must be a object result from makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ##Store inverse of matrix in s who was calculate before
        s <- x$getsolve()
        ##Verify if s was defined and return your previous execution cached
        if(!is.null(s)) {
                message("getting cache data")
                return(s)
        }
    
        ##Return the calculate s who contains the inverse of 
        ## matrix and cache the result
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
