## Lexical scoping assignment 2

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
       x <<- y
       m <<- NULL
      }
     get <-function() x  
     setinvers <-function(inverse) m <<- inverse
     getinvers <-function() m
     list(set=set, get=get,setinvers=setinvers,getinvers=getinvers)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getinvers() 
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinvers(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

g