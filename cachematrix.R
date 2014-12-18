## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a list that sets and gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y){
               x <<-y
                n <<-NULL
        }
        get <-function() x
        setinverse <-function(inv) n <<- inv
        getinverse <- function() n
        list(
                set=set,
                get=get,
                setinverse = setinverse,
                getinverse = getinverse)
        }


## The cacheSolve function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        n <- x$getInverse()
        if(!is.null(n)){
                message("getting cached data")
                return(n)
        }
        
        data <- x$get()
        
        n <- solve(data)
        
        x$setInverse(n)
        
        n
        
}
