makeCacheMatrix <- function(a = matrix()) {
        ## a - a square matrix which is inversible
        ## return: a list to set, get the matrix and the corresponding ofinverse
        ## this list is used as input to cacheSolve()
        
        inv = NULL
        set = function(b) {
                a <<- b
                inv <<- NULL
        }
        get = function() a
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(a, ...) {
        ## a is output of makeCacheMatrix()
        ## return inverse of the original matrix input to makeCacheMatrix()
        
        inv = a$getinv()
        
        ## if the inverse has already been calculated, get from cache
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## otherwise, calculate the inverse and set value in Cache
        mat.data = a$get()
        inv = solve(mat.data, ...)
        
        a$setinv(inv)
        
        return(inv)
}
