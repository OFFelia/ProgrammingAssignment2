## Process of inverse matrix calculation is usualy time-consuming. This pair of 
## the functions helps us to cache the inverse of the matrix.


## this function creates an object "matrix" that can cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) { ## initialization of function
    s <- NULL                           ## initializing a local variable to NULL
    set <- function(y) {                
        x <<- y                         ## setting the cached matrix "x"
        s <<- NULL                      ## setting the cached inverse to NULL
    }
    get <- function(){                  ## uses "x" from the cache
       return (x)
    }
    setsolve <- function(solve){       ## uses inverse matrix and set it to cache 
        s <<- solve
    }
    getsolve <- function(){             ## returns the cached inverse matrix
        return (s)
    }
    list(set = set, get = get, ## assign values to arguments for the next function  
         setsolve = setsolve,       
         getsolve = getsolve)

}


## this function calculates the inverse of special "matrix" from the previous
## function. If the inverse has already been calculated and "matrix" is the same,
## function just return cached inverse.

cachesolve <- function(makeCacheMatrix, ...) { ## initialization of the function
    
    local_solve <- makeCacheMatrix$getsolve() ## sets the local variable to cached
    
    if(!is.null(local_solve)) {         ## checks whether "local_solve" isn't NULL
        message("getting cached data") ## prints message 
        return(local_solve)            ## returns inverse from the cache. Exit
    }
    else {                              ## if "locale_solve is NULL
        local_data <- makeCacheMatrix$get() ## obtains matrix from makeCacheMatrix 
        local_solve <- solve(local_data, ...) ## calculates inverse
        
        makeCacheMatrix$setsolve(local_solve)  
        return(local_solve)         ## Return a matrix that is the inverse of 'x'
    }
}
        


