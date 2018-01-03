## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    to_be_recomputed <- TRUE # determines whether the inverse has already been calculated for the current matrix instance
    
    set <- function(x_in){
        to_be_recomputed <<- TRUE
        x <<- x_in
    }
    get <- function(){
        x
    }
    setInv <- function(inv_in){
        to_be_recomputed <<- FALSE
        inv <<- inv_in
    }
    getInv <- function(){
        inv
    }

    getTo_be_recomputed <- function(){
        to_be_recomputed
    }
    list(set = set, get = get, setInv = setInv, getInv = getInv, 
         getTo_be_recomputed = getTo_be_recomputed)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    if (!x$getTo_be_recomputed()){
        
        message("retrieving the cached inverse")
        return(x$getInv())
        
    } else{
        
        inv_tmp <- solve(x$get(), ...)
        x$setInv(inv_tmp)
        message("computing the inverse")
        return(inv_tmp)
        
    }
}
