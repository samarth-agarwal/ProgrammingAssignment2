## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCachMatrix performs 4 functions, namely:
##  1. Set the Matrix x
##  2. Retrieve (get) the matrix x
##  3. Set the inverse of the matrix x (xinv), and store it in the cache memory
##  4. Retrieve xinv from cache

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(xinv) inv <<- xinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function
## This function is used to retirve the inverse of matrix x
##  1. If the inverse is already saved in cache memory, it will retieve it.
##  2. Else, it will calcute it within the function and then cache it.
##  3. In the end, it will return in the inverse obtained.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached matrix-inverse")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
    message("this matrix-inverse was not obtained from cache")
    inv
}
