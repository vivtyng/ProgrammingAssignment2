## Put comments here that give an overall description of what your
## functions do
 
###    given an inversible matrix x
###    a<-makeCacheMatrix(x) caches its inverse in variable inv
###    THEN, cacheSolve(a) returns either the cached inv, 
###      or computes solve(x) and caches it into inv

## Write a short comment describing this function

### a <- makeCacheMatrix(x) creates a list of 4 functions labeled set,get,setsolve and getsolve
###  * a$set(y) changes the value of x to y, 
###             and erases any previous inv value;
###  * a$get()  fetches x;
###  * a$setsolve(z) resets the saved inv to value z; 
###  * a$getsolve() fetches pre-computed invese inv.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
### Assuming makeCacheMatrix has been run on the same argument
###   cacheSolve(x) first tries to retrieve the pre-computed inverse of x
###    if it exists, then returns it
###    if not, then (1) copies x into data,
###                 (2) computes its inverse inv
###                 (3) caches inv value via x$setsolve()
###                 (4) and returns inv

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
