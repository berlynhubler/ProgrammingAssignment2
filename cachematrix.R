## Makes x, a square matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {

     inv = NULL
     set = function(y) {
          # use `<<-` to assign a value to an object in an environment 
          # different from the current environment. 
          x <<- y
          inv <<- NULL
     }
     get = function() x
     setinv = function(inverse) inv <<- inverse 
     getinv = function() inv
     list(set=set, get=get, setinv=setinv, getinv=getinv)
     
}


## takes matrix of makeChacheMatrix and
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

     
     inv = x$getinv()
     
     # if inverse has already been calculated
     
     if (!is.null(inv)){
          # skips computation obtain from cache
          message("getting cached data")
          return(inv)
     }
     
     # calculates the inverse 
     matrix.dat = x$get()
     inv = solve(matrix.dat, ...)
     
     # sets the value of the inverse in the cache via the setinv function
     x$setinv(inv)
     
     return(inv)
     
}
