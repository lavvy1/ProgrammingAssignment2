##  Function 'makeCacheMatrix' parallels the function
##  'makeVector' of assignment 2.
##
## makeCacheMatrix() takes a matrix argument, and
##  returns a list of 4 function:
##
##  Functions: 
##   set(y) sets the value of the matrix.
##   get() gets the value of the matrix.
##   setInv(inv) caches the value of the matrix inverse, using "<<-".
##   getInv() returns the value of the cached inverse matrix.


makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function (y){
          x <<- y
          m <<- NULL
     }
     
      get    <- function() x
      setInv <- function(inv) m <<- inv 
      getInv <- function() m
     
     list (set = set, get = get, setInv = setInv, getInv = getInv)
     
}

 
## Function cachesolve() parallels the function 'cachemean',
##   given in assignment 2.
##
## cachesolve(x) calculates the inverse of the 'special'
##   matrix created in the 'makeCacheMatrix' function above.
##   It returns the value of the inverse matrix.
##
## In the first step, cacheSolve(x) checks to see if the inverse
##  has already been calculated. If so, it gets the
##  inverse from the cache and skips the actual calculation.
##
## Otherwise it calculates the inverse and sets the inverse
##   into the cache using the setInv function.
##
## Note the the function 'solve', below, requires that
##   the matrix be square. Also if the matrix is not invertible,
##   an error will occur.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     # check to see if cached inverse already exists.
     m <- x$getInv()
     if (!is.null(m)){
               message ("getting cached matrix inverse")
               return(m)
               
     }
     
     data <- x$get ()
     
     # calculate the invese of the square matrix 
     m <- solve (data,...)
     x$setInv (m)
     m
}
