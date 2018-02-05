+##Sets the value of m to Null, then cacheSolve uses functions defined in makeCacheMatrix to check
  ##has m been calculated, if not it calculates the inverse of the matrix, sets m to the inverse matrix value which is now cached.
  ##Then if m is already calculated, prints getting cached data and prints m 
  
makeCacheMatrix <- function(x = matrix()) {
    m <-NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv){m <<- inv}
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  }
  
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      print (m)
      if(!is.null(m)) {
        "getting cached data"
        return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinv(m)
      m
  }