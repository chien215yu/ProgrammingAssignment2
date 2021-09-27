makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y #creates x
    inver <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set <- set, #function to take argument and do things inside
       get <- get,
       setInverse <- setInverse,
       getInverse <- getInverse)
}

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$setInverse(inver)
  inver
}
