makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y #creates x and retrieved by get(), search parent environment for existing definition
    inver <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set <- set, #function to take argument and do things inside, set take y as an argument and assigm to matrix x
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
