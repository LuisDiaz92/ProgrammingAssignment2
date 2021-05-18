## My functions first create a matrix object that can cache its inverse and then compute the inverse of this matrix


## I create a function that creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## I compute the inverse of the special “matrix” returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Testing
B <- matrix(c(2,4,7,9),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
