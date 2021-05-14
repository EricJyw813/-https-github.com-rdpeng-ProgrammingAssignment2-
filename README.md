# -https-github.com-rdpeng-ProgrammingAssignment2-
makeCacheMatrix <- function(x = matrix(sample(1:200,6),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#Test
source("ProgrammingAssignment2/cachematrix.R")
m <- matrix(rnorm(36),6,6)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
#
my_matrix <- makeCacheMatrix(matrix(1:10, 2, 2))
my_matrix$get()
#
my_matrix$getInverse()
#
cacheSolve(my_matrix)
#
my_matrix$getInverse()
#
my_matrix$set(matrix(c(3, 4, 5, 6), 2, 2))
my_matrix$get()
#
my_matrix$getInverse()
#
cacheSolve(my_matrix)
#
my_matrix$getInverse()
