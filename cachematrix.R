## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" in which the matrix can be:
##    set - assign a value
##    get - get the value
##    setinverse - set the inverse of the matrix
##    getinverse - get the inverse of the matrix (retrieve if cached)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Compute the inverse of the matrix x if not already computed
## and x has not changed
cacheSolve <- function(x, ...) {
  # try to get the inverse from cache, if so return it
  ## QQQQ - how know if matrix has changed??
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message('returning cached inverse')
    return(inv)
  }
  # if not cached, calculate it from x
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  inv   # return the inverse which has already been set in x
}

## test the functions above
testmeplease <- function() {
  ## test functionality - create cached matrix, see
  y <- matrix(rnorm(9), 3, 3)
  a <- makeCacheMatrix(y)
  print("y = ")
  print(y)

  print("a = ")
  print(a$get())
  print("inv(a) = ")
  print(a$getinverse())
  
  print("inv(y) = ")
  print(solve(y))

  cacheSolve(a)
  print("inv(a) = (after cacheSolve(a)), should match inv(y) above")
  print(a$getinverse())

  print("changing a...")
  a$set(matrix(rnorm(100), 10, 10))
  print("check a$getinverse() ... should be NULL")
  print(a$getinverse())

  cacheSolve(a)
  print("check a$getinverse() ... should be correct now (10x10 matrix)")
  print(a$getinverse())
}
