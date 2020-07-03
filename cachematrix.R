## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" which is really a list containing
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## It calculates the inverse of the special matrix but it first checks
## to see if the inverse has already been calculated. 

cachesolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


#Example to excute the  program

mat=matrix(c(1:4),nrow= 2 ,ncol= 2 , byrow=TRUE)

makeMatrix_value= makeMatrix(mat)
makeMatrix_value$get()
makeMatrix_value$getinv()

cachesolve_value = cachesolve (makeMatrix_value)
cachesolve_value