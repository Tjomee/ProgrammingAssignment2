# Programming Assignment 2 - Lexical Scoping

# These two functions can help in caching the inverse of a matrix.

# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly . These two functions are used to cache the inverse of a matrix.

# The first function, makeCacheMatrix creates a list containing a function to
# 1.set the value of the vector
# 2.get the value of the vector
# 3.set the value of the mean
# 4.get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv<<- solve(x)
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

# The second function, cacheSolve, returns the inverse of the matrix. It first checks
# to see if the inverse has already been calculated. If so, it gets the
# result and skips the computation. Otherwise, it calculates the inverse, 
# and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv 
}

# Example

# x<-matrix(3:6,nrow=2,ncol = 2)
# s<-makeCacheMatrix(x)
# s$get()
# cacheSolve(s)

#> s$get()
# We now have 2x2 matrice
#      [,1] [,2]
#[1,]    3    5
#[2,]    4    6

#> cacheSolve(s)
#The determinant od the matrice is -2  
# So, the inverse of the matrice is... 

#      [,1] [,2]
#[1,]   -3  2.5
#[2,]    2 -1.5

#Retrieving from the cache in the second run
# >cacheSolve(s)
# getting cached data
#       [,1] [,2]
# [1,]   -3  2.5
# [2,]    2 -1.5
 