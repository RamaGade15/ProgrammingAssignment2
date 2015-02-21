## Purpose of these functions is to cache the inverse of a given matrix to avoid 
## repeated calculation of inverse and optimize run time if it is required to use
## the same value multiple times
###########################################
## makeCacheMatrix  
###########################################  
## 
## defines the following 4 methods
##
## set        - to set the value of matrix - in x
## get        - get the value of matrix set above - from x
## setinverse - save the inverse calculated in cache - in m
## getinverse - get the inverse value from cache - from m
##
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

###########################################
## cacheSolve 
###########################################  
#
# This function returns the inverse. 
# 
# if the cached value is null, calculates and saves in cache
# 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

####################
## Testing
####################
##> 
## 1. Create Matrix Cache 
##
#> test <- makeCacheMatrix()
##> 
## 2. test data
##
#> a <- matrix(c(2,4,1,1,8,6,22,3,5),3,3)
##
##  3. Set the value of matrix
##
#> test$set(a)
##>
## 4. Find Inverse
#
#> cacheSolve(test)
##[,1]        [,2]        [,3]
##[1,]  0.05804749  0.33509235 -0.45646438
##[2,] -0.04485488 -0.03166227  0.21635884
##[3,]  0.04221636 -0.02902375  0.03166227
##
## 5. When called second time, value is fectched from cache
##
#> cacheSolve(test)
##getting cached data
##[,1]        [,2]        [,3]
##[1,]  0.05804749  0.33509235 -0.45646438
##[2,] -0.04485488 -0.03166227  0.21635884
##[3,]  0.04221636 -0.02902375  0.03166227
##> 
