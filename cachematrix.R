##This function will create a special matrix that can 
##cache inverse
##The function will be called CacheMatrix

#The function will make a matrix which do  the inverse cache
CacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
##This function is called cacheSolver
##The function will compute the function above
##CacheMatrix. If the inverse is completed, 
##the function will retrieve the inverse from the cache

#The CacheSolever will compute the inverse of the function
#returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    
    message("getting cached data.")
    
    return(inv)
    
  }
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  inv
}
