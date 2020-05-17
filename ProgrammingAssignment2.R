makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}
cacheSolve <- function(x, ...) { k <- x$getInverse()
if(!is.null(k)){
  message("getting cached data")
  return(k)
}
mat <- x$get()
k <- solve(mat,...)
x$setInverse(k)
k
}
