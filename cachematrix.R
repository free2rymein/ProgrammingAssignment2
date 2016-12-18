## First function to make the cache when the Solve is called first time on the matrix
## Second function checks if the inverse matrix already available in cache or else it solves the inverse of matrix

## Below function to Make cache function, very similar to Mean vector example and here we make list for Set, Get, Setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x<<-y
  inv_mat<<-NULL
  }
  get <- function() x
  setinv <- function(solve) inv_mat <<- solve
  getinv <- function() inv_mat
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function basically takes matrix and first check if the Inverse already available 
## if already available it will use that or else it will call Solve and store the inverse in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)){
    message("getting inverse matrix from cache")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data,...)
  x$setinv(inv_mat)
  inv_mat
}
