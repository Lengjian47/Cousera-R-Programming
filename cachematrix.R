##These two functions that are used to create a special object 
## that stores a numeric matrix and cache‘s its diagonal matrix.

#This function creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
  dia <- NULL
  set <- function(z){
    x <<- z
    dia <<- NULL
  }
  get <- function() x
  setdia <- function(diag) dia <<- diag
  getdia <- function() dia
  list(set = set,get = get,setdia = setdia,getdia=getdia)
}


# This function computes the diagonal of the special "matrix" created by 
# makeCacheMatrix above. If the diagonal has already been calculated (and the 
# matrix has not changed), then it should retrieve the diagonal from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  dia <- x$getdia()
  if(!is.null(dia)){
    message("getting cached data")
    return(dia)
  }
  dia_data <- x$get()
  dia <- diag(dia_data,...)
  x$setdia(dia)
  dia
}