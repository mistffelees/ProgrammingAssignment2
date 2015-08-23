##By using the lexical scoping, makeCacheMatrix, the first function, determines if the inverse matrix is
##cached. cacheSolve, the second function, calculate inverse matrix if cached matrix doesn't exist. If 
##cached matrix exists, it returns the cached inverse matrix. 


#makeCacheMatrix function creates a metrix object which chces it inverse. Within MakeCacheMatrix function:
  #set: set the value of the matrix
  #get: get the value of the matrix
  #seti: set the value of the inverse matrix
  #geti: get the value of the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  seti <- function(inv) im <<- inv
  geti <- function() im
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}


#If cached inverse matrix exists, the function below returns cached matrix with message "getting cached data".
#If cached inverse matrix doesn't exist, the function solves and return inverse matrix. 

#If cached inverse matrix exists, the function below returns cached matrix with message "getting cached data".
#If cached inverse matrix doesn't exist, the function solves and return inverse matrix. 
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  im <- x$geti()
  if(!is.null(im)) {
      message("getting cached data")
      return(im)
  }                 
  data <- x$get()
  im <- solve(data)
  x$seti(im)
  im
  }


#Function test
inputm <- matrix(1:4, nrow=2, ncol=2) #input matrix for testing.
cached <- makeCacheMatrix(inputm)

test1 <- cacheSolve(cached) #Because this is the first run, this should return calculated inverse matrix.
test1
test2 <- cacheSolve(cached) #This should return cached inverse matrix.
test2
