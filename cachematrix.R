## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # We set the inverse matrix of x to the NULL object
  set <- function(y) { # We define the function set() for this special matrix object. It will set the underlying matrix in this special matrix object and will set its inverse to NULL.
    x <<- y
    inv <<- NULL
  }
  get <- function() x # We define a function which returns the underlying matrix of this special matrix object
  setinverse <- function(inv_temp) inv <<- inv_temp # We define a function which sets the inverse matrix
  getinverse <- function() inv # We define a function which gets the inverse matrix
  list(set = set, get = get,  # This function makeCacheMatrix returns a list which the four functions defined above to get and set the underlying matrix and also its inverse.
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of the special matrix object 'x'.
## If the inverse of x is in the cache we return it, otherwise we compute it and set it in the cache of x before returning it.
cacheSolve <- function(x) {
  # we get the inverse matrix from the cache of the object x
  inv <- x$getinverse()
  
  # We test if the inverse matrix we got from the cache is not empty. If it is not empty we return it.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # In the case where the inverse matrix we got from the cache above is empty we execute the code below.
  data <- x$get() # We get the simple matrix object stored in x
  inv <- solve(data) # We compute the inverse of the underlying matrix data in x with the solve() function.
  x$setinverse(inv) # We set the inverse of the underlying matrix in x
  inv # We return the inverse matrix
}

## You just need to call the function below test_cachematrix() to test the code in this file.
## Here is an example where we create a special matrix object x and we retrieve his inverse many times using the cacheSolve function.
## We print the time that it takes to retrieve the inverse of this matrix at each call.
## As you can see, it takes a lot more time to get the inverse on the first call than on the subsequent calls as we get the inverse of this matrix directly from its cache after the first call.
test_cachematrix <- function() 
{  
  # We generate a random matrix of size 1000
  n<-1000
  mat<-matrix(runif(n^2),n)
  
  # We create a special matrix object
  mat_special<-makeCacheMatrix(mat)
  
  # We get the inverse of this special matrix 3 times in a row and we see how long it takes on each call to get the inverse.
  for (i in 1:3){
    print(paste("Call ", i, ":"))
    ptm <- proc.time() # we start the clock to see how long the next call will take
    inv_mat_special <- cacheSolve(mat_special)
    call_timing<-proc.time() - ptm  # we compute how long the previous call took
    print(call_timing)
  }
}
