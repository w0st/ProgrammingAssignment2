## This function create wrraped matrix object, which can cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # sets inverse matrix to NULL
  I <- NULL;
  # setter ang getter for matrix (x)
  set <- function(y) {
    x <<- y;
    # If matrix change (reassign), inverse matrix is unset (set to NULL)
    # to avoid incorrect value of inverse matrix
    I <<- NULL;
  }
  get <- function() x;
  # setter ang getter for inverse matrix (I)
  setInverse <- function(inv) {
    I <<- inv;
  }
  getInverse <- function() {
    I;
  }
  # list of functions - wrapped matrix object 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function return cache value of inverse of matrix inside wrapped object x 
## if cache contains inverse matrix in wrapped object
## Otherwise this function calculate inverse of matrix inside in wrapped object 'x',
## set inverse of 'x' in wrapped object by call setInverse and return this value

cacheSolve <- function(x, ...) {
  ## Return a matrix (I) that is the inverse of 'x'
  I <- x$getInverse();
  # if not assign reverse matrix
  if(!is.null(I)) {
    message("getting cached data");
    return(I);
  }
  data <- x$get();
  # assign I to inverse matrix of 'x'
  I <- solve(data, ...);
  x$setInverse(I);
  I;
}

## Examples of using
 mat$set(matrix(c(2,4,3,1), 2,2 )) # Set matrix to wrapped matrix object
 mat$get() # get defined matrix
 cacheSolve(mat) # get inverse matrix
 cacheSolve(mat) # get inverse matrix (now from cache)
 mat$set(matrix(c(2,1,3,5), 2,2)) # Set new matrix to wrapped matrix object
 cacheSolve(mat) # get inverse matrix (again calculate inverse matrix, not fetch from cache previous matrix)
