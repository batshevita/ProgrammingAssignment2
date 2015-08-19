## I've created 2 functions, aimed to calculate inverse matrix avoiding time consuming for the
## case of a repeated matrix.
## The first function its a list of function that handle the saving and recalling
## the values of the matrix and its inverse matrix from the cache
## The second function calculates the inverse, either through actual calculation or recalling
## from the cache in teh case the data is available their

## The first function, `makeMatrix` creates a special "matrix", which is
## really a list containing functions to

## 1.  set the value of the matrix (matx)
## 2.  get the value of the matrix (matx)
## 3.  set the value of the inverse matrix (imatx)
## 4.  get the value of the inverse matrix (imatx)

makeCacheMatrix <- function(x = matrix()) {
  imatx <- NULL
  
  set <- function(y) {
    matx <<- y
    imatx <<- NULL
  }
  
  get <- function() matx
  
  setimatx <- function(inv_matx) imatx <<- inv_matx
  
  getimatx <- function() imatx
  list(set = set, get = get,
       setimatx = setimatx,
       getimatx = getimatx)
}

## Second function calculates inverse of a matrix, either getting it from the cache
## or if the value is not available in the cache through solve (assume inversible matrix)

cacheSolve <- function(x) {
  imatx <- x$getimatx()
  if(!is.null(imatx)) {
    message("getting cached data")
    return(imatx)
    }
  
  mydata <- x$get() ## load the value of the current matrix to local data var
  imatx <- solve(mydata) ## calculate inverse of data and save in imatx var
  x$setimatx(imatx) ## save in global variable the inverse of the matrix
  imatx
  }

