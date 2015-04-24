## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function named ``makeCacheMatrix'' returns a special
## structured list containing a series of functions to work 
## as follows,
## 1. set: Set the matrix
## 2. get: Get the matrix
## 3. setinv: Set the inverse matrix of the original matrix
## 4. getinv: Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  # m_inv: the stored inversed matrix
  m_inv <- NULL
  
  set <- function(y){
    x <<- y
    m_inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(m_inverse) m_inv<<-m_inverse
  
  getinv <- function() m_inv
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## The function ``cacheSolve'' takes the special structured 
## matrix returned defined by the function ``makeCacheMatrix''
## as its input, calculating the inversed matrix of the input
## matrix. This function first checks if there is already a
## calculated and stored inversed matrix, and gets that reversed
## matrix directly, if so. Otherwise, the inversed matrix will
## be calculated and saved into the ``cache'' variable m_inv, by 
## calling the ``setinv'' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinv()
  if(!is.null(m_inv)){
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
}
