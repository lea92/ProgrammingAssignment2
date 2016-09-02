#' A cache matrix
#' 
#' This function `makeCacheMatrix` creates an special 
#' matrix  which allows to keep in memory the inverse of its matrix 
#' @return A list of function :
#' \item{set}{set the value of the matrix}
#' \item{get}{get the value of the matrix}
#' \item{setInverse}{set the value of the inverse matrix}
#' \item{getInverse}{get the value of the inverse matrix}
#' @param x the matrix
#' @rdname makeCacheMatrix
#' @export
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#' cache the inverse matrix 
#'
#' cacheSolve computes the inverse matrix of makeCacheMatrix
#' if the inverse has already computed once time cacheSolve 
#' return this value which is kept in memory 
#' else it computes it and keep the new value in memory
#' @param x the matrix
#' @return the inverse of the matrix x
#' @examples
#' a = matrix(runif(10, 5.0, 7.5),2,2)
#' specialA = makeCacheMatrix(a)
#' cacheSolve(specialA) #first time, it computes the inverse
#' cacheSolve(specialA) # second time, it gives the value given in-memory 
#' @rdname cacheSolve
#' @export
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting inverse of matrix ")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
