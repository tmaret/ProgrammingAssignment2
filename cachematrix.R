#' The functions 'makeCacheMatrix' and 'cacheSolve' allow to cache 
#' the inverse of matrices in a computation. The methods assume that
#' that the matrices supplied are always invertible.
#' 
#' Solution for https://github.com/rdpeng/ProgrammingAssignment2
#' @author Timothee Maret - timothee.maret@gmail.com

#' This function creates a special "matrix" object that 
#' can cache the matrix inverse.
#' 
#' @param m the original matrix to be cached (default is an empty matrix)
makeCacheMatrix <- function(m = matrix()) {

  # the state to hold the inverted matrix
  im <- NULL

  #' Set the original matrix
  #' 
  #' @param nm the new original matrix 
  set <- function(nom) { m <<- nom ; im <<- NULL }
  
  #' Get the original matrix
  #' 
  #' @return the original matrix
  get <- function() m

  #' Sets the inverted matrix.
  #' 
  #' @param inv the inverted matrix
  setInv <- function(inv) im <<- inv 

  #' Get the inverted matrix.
  #' 
  #' @return the inverted matrix or NULL is undefined
  getInv <- function() im
  
  list('get' = get, 'set' = set, 'setInv' = setInv, 'getInv' = getInv)
}

#' This function computes the inverse of the special "matrix" returned 
#' by the makeCacheMatrix function. 
#' If the inverse has already been calculated (and the matrix has not 
#' changed), then cacheSolve should retrieve the inverse from the cache.
#' 
#' @param m the matrix returned by makeCacheMatrix
#' @return the matrix inverse
cacheSolve <- function(m, ...) {
  inv <- m$getInv() 
  if (is.null(inv)) {
    message('computing the matrix inverse')
    inv <- solve(m$get(), ...)
    m$setInv(inv)
  } 
  inv
}
