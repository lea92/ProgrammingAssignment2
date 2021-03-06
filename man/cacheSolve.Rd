% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cachematrix.R
\name{cacheSolve}
\alias{cacheSolve}
\title{cache the inverse matrix}
\usage{
cacheSolve(x, ...)
}
\arguments{
\item{x}{the matrix}
}
\value{
the inverse of the matrix x
}
\description{
cacheSolve computes the inverse matrix of makeCacheMatrix
if the inverse has already computed once time cacheSolve 
return this value which is kept in memory 
else it computes it and keep the new value in memory
}
\examples{
a = matrix(runif(10, 5.0, 7.5),2,2)
specialA = makeCacheMatrix(a)
cacheSolve(specialA) #first time, it computes the inverse
cacheSolve(specialA) # second time, it gives the value given in-memory 
}

