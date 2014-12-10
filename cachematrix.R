## cachematrix.R
## Adapted from MakeVector() and cachemean() examples for coursera rprog-016 Assignment 2
## https://github.com/rdpeng/ProgrammingAssignment2

## This script contains functions to create and access cached solutions for the inverse of a matrix
## A computed inverse should be cahced so that future attempts to get the same solution are pulled from the cache instead of being recomputed

## makeCacheMatrix
## Constructs the cache
## provides get and set functions for the inverse solution to a matrix

makeCacheMatrix <- function(myMatrix = matrix()) {
  s <- NULL
  set <- function(y) {
    myMatrix <<- y
    s <<- NULL
  }
  get <- function() myMatrix #get function for the matrix
  setsolve <- function(inverse) s <<- inverse #add solution to cache
  getsolve <- function() s #return cached solution
  
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Attempts to provide the inverse solution to a matrix by calling functions from MakeCacheMatrix
## A solution that has been cached should return with the message "getting cached data"

cacheSolve <- function(myMatrix, ...) {
  ## First, check for a cached solution
  s <- myMatrix$getsolve() 
  if(!is.null(s)) {
    message("getting cached data")
    return(s) #return the cached solution
  }
  
  ## no cached solution was found, solve and cache
  data <- myMatrix$get() 
  s <- solve(data, ...) # solve inverse matrix
  myMatrix$setsolve(s) # cache the solution
  s
}


## Example Run
# > myMatrix <- matrix(rnorm(25), nrow = 5)

# > c <- makeCacheMatrix(myMatrix)

# > c$get()
#[,1]       [,2]       [,3]       [,4]        [,5]
#[1,] -0.6962177 -0.2382861 -0.5177081 -1.4257784 -1.07123458
#[2,] -1.2860317 -0.8874988 -0.4161068  0.7797153  1.60963721
#[3,]  1.6300569  0.5742786 -2.0334527  0.4245825 -0.09869857
#[4,]  0.1898971  0.4297909  0.5564600  0.6863487  1.68175275
#[5,] -3.2373575  0.5856037  0.6185908  0.5145096  0.28762546

# > cacheSolve(c)
#[,1]        [,2]         [,3]        [,4]        [,5]
#[1,] -0.1132455 -0.10182725 -0.006116131  0.06657200 -0.24326360
#[2,]  0.2160837 -0.49193535  0.303271559  0.56898025  0.33502655
#[3,] -0.2369628 -0.21308528 -0.395420822  0.04025709 -0.06113059
#[4,] -0.8987557  0.07678677  0.079743414 -0.66929166  0.16367337
#[5,]  0.4027669  0.17638560  0.021478866  0.70151907 -0.10472205

# > cacheSolve(c)
#getting cached data
#[,1]        [,2]         [,3]        [,4]        [,5]
#[1,] -0.1132455 -0.10182725 -0.006116131  0.06657200 -0.24326360
#[2,]  0.2160837 -0.49193535  0.303271559  0.56898025  0.33502655
#[3,] -0.2369628 -0.21308528 -0.395420822  0.04025709 -0.06113059
#[4,] -0.8987557  0.07678677  0.079743414 -0.66929166  0.16367337
#[5,]  0.4027669  0.17638560  0.021478866  0.70151907 -0.10472205



## Example Cache Speed-up
# > m <- matrix(sample.int(100,size=1000000,replace=TRUE), nrow=1000)
# > d <- makeCacheMatrix(m)
# > system.time(cacheSolve(d))
# user  system elapsed 
# 1.144   0.000   1.116 
# > system.time(cacheSolve(d))
# getting cached data
# user  system elapsed 
# 0       0       0 