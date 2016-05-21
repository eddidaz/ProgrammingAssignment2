## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not 
#changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


# Function testing
# https://www.coursera.org/learn/r-programming/discussions/all/threads/hdpNLxwBEeaxVRJ-Fv2Eqw/replies/-xz_oxyiEeaNSw6v6KnGpw
# approach 1: create a matrix object, then use it as input to cacheSolve()
a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
cacheSolve(a)
# approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
#             note that the argument to cacheSolve() is a different object
#             than the argument to the first call of cacheSolve()
cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))

# call cacheSolve(a) a second time to trigger the "getting cached inverse" message
cacheSolve(a)

# try a non-invertible matrix
b <- makeCacheMatrix(matrix(c(0,0,0,0),2,2))
cacheSolve(b)

# illustrate getting the memory locations
a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
tracemem(a)
tracemem(matrix(c(-1, -2, 1, 1), 2,2))
