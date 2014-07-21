## fun 1 . makeCacheMatrix 
## fun 2 . cacheSolve
## fun 1 create a matrix holder and return this matrix
## fun 2 computes the inverse of the matrix




makeCacheMatrix <- function(mat_x = matrix()) 
{
    invx <- NULL
    set <- function(y) 
    {
        mat_x <<- y
        invx <<- NULL
    }
    get <- function() mat_x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


cacheSolve <- function(mat_x, ...) 
{
    invmat <- mat_x$getinverse()
    if (!is.null(invmat))
    {
        return(invmat)
    }
    data <- mat_x$get()
    invmat <- solve(data, ...)
    mat_x$setinverse(invmat)
    invmat
}
