## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inversemtrx <- NULL;
    set <- function (y = matrix()){
        x <<- y;
        inversemtrx <<- NULL;
    }
    get <- function () x;
    setinverse <- function (solve) inversemtrx <<- solve;
    getinverse <- function () inversemtrx;
    list (set = set, get = get,
          setinverse = setinverse, 
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
    inversemtrx <- x$getinverse();
    if (!is.null(inversemtrx)){
        message ("getting cache data");
        return (inversemtrx);
    }
    data <- x$get();
    inversemtrx <- solve(data,...);
    x$setinverse (inversemtrx);
    inversemtrx;
}
