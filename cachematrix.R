## This function creates a special "matrix" object that can cache its inverse.
## With the getters and setters the matrix will be inversed.

        makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) inv <<- solve
                getsolve <- function() inv
                list(set = set, get = get,
                        setsolve = setsolve,
                        getsolve = getsolve)
        }

## This function computes the inverse of the special matrix returned by 
## `makeCacheMatrix` above. 

        cacheSolve <- function(x, ...) {
                inv <- x$getsolve()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setsolve(inv)
                inv
        }
## Return a matrix that is the inverse of 'x'
## This is an example that the code is working. 
        my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
        my_matrix$get()
                #[,1] [,2]
                #[1,]    1    3
                #[2,]    2    4
        my_matrix$getsolve()
                # NULL
        cacheSolve(my_matrix)
                #[,1] [,2]
                #[1,]   -2  1.5
                #[2,]    1 -0.5
        my_matrix$getsolve()
                #[,1] [,2]
                #[1,]   -2  1.5
                #[2,]    1 -0.5
