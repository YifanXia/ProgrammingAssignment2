## Put comments here that give an overall description of what your
## functions do

# The two functions below compute the inverse of a give matrix x,
# then save it in the cache for future use.

## Write a short comment describing this function
# The function *makeCacheMatrix* creats a special matrix, which is a list containing functions to :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

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


## Write a short comment describing this function
# The function *cacheSolve* returns the inverse of the matrix x.
# If the inverse is previously computed and saved in the cache, the function returns the cached value.
# If it is not yet computed, the function computes the inverse using *solve*, then saves it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}



