# This pair of functions will check to see if the inverse is stored in cache,
# and if not will calculate the inverse and set it to the cache.

# Generate a matrix that contains the following functions as a list:
# 1. set the values of the matrix
# 2. get the values of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(orig_matrix = matrix()) {
    
    inv_matrix <- NULL

    # Clear the cache and assign input to orig_matrix
    set_matrix <- function(y) {
        orig_matrix <<- y
        inv_matrix <<- NULL
    }
    
    # Return the original matrix
    get_matrix <- function() orig_matrix
    # Assign the solved inverse to inv_matrix, puts it in the cache
    set_inverse <- function(solution) inv_matrix <<- solution
    # Return the matrix inverse
    get_inverse <- function() inv_matrix
    
    # Construct a list that allow for the functions inside
    # makeCacheMatrix to be called
    list(
        set_matrix = set_matrix,
        get_matrix = get_matrix,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )

}


# Check to see if the inverse is in the cache. If TRUE, return from cache.
# If FALSE, calculate the inverse, put in cache, and return the solution.

cacheSolve <- function(invertible, ...) {
    # Check if the inverse is in the cache. Return value if NOT NULL.
    inv_matrix <- invertible$get_inverse()
    if(!is.null(inv_matrix)) {
        message("Getting inverse from cache")
        return(inv_matrix)
    }
    
    # Calculate the inverse, put in cache, and return the solution.
    orig_matrix <- invertible$get_matrix()
    inv_matrix <- solve(orig_matrix)
    invertible$set_inverse(inv_matrix)
    inv_matrix
}
