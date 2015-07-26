
## It does use the '<<-' assignment operator to illustrate scope
## --------------------------------------------------------------
## Variables created in a function are extinguished on exiting the function -
## they have local scope only; but here we push a matrix into the parent environment
## I've changed the variable and parameter names trying to make meaning clearer
## Matrix is assumed ALWAYS INVERTIBLE!!
##
## Testing script:
## m6 <- matrix(rnorm(36,0,1),6,6)
## m6f <- makeCacheMatrix(m6)
## m6f$getinverse()  ... s/be NULL
## cacheSolve(m6f)
## cacheSolve(m6f)
## cacheSolve(m6f)
## m6f$getinverse()
## m7 <- matrix(rnorm(49),7,7)
## m6f$set(m7)       ## set a new matrix
## m6f$get           ## 7x7 matrix
## m6f$getinverse()  ## should be NULL again
## cacheSolve(m6f)   ## recalculates
## cacheSolve(m6f)   ## returns cache value
makeCacheMatrix <- function(inputmat = matrix())
   {
   inverse.matrix <- NULL        ## define and set the inverse cached copy
   original.matrix <- inputmat   ## save original for comparison
   set <- function(input.matrix)
      {
         ## save a copy in makeCacheMatrix env ( since this is a child function)
         ## and NULL the cached inverse
         original.matrix <<- input.matrix
         inverse.matrix <<- NULL
      }
   get <- function() original.matrix
   ## is there some way to make setinverse invisible outside the script?
   setinverse <- function(inverse) inverse.matrix <<- inverse
   getinverse <- function() inverse.matrix
   status <- function() result <- c("original", (!is.null(original.matrix)), "inverted", (!is.null(inverse.matrix)))
   ## return value is the list object of the four child functions set,get,setinverse,getinverse
   list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse,
        status = status)
}

## Check the matrix is 1. Cached, 2. Unchanged
## and return cached or calculate inverse; note that the input 'inputmat' is a function object
## actually needs the current matrix as input to check changes
cacheSolve <- function(inputmat = NULL) {
   ## Return a matrix that is the inverse of 'inputmat (original.matrix)'
   if (missing(inputmat)) return (NULL)
   inverted <- inputmat$getinverse()
   original <- inputmat$get()
   if (is.null(original)) return (NULL)

   if(!is.null(inverted)) {
      if (!identical(original,original))
      {
         ## we don't have the current matrix to compare with!!! so this cannot be executed
         message("matrix changed - recalculating")
      }
      else
      {
         message("getting cached data")
         return(inverted)
      }
   }
   inverted <- solve(original)
   inputmat$setinverse(inverted)
   inverted
}
