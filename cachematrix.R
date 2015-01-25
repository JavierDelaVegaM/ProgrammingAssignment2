## Javier De la Vega Magaña
makeCacheMatrix = function(x = matrix()) {
   m = NULL
   set = function(y) {
      x <<- y
      m <<- NULL
   }
   get = function() x
   setmatrix = function(matrix) m <<- matrix
   getmatrix = function() m
   list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

cacheSolve = function(x, ...) {
   m = x$getmatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data = x$get()
   m = try(solve(data, ...), silent = TRUE)
   if(is.matrix(m)){
      x$setmatrix(m)
      m
   }else
      print("No se puede obtener matriz inversa, proporcione otra matriz")
}
