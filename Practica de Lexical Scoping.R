makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
x<-c(2,3,4,5,6,7)

makeCacheMatrix  <-  function ( x  =  matrix ()) {
        
        inv  <-  NULL
        set  <-  function ( y ) {
                x  <-  y
                inv  <-  NULL
        }
        get <-  function () x
        setinverse  <-  function ( inverse ) inv  < -  inverse
        getinverse  <-  function () inv
        list ( set  =  set , get  =  get ,
                setinverse  =  setinverse ,
                getinverse  =  getinverse )
}
cacheSolve  <-  function ( x , ... ) {
        
        inv  <-  x $ getinverse ()
        if ( ! is.null ( inv )) {
                message ( " obteniendo la matriz en caché inversa " )
                return ( inv )
        }
        data  <-  x $ get ()
        inv  <- solve( data , ... )
        inv
}