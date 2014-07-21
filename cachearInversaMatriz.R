## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## NOTE: function's names and comments in spanish
## Genera una matriz y establece una serie de metodos para poder manipular sus datos fuera de esta misma función

crearMatriz <- function(x = matrix()) {
        inversa <- NULL
        establecer <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        obtener <- function() x
        establacerMultiplicacionMatriz <- function(solve) inversa <<- solve
        obtenerMultiplicacionMatriz <- function() inversa
        list(obtener = obtener, establecer = establecer,
             establacerMultiplicacionMatriz = establacerMultiplicacionMatriz,
             obtenerMultiplicacionMatriz = obtenerMultiplicacionMatriz)
}

## Write a short comment describing this function
## Recoge los datos de una matriz generada por la función anterior y calcula su inversa en caso de que no exista
## Si la inversa ya ha sido calculada previamente, recoge el valor almacenado en lugar de volver a calcularla

cachearMatriz <- function(x=matrix(), ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$obtenerMultiplicacionMatriz()
        if(!is.null(inversa)){
                message("getting cached data")
                return(inversa)
        }
        matriz <- x$obtener()
        inversa <- solve(matriz, ...)
        x$establecer(inversa)
        inversa
}