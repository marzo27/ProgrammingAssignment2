#=======================================================================
#cachematrix.R
#Curso: Programacion en R
#Objetivo: Almacenamiento en cache de la inversa de una matriz
#Por: Alvaro Gutierrez		Fecha: 2015-06-14
#=======================================================================

# Indicar la carpeta por default
setwd("C:/Users/Alvaro/ProgrammingAssignment2")

#=======================================================================
#makeCacheMatrix Crea una matriz que puede ser usada para almacenamiento en cache
#=======================================================================
makeCacheMatrix <- function(x = matrix()) 
{
	#Almacena el valor del cache inicialmente a NULL
	cache <- NULL
	set <- function(y) 
	{
		x <<- y
		cache <<- NULL
	}
	#Obtiene el valor de la matriz
	get <- function() x
	setMatrix <- function(inverse) cache <<- inverse
	getInverse <- function() cache
	#Retorna la funcion creada al environment de trabajo
	list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}

#=======================================================================
#cacheSolve Obtiene o pone una matriz para almacenamiento en cache
#=======================================================================
cacheSolve <- function(x, ...) 
{
	#Trata de obtener la inversa
	cache <- x$getInverse()
	if (!is.null(cache)) {
		message("getting cached data")
		return(cache)
	}
	#Crea la matriz
	matrix <- x$get()
	#Asegurarse de que la matriz sea cuadrada e invertible
	tryCatch( {
		cache <- solve(matrix, ...)
	},
	error = function(e) {
		message("Error:")
		message(e)
		return(NA)
	},
	warning = function(e) {
		message("Cuidado:")
		message(e)
		return(NA)
	},
	finally = {
		x$setMatrix(cache)
	} )
	#Desplegar matriz
	return (cache)
}
