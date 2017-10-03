# Just a helper function for testing turned(x)
do_work <- function(n){
	mat<-getmatrix(n)
	mat<-turned(mat)
	return(mat)
}

# Takes n and returns a nxn matrix filled with values 1:(n*n)
# Just a helper function for testing turned(x)
getmatrix <- function(n){
	v <- c(1:(n*n))
	mat <- matrix(v, ncol=n, nrow=n)
	return(mat)
}

# I don't know if R's transpose function uses any extra space so I am including
# a version that I know does not use any extra space
# Note that this will only work for square matrices
transpose_square <- function(x){
	for(i in 1:(nrow(x) - 1)){
		for(j in (i + 1):(ncol(x))){
			x[i, j] <- bitwXor(x[i, j], x[j, i])
			x[j, i] <- bitwXor(x[j, i], x[i, j])
			x[i, j] <- bitwXor(x[i, j], x[j, i])
		}
	}
	return(x)
}

# Rotate the matrix 90 degrees anti clockwise by first reversing the elements
# in each row, then transposing the matrix
# This implementation uses no extra space at all.
turned <- function(x){
	if(!is.matrix(x)){
		warning("Expected a matrix")
		return(NA)
	}
	if(ncol(x) != nrow(x)){
		warning("Expected a square matrix")
		return(NA)
	}
	# Reverse the elements in each row
	for(i in 1:nrow(x)){
		for(j in 1:(ncol(x) / 2)){
			x[i, j] <- bitwXor(x[i, j], x[i, nrow(x) - (j - 1)])
			x[i, nrow(x) - (j - 1)] <- bitwXor(x[i, nrow(x) - (j - 1)], x[i, j])
			x[i, j] <- bitwXor(x[i, j], x[i, nrow(x) - (j - 1)])
		}
	}
	x <- transpose_square(x)
	return(x)
}
